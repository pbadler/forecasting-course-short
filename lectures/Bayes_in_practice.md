---
title: "Bayes in practice"
output:
  html_document: default
layout: post
mathjax: true
---

En esta clase vamos a ver cómo ajustar modelos Bayesianos usando `JAGS`, `Stan` y `brms`

Volvamos al ejemplo de los búfalos en Yellowstone. Anteriormente vimos como ajustar un modelo del tipo `lm(logN ~ loglagN + ppt_Jan, data=train )`. Para ajustar ese modelo en `JAGS` tenemos que hacer varias cosas...

Primero, tenemos que escribir el modelo en lenguaje `BUGS` y guardarlo en el directorio de trabajo. Una forma de hacer eso diréctamente desde `R` es usando la función `cat`:

```R
cat(file = "bison.bug",
  "
  model {
  
    # Función de likelihood
    for( i in 2:n ){
      logN[i] ~ dnorm (mu[i], tau)
      loglagN[i] <- logN[i-1]
      mu[i] <- b0 + b_lag * loglagN[i] + b_ppt * ppt_Jan[i]
    }

    # Previas
    b0 ~ dnorm(0, 0.1) 
    b_lag ~ dnorm(0, 1)
    b_ppt ~ dnorm(0, 1)
    tau <- 1/(s*s) 
    s ~ dexp(1)
  }
")

```

Ahora cargamos los datos y los acomodamos un poco, además de preparar los datos como hicimos antes, vamos a generar algunos data.frames extras (ver bb_wide y btrain):

```R
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(mvtnorm)

# Set up bison data ---------------------------------------------------
bison = read.csv("https://raw.githubusercontent.com/pbadler/forecasting-course-short/master/data/YNP_bison_counts.csv", stringsAsFactors = FALSE)
bison = select(bison,c(year,count.mean)) # drop non-essential columns
names(bison) = c("year","N") # rename columns
bbison = bison # to use with JAGS

# # the next few lines set up a lag N variable
tmp = bison
tmp$year = tmp$year + 1
names(tmp)[2] = "lagN"
bison = full_join(bison,tmp)
bison = filter(bison,year > min(year) & year < max(year))  # drop incomplete observations
rm(tmp)

# add log transformed variables
bison = mutate(bison, logN = log(N))
bison = mutate(bison, loglagN = log(lagN))
# Set up weather data ----------------------------------------------------
weather = read.csv("https://raw.githubusercontent.com/pbadler/forecasting-course-short/master/data/YNP_prism.csv", stringsAsFactors = FALSE)
weather = weather %>% separate(Date,c("year","month"), sep = '-')
weather$year = as.numeric(weather$year)
weather$month = as.numeric(weather$month)
weather$clim_year = ifelse(weather$month < 9, weather$year, weather$year + 1)
weather$clim_month = ifelse(weather$month < 9, weather$month + 4, weather$month - 8)
head(weather)

# To prepare for a merge with the bison data, we need to arrange months horizontally
# rather than vertically. Here is how to do it for the precip data:
precip_wide = weather %>% 
  select(c(clim_year,clim_month,ppt_in)) %>%  # drop all the other climate variables
  spread(clim_month,ppt_in) # 'spread' is where the magic happens

# rename months (optional, but I think it makes the data frame easier to understand)
names(precip_wide)[2:13] = paste0("ppt_",c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug"))
head(precip_wide)

# aggregate by season
precip_wide = mutate(precip_wide, ppt_Fall = rowSums(precip_wide[,c("ppt_Sep","ppt_Oct","ppt_Nov")]))
precip_wide = mutate(precip_wide, ppt_Win = rowSums(precip_wide[,c("ppt_Dec","ppt_Jan","ppt_Feb")]))
precip_wide = mutate(precip_wide, ppt_Spr = rowSums(precip_wide[,c("ppt_Mar","ppt_Apr","ppt_May")]))
precip_wide = mutate(precip_wide, ppt_Sum = rowSums(precip_wide[,c("ppt_Jun","ppt_Jul","ppt_Aug")]))
head(precip_wide)

# merge with bison
bison_wide = left_join(bison,precip_wide,by=c("year" = "clim_year"))
bb_wide = left_join(bbison,precip_wide,by=c("year" = "clim_year"))

train = subset(bison_wide, year < 2012)
test = subset(bison_wide, year >= 2012)
btrain = subset(bb_wide, year < 2012)

```

Ahora armamos una lista con los datos que vamos a pasarle a `JAGS`, un vector con los nombres de los parámetros que queremos guardar:

```R
datos <- list(n = dim(btrain)[1],
              logN = log(btrain$N),
              ppt_Jan = btrain$ppt_Jan)

params <- c("b0", "b_lag", "b_ppt", "s")

```

Además, queremos que las cadenas Markovianas comiencen en distintos valores. Para esto escribimos una pequeña función para generar valores iniciales de las cadenas (recuerden que esto no tiene nada que ver con las previas)

```
inits <- function() list(b0 = runif(1, 0, 1), 
                         b_lag = runif(1, 0, 1), 
                         b_ppt = runif(1, 0, 1),
                         s = runif(1, 0, 10))
                         
```

Ahora podemos llamar a `JAGS` para ajustar el modelo

```R
library(jagsUI)

bison.sim <- jags(data = datos, 
                  inits = inits, 
                  parameters.to.save = params, 
                  model.file = "bison.bug", 
                  n.chains = 3, 
                  n.iter = 1000, 
                  n.burnin = 500, 
                  n.thin = 1)
                  
```
Para ver los resultados:

```R

print(bison.sim)

plot(bison.sim)

```
Si queremos hacer predicciones, tenemos que tener en cuenta la incertidumbre en los valores de los parámetros. Esa incertidumbre está representada por la posterior conjunta de nuestro modelo. Veamos como hacer una predicción de Monte Carlo a partir de esta posterior conjunta:

```R
nsim = 1000      # Ensemble size
tot_time = dim(test)[1] + 1
nClim = matrix(NA, nsim, tot_time)   # storage for all simulations
init_obs = test$loglagN[test$year==2012]
nClim[,1] = rnorm(nsim, init_obs, 0)

for(i in 1: nsim){
  idx <- sample.int(bison.sim$mcmc.info$n.samples, 1)
  for(j in 2: tot_time){
    best_guess = bison.sim$sims.list$b0[idx] + 
      bison.sim$sims.list$b_lag[idx] * nClim[i,j-1] + 
      bison.sim$sims.list$b_ppt[idx] * test$ppt_Jan[test$year == 2010 + j]
    nClim[i,j] = rnorm(1, best_guess, bison.sim$sims.list$s[idx]) 
  }
}


# calculate 95% credible intervals the median and 95% CI limits for each time 
library(coda)

CI <- HPDinterval(as.mcmc(nClim))
mp <- colMeans(nClim)
# plot the data
plot(c(train$year, test$year), c(train$logN, test$logN), ylim = c(6, 10), type = "l", xlab = "year", ylab = "log N")
# add predictions, upper and lower CI's
lines(test$year, mp[2: ncol(nClim)], col = "red", lty = "solid")
lines(test$year, CI[2: ncol(nClim), 1], col = "red", lty = "dashed")
lines(test$year, CI[2: ncol(nClim), 2], col = "red", lty = "dashed")

accuracy(mp[2: tot_time], test$logN)

```

Un aspecto bastante útil de esta formulación es que podemos usarla directamente para hacer predicciones. Esto es porque cuando escribimos algo como `logN[i] ~ dnorm(mu[i], tau)` estamos diciendo que los valores de `logN` son muestreados de una distribución normal. Cuando `JAGS` encuentra valores en `logN[i]`, va a usar esos valores para actualizar las cadenas Markovianas de los parmámetros de esa distribución normal. Si en vez de encontrar valores encuentra `NA` (valores perdidos), entonces genera una muestra de la normal en base a los valores de los parámetros en las cadenas Markovianas. Veamos cómo usar esta característica para predecir el futuro:

```R
bison_wNA <- bb_wide
bison_wNA$N[bb_wide$year >= 2012] <- NA

datos <- list(n = dim(bison_wNA)[1],
              logN = log(bison_wNA$N),
              ppt_Jan = bison_wNA$ppt_Jan)

params <- c("b0", "b_lag", "b_ppt", "s", "logN")

bp.sim <- jags(data = datos, 
               inits = inits, 
               parameters.to.save = params, 
               model.file = "bison.bug", 
               n.chains = 3, 
               n.iter = 1000, 
               n.burnin = 500, 
               n.thin = 1)

print(bp.sim)

plot(bb_wide$year, log(bb_wide$N), 
     ylim=c(6,10), type ="l", 
     xlab= "year", ylab = "log N")

lines(bison_wNA$year[43:48], bp.sim$mean$logN[43:48], 
      col = "red")
lines(bison_wNA$year[43:48], bp.sim$q2.5$logN[43:48], 
      col = "red", lty = "dashed")
lines(bison_wNA$year[43:48], bp.sim$q97.5$logN[43:48], 
      col = "red", lty = "dashed")

accuracy( as.numeric( bp.sim$mean$logN[43:48] ), test$logN)

```

Veamos como ajustar este mismo modelo pero usando `Stan`. Primero tenemos que definir el modelo usando el lenguaje de `Stan`:

```R
cat(file = "bison.stan", 
    "
data { 
  int<lower=1> n;
  vector[n] logN;
  vector[n] ppt_Jan;
}

parameters {
  real b0;
  real b_lag;
  real b_ppt;
  real<lower=0> sigma;
}

model{
  vector[n] mu;
  vector[n] loglagN;
  b0 ~ normal(0,10);
  b_lag ~ normal(0,1);
  b_ppt ~ normal(0,1);
  sigma ~ exponential(1);
  
  for(i in 2: n){
    loglagN[i] = logN[i-1];
    mu[i] = b0 + b_lag * loglagN[i] + b_ppt * ppt_Jan[i];
  }
  
  logN[2: n] ~ normal(mu[2: n], sigma);
}
"
)

```

Ahora preparamos los datos y llamamos a `stan`

```R

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

bison_dat <- list(n = dim(btrain)[1],
                logN = log(btrain$N),
                ppt_Jan = btrain$ppt_Jan)

fit <- stan(file = 'bison.stan', data = bison_dat,
            iter = 1000, thin = 1, chains = 3)

print(fit)

```

Para hacer predicciones a partir de la posterior conjunta de este modelo, podemos extraer las muestras de las posteriores y luego hacer la simulación de Monte Carlo como de costumbre

```R

pos <- rstan::extract(fit, pars = c("b0", "b_lag", "b_ppt", "sigma"))

nsim = 1000      # Ensemble size
tot_time = dim(test)[1] + 1
nClim = matrix(NA, nsim, tot_time)   # storage for all simulations
init_obs = test$loglagN[test$year == 2012]
nClim[,1] = rnorm(nsim, init_obs, 0)

for(i in 1: nsim){
  idx <- sample.int(dim(pos$b0), 1)
  for(j in 2: tot_time){
    best_guess = pos$b0[idx] + 
      pos$b_lag[idx] * nClim[i,j-1] + 
      pos$b_ppt[idx] * test$ppt_Jan[test$year == 2010 + j]
    nClim[i,j] = rnorm(1, best_guess, pos$sigma[idx]) 
  }
}


# calculate 95% credible intervals the median and 95% CI limits for each time 
library(coda)

CI <- HPDinterval(as.mcmc(nClim))
mp <- colMeans(nClim)
# plot the data
plot(c(train$year, test$year), c(train$logN, test$logN), ylim = c(6, 10), type = "l", xlab = "year", ylab = "log N")
# add predictions, upper and lower CI's
lines(test$year, mp[2: ncol(nClim)], col = "red", lty = "solid")
lines(test$year, CI[2: ncol(nClim), 1], col = "red", lty = "dashed")
lines(test$year, CI[2: ncol(nClim), 2], col = "red", lty = "dashed")

accuracy(mp[2: tot_time], test$logN)

```

También podemos hacer las predicciones usando el bloque de "generated quantities" en el modelo de `Stan` 

```R

cat(file = "bison1.stan", 
    "
data { 
  int<lower=1> n_train;
  int<lower=1> n_test;
  vector[n_train] logN;
  vector[n_train + n_test] ppt_Jan;
}

parameters {
  real b0;
  real b_lag;
  real b_ppt;
  real<lower=0> sigma;
}

model{
  vector[n_train] mu;
  vector[n_train] loglagN;
  b0 ~ normal(0,10);
  b_lag ~ normal(0,1);
  b_ppt ~ normal(0,1);
  sigma ~ exponential(1);
  
  for(i in 2: n_train){
    loglagN[i] = logN[i-1];
    mu[i] = b0 + b_lag * loglagN[i] + b_ppt * ppt_Jan[i];
  }
  
  logN[2: n_train] ~ normal(mu[2: n_train], sigma);
}

generated quantities {
  vector[n_test] logNpred;
  vector[n_test + 1] llagN;
  llagN[1] = logN[n_train];
  for(t in 1:n_test){
    logNpred[t] = normal_rng(b0 + b_lag * llagN[t] + b_ppt * ppt_Jan[n_train + t], sigma);
    llagN[t+1] = logNpred[t];
  }
}

"
)

bison1_dat <- list(n_train = dim(btrain)[1],
                   n_test = dim(test)[1],
                  logN = log(btrain$N),
                  ppt_Jan = bison_wNA$ppt_Jan)

fit <- stan(file = 'bison1.stan', data = bison1_dat,
            iter = 1000, thin = 1, chains = 3)
            
print(fit)

fit_summary <- summary(fit)$summary
logNpred <- fit_summary[grepl("logNpred", rownames(fit_summary)),]

plot(bb_wide$year, log(bb_wide$N), 
     ylim=c(6,10), type ="l", 
     xlab= "year", ylab = "log N")
lines(test$year, logNpred[, 1], 
      col = "red")
lines(test$year, logNpred[, 4], 
      col = "red", lty = "dashed")
lines(test$year, logNpred[, 8], 
      col = "red", lty = "dashed")

accuracy( as.numeric( logNpred[,1] ), test$logN)

```

Para modelos de tipo "regresión", podemos usar el paquete `brms` de [Paul Bürkner](https://paul-buerkner.github.io/brms/). `brms` es una interfase entre `R` y `Stan`, podemos escribir modelos con las fórmulas de `R` para modelos lineales y `brms` escriber por nosotros el modelo de `Stan`

```R

library(brms)

# mb <- brm(logN ~ loglagN + ppt_Jan, data = train)
# make_stancode(logN ~ loglagN + ppt_Jan, data=train)

mb <- brm(logN ~ loglagN + ppt_Jan, 
          family = gaussian(),
          prior = c(set_prior("normal(0, 1)", class = "b"),
                    set_prior("exponential(1)", class = "sigma")),
          chains = 3,
          iter = 1000,
          warmup = 500,
          data = train)
          
print(mb)

plot(mb)

pos <- posterior_samples(mb)


nsim = 1000      # Ensemble size
tot_time = dim(test)[1] + 1
nClim = matrix(NA, nsim, tot_time)   # storage for all simulations
init_obs = test$loglagN[test$year==2012]
nClim[,1] = rnorm(nsim, init_obs, 0)

for(i in 1: nsim){
  idx <- sample.int(dim(pos)[1], 1)
  for(j in 2: tot_time){
    best_guess = pos$b_Intercept[idx] + 
      pos$b_loglagN[idx] * nClim[i,j-1] + 
      pos$b_ppt_Jan[idx] * test$ppt_Jan[test$year == 2010 + j]
    nClim[i,j] = rnorm(1, best_guess, pos$sigma[idx]) 
  }
}


# calculate 95% credible intervals the median and 95% CI limits for each time 
library(coda)

CI <- HPDinterval(as.mcmc(nClim))
mp <- colMeans(nClim)
# plot the data
plot(c(train$year, test$year), c(train$logN, test$logN), ylim = c(6, 10), type = "l", xlab = "year", ylab = "log N")
# add predictions, upper and lower CI's
lines(test$year, mp[2: ncol(nClim)], col = "red", lty = "solid")
lines(test$year, CI[2: ncol(nClim), 1], col = "red", lty = "dashed")
lines(test$year, CI[2: ncol(nClim), 2], col = "red", lty = "dashed")

accuracy(mp[2: tot_time], test$logN)

```
