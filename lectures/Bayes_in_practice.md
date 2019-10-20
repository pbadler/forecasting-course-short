---
title: "Bayes in practice"
output:
  html_document: default
layout: post
mathjax: true
---

En esta clase vamos a ver cómo ajustar modelos Bayesianos usando `JAGS`, `Stan` y `brms`.

Volvamos al ejemplo de los búfalos en Yellowstone. Anteriormente vimos como ajustar un modelo del tipo `lm(logN ~ loglagN + ppt_Jan, data=train )`. Para ajustar ese modelo en `JAGS` tenemos que hacer varias cosas...

`JAGS` es un software que programa cadenas de Markov Chain Monte Carlo (MCMC) para modelos bayesianos (Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs sampling. In Proceedings of the 3rd international workshop on distributed statistical computing (dsc 2003), Vienna, Austria.ISSN 1609-395X. Plummer, M. (2015). JAGS version 4.0 user manual).

`JAGS` es un sucesor de `BUGS`, que es *Bayesian inference using Gibbs sampling* (Lunn, Jackson, Best, Thomas, & Spiegelhalter, 2013; Lunn, Thomas, Best, & Spiegelhalter, 2000). `JAGS` es muy parecido a `BUGS` pero tiene algunas funciones extra y a veces es más rápido. Además, `JAGS` se puede usar en Windows, Mac y Linux.

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

Un detalle para tener en cuenta es que `BUGS` usa precisión ($1/ \sigma^2$) en la distribución normal. Pero como estamos acostumbrados a interpretar desvío estándar, definimos la previa sobre el desvío y luego transformamos a precisión.

Ahora cargamos los datos y los acomodamos un poco, además de preparar los datos como hicimos antes, vamos a generar algunos data.frames extras (ver `bb_wide` y `btrain`):

```R
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(mvtnorm)

bison = read.csv("https://raw.githubusercontent.com/pbadler/forecasting-course-short/master/data/YNP_bison_counts.csv", stringsAsFactors = FALSE)
bison = select(bison,c(year,count.mean)) # drop non-essential columns
names(bison) = c("year","N") # rename columns
bbison = bison # to use with JAGS

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

Ahora armamos una lista con los datos que vamos a pasarle a `JAGS`, y un vector con los nombres de los parámetros que queremos guardar:

```R
datos <- list(n = dim(btrain)[1],
              logN = log(btrain$N),
              ppt_Jan = btrain$ppt_Jan)

params <- c("b0", "b_lag", "b_ppt", "s")

```

Además, queremos que las cadenas Markovianas comiencen en distintos valores para poder corroborar que converjan a la misma distribución estacionaria. Para esto escribimos una función que genera valores iniciales de las cadenas (recuerden que esto no tiene nada que ver con las previas)

```R
inits <- function() list(b0 = runif(1, 0, 1), 
                         b_lag = runif(1, 0, 1), 
                         b_ppt = runif(1, 0, 1),
                         s = runif(1, 0, 10) )
                         
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
Para ver los resultados de este análisis podemos pedirle a `R` que "imprima" la salida de la función `jags` usando `print(bison.sim)` en este caso. Vemos que se reporta el nombre del modelo que se usó, cuántas cadenas se simularon y otros detalles como el tiempo de ejecución. Después aparece una tabla con la lista de parámetros que le pedimos que registre y la devianza. En la tabla aparecen también la media, desvío y cuantiles estimados a partir de las cadenas Markovianas. También aparece una columna (`overlap0`) que nos dice si la posterior incluye al cero o no, y otra (`f`) que nos dice qué fracción de la posterior es del mismo signo que la media. Finalmente, aparecen dos columnas con información importante. `Rhat` estima si las cadenas convergieron a una distribución estable y `n.eff` estima el número efectivo de muestras de la posterior que surgen de las cadenas. Antes que hacer nada con la salida de JAGS, tenemos que chequear que las cadenas hayan convergido (`Rhat` $\leq 1.1$). También, ver que el tamaño efectivo de la muestra de la posterior (`n.eff`) sea suficiente. Si encontramos algún valor de `Rhat` $> 1.1$, quiere decir que las cadenas Markovianas para ese parámetro (o cantidad no observada que queremos estimar) todavía no llegaron a una distribución estacionaria. A veces solo es necesario correr más iteraciones de MCMC, pero otras veces es signo de que hay problemas a nivel de la estructura del modelo, o que la combinación de datos y previas que tenemos no son suficientemente informativas como para estimar las posteriores. 

```R

print(bison.sim)

```

Podemos ver unos gráficos de las cadenas y posteriores:

```R

plot(bison.sim)

```
Si queremos hacer predicciones, tenemos que tener en cuenta la incertidumbre en los valores de los parámetros. Esa incertidumbre está representada por la posterior conjunta de nuestro modelo. Veamos como hacer una predicción de Monte Carlo a partir de esta posterior conjunta:

```R
nsim = 1000      # Ensemble size
tot_time = dim(test)[1] + 1
nClim = matrix(NA, nsim, tot_time)   # storage for all simulations
init_obs = test$loglagN[test$year == 2012]
nClim[,1] = rnorm(nsim, init_obs, 0)

for(i in 1: nsim){
  idx <- sample.int(bison.sim$mcmc.info$n.samples, 1)
  for(j in 2: tot_time){
    best_guess = bison.sim$sims.list$b0[idx] + 
      bison.sim$sims.list$b_lag[idx] * nClim[i, j-1] + 
      bison.sim$sims.list$b_ppt[idx] * test$ppt_Jan[test$year == 2010 + j]
    nClim[i, j] = rnorm(1, best_guess, bison.sim$sims.list$s[idx]) 
  }
}


library(coda)

CI <- HPDinterval(as.mcmc(nClim))
mp <- colMeans(nClim)

plot(c(train$year, test$year), c(train$logN, test$logN), 
ylim = c(6, 10), type = "l", xlab = "year", ylab = "log N")

lines(test$year, mp[2: ncol(nClim)], col = "red", lty = "solid")
lines(test$year, CI[2: ncol(nClim), 1], col = "red", lty = "dashed")
lines(test$year, CI[2: ncol(nClim), 2], col = "red", lty = "dashed")

accuracy(mp[2: tot_time], test$logN)

```

Un aspecto bastante útil de los modelos formulados en lenguaje `BUGS` es que podemos usarla directamente para hacer predicciones. Esto es porque cuando escribimos algo como `logN[i] ~ dnorm(mu[i], tau)` estamos diciendo que los valores de `logN` son muestreados de una distribución normal. Cuando `JAGS` encuentra valores en `logN[i]`, va a usar esos valores para actualizar las cadenas Markovianas de los parmámetros de esa distribución normal. Si en vez de encontrar valores encuentra `NA` (valores perdidos), entonces genera una muestra de la normal en base a los valores de los parámetros en las cadenas Markovianas. Veamos cómo usar esta característica para predecir el futuro:

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

Veamos como ajustar este mismo modelo pero usando `Stan`. A diferencia de `JAGS`, (entre otras cosas) `Stan` usa Hamiltonian Monte Carlo. Es una plataforma de alta performance para modelado estadístico. 

Primero tenemos que definir el modelo usando el lenguaje de `Stan`:

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
nClim[, 1] = rnorm(nsim, init_obs, 0)

for(i in 1: nsim){
  idx <- sample.int(dim(pos$b0), 1)
  for(j in 2: tot_time){
    best_guess = pos$b0[idx] + 
      pos$b_lag[idx] * nClim[i, j-1] + 
      pos$b_ppt[idx] * test$ppt_Jan[test$year == 2010 + j]
    nClim[i,j] = rnorm(1, best_guess, pos$sigma[idx]) 
  }
}

library(coda)

CI <- HPDinterval(as.mcmc(nClim))
mp <- colMeans(nClim)

plot(c(train$year, test$year), c(train$logN, test$logN), ylim = c(6, 10), type = "l", xlab = "year", ylab = "log N")

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

nsim = 1000 
tot_time = dim(test)[1] + 1
nClim = matrix(NA, nsim, tot_time)
init_obs = test$loglagN[test$year == 2012]
nClim[, 1] = rnorm(nsim, init_obs, 0)

for(i in 1: nsim){
  idx <- sample.int(dim(pos)[1], 1)
  for(j in 2: tot_time){
    best_guess = pos$b_Intercept[idx] + 
      pos$b_loglagN[idx] * nClim[i, j-1] + 
      pos$b_ppt_Jan[idx] * test$ppt_Jan[test$year == 2010 + j]
    nClim[i,j] = rnorm(1, best_guess, pos$sigma[idx]) 
  }
}

library(coda)

CI <- HPDinterval(as.mcmc(nClim))
mp <- colMeans(nClim)

plot(c(train$year, test$year), c(train$logN, test$logN), ylim = c(6, 10), type = "l", xlab = "year", ylab = "log N")

lines(test$year, mp[2: ncol(nClim)], col = "red", lty = "solid")
lines(test$year, CI[2: ncol(nClim), 1], col = "red", lty = "dashed")
lines(test$year, CI[2: ncol(nClim), 2], col = "red", lty = "dashed")

accuracy(mp[2: tot_time], test$logN)

```

### Para qué tanto lío?

Vimos cómo implementar análisis Bayesianos en `JAGS`, `Stan`, y `brms`. En general, usar estos métodos implica más trabajo de nuestra parte (y de parte de la computadora). Bayes nos obliga a escribir más código, a pensar en qué previas usar, a revisar que las cadenas hayan converjido, a chequear que el tamaño de muestra es suficiente, etc. Sin embargo, al final pareciera que otenemos los mismos resultados que con análisis mucho más simples como cuando usamos `lm(logN ~ loglagN + ppt_Jan, data=train )` y luego hicimos predicciones con métodos de Monte Carlo usando la matriz de varianza-covarianza de los coeficientes. 

En general, para modelos simples y con muchos datos, los análisis Bayesianos y los más "tradicionales" dan resultados muy similares a menos que las previas sean bastante informativas. Pero los métodos Bayesianos nos permiten expandir fácilmente nuestros modelos y analizarlos sin problemas. Además, no siempre podemos caracterizar a la covariación de los parámetros usando multivariadas normales. Veamos por ejemplo qué pasa cuando modelamos la distancia de desplazamiento diario (en km) de un elk. Vamos a ajustar una distribución Gamma a estos datos usando máxima veresomilitud y Bayes.

```R

library(emdbook)
library(bbmle)

datos <- read.csv("https://raw.githubusercontent.com/pbadler/forecasting-course-short/master/data/elk_steps.csv", header = TRUE)

steps <- datos$steps[1:30]

gNLL <- function(shape, scale) {
  -sum(dgamma(steps, 
              shape = shape, 
              scale = scale, 
              log = TRUE))
}

gm <- mean(steps)
cv <- var(steps)/mean(steps)

mt <- mle2(gNLL, start = list(shape = gm/cv, scale = cv))

summary(mt)

```

Ahora podemos hacer Monte Carlo para ver predicciones de distancias de desplazamiento

```R
mu <- coef(mt)
sigma <- vcov(mt)

hist(steps, 20, freq = FALSE, main = "", xlim = c(0, 4), ylim = c(0, 1.5))
curve(dgamma(x, shape = mu[1], scale = mu[2]), add = TRUE, lwd = 2)

NE = 100
coef_samples = rmvnorm(NE,mean=mu,sigma=sigma)  
for(i in 1:NE){
  curve(dgamma(x, shape = coef_samples[i,1], scale = coef_samples[i,2]), 
        add = TRUE, col = alpha("black", 0.2))
}

```

Veamos ahora la versión Bayesiana

```R

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


cat(file = "gamma.stan", 
    "
data { 
  int<lower=1> n;
  vector[n] y;
}

parameters {
  real<lower=0> shape;
  real<lower=0> rate;
}

model{
  shape ~ normal(0, 5);
  rate ~ normal(0, 5);
  y ~ gamma(shape, rate);
}

generated quantities{
  vector[n] y_pred;
  for(i in 1:n) y_pred[i] = gamma_rng(shape, rate);
}
"
)


gamma_dat <- list(n = length(steps),
                  y = steps)

fit <- stan(file = 'gamma.stan', data = gamma_dat,
            iter = 5000, thin = 5, chains = 3)

print(fit)


sims <- extract(fit, pars = c("shape", "rate"))

hist(steps, 20, freq = FALSE, main = "", xlim = c(0, 4), ylim = c(0, 1.5))

NE = 100
for(i in 1:NE){
  idx <- sample.int(length(sims$shape), 1)
  curve(dgamma(x, shape = sims$shape[idx], rate = sims$rate[idx]), 
        add = TRUE, col = alpha("black", 0.2))
}

```

Las diferencias en las predicciones de una versión y otra en este caso se deben a que la superficie de likelihood de este modelo tiene una forma más parecida a una banana que a un elipse

```R
bsurf = curve3d(gNLL(x,y), sys="none",
                from=c(0.01,0.01), to=c(6,1.5),
                n=c(91,91))

image(bsurf$x,bsurf$y, log(bsurf$z),
      xlab="shape", 
      ylab = "scale", 
      col=gray((20:0)/20))


points(sims$shape, 1/sims$rate, pch = 19, 
       col = alpha("red", 0.1))

points(rmvnorm(1000,mean=mu,sigma=sigma), 
       pch = 19, col = alpha("blue", 0.1))
       
``` 

