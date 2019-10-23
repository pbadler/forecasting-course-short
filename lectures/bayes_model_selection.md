---
title: "Bayes and Model Selection"
output:
  html_document: default
layout: post
mathjax: true
---

  *Essentially all models are wrong but some are useful*
  
  George Box

Cuando ajustamos modelos usando métodos Bayesianos podemos hacer varias cosas. Podemos preguntar por ejemplo si los datos son posibles bajo el modelo ajustado y considerando la incertidumbre en los parámetros que está repesentada en la posterior conjunta. 

### Posterior predictive checks

Ejemplo: independencia en una secuencia de tipo presencia/ausencia

Colectamos datos en una transecta donde registramos cada $10$ metros la presencia ($1$) o ausencia ($0$) de una especie de interés en parcelas de $1$ metro cuadrado y obtenemos el siguiente resultado:

```{r}
y <- c(1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

```

Como una primera aproximación, podríamos pensar que un modelo apropiado para estos datos sería una Binomial con probabilidad de éxito (presencia) fijo. Como vimos en el práctico de análisis Bayesianos, existe una solución analítica para la posterior de la probabilidad de éxito de una Binomial si usamos como previa una distribución Beta. Usando una previa no-informativa tenemos:

```{r}
op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
curve(dbeta(x, 1 + sum(y), 1 + length(y) - sum(y)), lwd = 2, xlab = "Probabilidad de presencia", ylab = "Densidad")
par(op)

```

¿Qué tan apropiado es este modelo para nuestro datos? Si miramos los datos, parece haber cierta autocorrelación en la presencia de la especie (los $0$ y $1$ están amontonados), pero nuestro modelo asume independencia entre parcelas. ¿Es posible que la estructura espacial que vemos sea producto del azar? En un contexto Bayesiano podemos responder a esta pregunta teniendo en cuenta no solo propiedades de los datos y del modelo de datos (distribución Binomial) sino también la incertidumbre alrededor de las estimaciones de los parámetros (en este caso tenemos un solo parámetro, la probabilidad de presencia). 

Cuando hacemos un *posterior predictive check*, queremos ver si nuestros datos son consistentes con el modelo que ajustamos a ellos, teniendo en cuenta la incertidumbre existente acerca de qué valores tienen los parámetros. La idea es bastante intuitiva: si el modelo es apropiado para nuestros datos, entonces nuestros datos deberían ser coherentes con datos generados (simulados) a partir de ese modelo. Hay varias formas de definir si los datos observados son *consistentes* o son *coherentes* con el modelo que ajustamos. Una forma es definir como "test quantity" una propiedad de los datos que queremos que el modelo capture. Por ejemplo, para los datos que estamos considerando podemos calcular cuántas veces hay un cambio de presencia a ausencia en la transecta. En los datos originales, encontramos apenas tres cambios y queremos ver qué tan probable es encontrar ese valor si los datos provienen de una distribución Binomial con probabilidad de éxito estimada a partir de los datos. Para eso podemos ver si la cantidad de cambios observada cae dentro de la *distribución predictiva posterior* de este estadístico.

¿Pero, qué es la *distribución predictiva posterior*? 

Empecemos desde el principio... La regla de Bayes dice que

$$
P(\theta|y) = \frac{P(y| \theta) P(\theta)}{P(y)}
$$

Donde $P(y)$ es la probabilidad total (o marginal) de los datos, que es algo que podemos calcular así: 

$$
P(y) = \int P(y|theta) P(\theta) d \theta
$$

Esa es la *distribución predictiva previa* es decir, una distribución que predice la probabilidad de datos que no hemos observado en base al modelo de datos (likelihood) y las previas. Una vez que observamos un set de datos ($y_o$), podemos calcular la *distribución predictiva posterior* para datos que todavía no observamos pero en base a lo que aprendimos con los datos $y_o$:

$$
P(y^{\text{rep}}|y_o) = \int P(y^{\text{rep}}|\theta) P(\theta| y_o) d \theta
$$

donde $y^{\text{rep}}$ es una nueva observación o un set de datos nuevos. Ahora podemos ver qué propiedades tendrían estos $y^{\text{rep}}$ y ver si los datos observados son consistentes con estas predicciones. Si usamos un "test quantity" $T(y, \theta)$, podemos calcular una especie de valor de *p* (Bayesian *p*-value, aunque suene perverso) para lo cual tenemos que resolver esta cuenta:

$$
p_B \left( T(y^{\text{rep}}, \theta  )  \geq T(y, \theta) |y \right)
$$

Para lo cual hay que calcular:

$$
p_B = \int \int I_{T(y^{\text{rep}, \theta}) \geq T(y, \theta)} p(y^{\text{rep}}| \theta) p(\theta|y) d y^{\text{rep}} d \theta
$$

donde $I$ es una función indicadora que vale $1$ si la condición se cumple y $0$ si no se cumple. Si bien esta equación parece formidable, veremos que es fácil resolverla usando Monte Carlo. Lo que tenemos que hacer es obtener valores de la posterior y a partir de ellos simular valores de $y^{\text{rep}}$, calcular $T$ para cada una de estas réplicas y luego ver qué proporción de estos son más extremos o iguales que los observados. 

Para hacer estas cuentas con el ejemplo de presencia/ausencia de una especie en una transecta, primero vamos a escribir una función que cuente cuántos cambios hay en una secuencia:

```{r}
countsw <- function(y){
  count <- 0
  for(i in 2: length(y)){
    if(y[i] != y[i-1]) count <- count + 1
  }
  return(count)
}
```

Con esta función podemos ver cuál es el valor observado de número de cambios y luego hacer unas cuantas simulaciones de datos a partir de la posterior de la probabilidad de presencia.

```{r}
T_o <- countsw(y) # número de cambios observado

nsims <- 10000 
theta <- rbeta(nsims, 1 + sum(y), 1 + length(y) - sum(y)) 
y_rep <- matrix(NA, nrow = nsims, ncol = length(y))

for(i in 1: nsims){
  y_rep[i, ] <- rbinom(length(y), 1, theta[i])
}

Ts <- numeric(nsims)
for(i in 1: nsims){
  Ts[i] <- countsw(y_rep[i, ]) # núm cambios
}

```
Ahora podemos ver cómo es la distribución del número de cambios para los datos simulados y ver dónde cae la cantidad de cambios observada en el set de datos original

```{r}
op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
plot(table(Ts) / length(Ts), col = "gray", lwd = 4, ylab = "Probabilidad")
arrows(T_o, 0.1, T_o, 0.025, angle = 20, lwd = 3)
par(op)
```

Y podemos calcular un valor de *p* bayesiano  ¯\_(ツ)_/¯

```{r}
p_B <- length(which(Ts <= T_o)) / nsims 
p_B
```

¿Qué quiere decir este valor respecto a la pregunta original de si el modelo que usamos es apropiado para estos datos?

Más allá de este ejemplo de cómo hacer un *posterior predictive check*, es importante que vean con qué facilidad podemos simular datos del modelo teniendo en cuenta lo que aprendimos de los datos (las posteriores). Con los datos que simulamos a partir de las posteriores podemos hacer cualquier cosa que nos resulte interesante...


### Validación cruzada

La idea es dejar cierta cantidad de datos como "train" y otra como "test". Un extremo es dejar de lado una sola observación como "test" y usar el resto como train. Si hacemos esto para cada una de las observaciones, tenemos un **leave one out cross validation**. Hacer esto es computacionalmente muy caro, pero podemos usar una aproximación usando un **Pareto-smoothed importance sampling leave-one-out cross-validation**( [Vehtari, A., Gelman, A., and Gabry, J. 2017](http://arxiv.org/abs/1507.02646/) ). Veamos un ejemplo con los datos de Bisontes. 

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

library(brms)


mb <- brm(logN ~ loglagN + ppt_Jan, 
          family = gaussian(),
          prior = c(set_prior("normal(0, 2)", class = "b"),
                    set_prior("exponential(1)", class = "sigma")),
          chains = 3,
          iter = 1000,
          warmup = 500,
          data = train)
          
mlag <- brm(logN ~ loglagN, 
          family = gaussian(),
          prior = c(set_prior("normal(0, 2)", class = "b"),
                    set_prior("exponential(1)", class = "sigma")),
          chains = 3,
          iter = 1000,
          warmup = 500,
          data = train)          

loo_mb <- loo(mb, reloo = TRUE)

loo_lag <- loo(mlag, reloo = TRUE)

print(loo_compare(loo_mb, loo_lag), digits = 3)
          
```

### Criterios de información


* La teoría de la información provee una buena forma de medir distancia entre dos distribuciones de probabilidad
* La *devianza* mide distancia relativa a una precisión perfecta
* Queremos estimar la *out of sample deviance*. Es decir qué tan lejos vamos a estar de una muestra nueva.


Informaciónn: La reducción en incertidumbre derivada de conocer un resultado.

Information entropy:
$$
H \left( p \right) = - \text{E} \log \left( p_i \right) = \sum_{i=1}^{n} p_i \log \left( p_i \right)
$$

La incertidumbre contenida en una distribución de probabilidad es el promedio de log-probabilidades de un evento.

Divergencia (Kullback-Leibler): La incertidumbre adicional generada por usar probabilidades de una distribución para describir a otra 

Para estimar la divergencia de un modelo usamos la devianza 
$$
-2 \sum_{i} \log \left( p_i \right)
$$

Cuando trabajamos con métodos Bayesianos, consideramos la incertidumbre en los parámetros y calculamos la **log pointwise predictive density**:

$$
\text{lppd} \left(y | \theta \right) = \sum_i \log \frac{1}{S} \sum_s P(y|\theta_s)
$$

donde la primera suma es sobre las observaciones ($i$), y la segunda sobre las muestras de la posterior. $S$ es el tamaño de la muestra de la posterior. 

Si:

* usamos previas no informativas
* tenemos muchas más observaciones que parámetros
* la posterior es aproximadamente multivariada normal

Entonces podemos usar AIC
$$
AIC = -2 \text{lppd} + 2 k
$$

Pero muchas veces no usamos previas no informativas, o no sabemos bien cómo contar el número de parámetros (modelos con *random effects*). El **Deviance Information Criterion** tiene en cuenta estos aspectos pero todavía asume posteriores aproximadamente normales y tamaños de muestras grandes.

El **Widely Applicable Information Criterion** (WAIC) no asume nada respecto a la forma de las posteriores.

$$
WAIC = -2 \text{lppd} + 2 \sum_i \text{var}_{\theta} \log p(y_i| \theta)
$$

Ahora la penalidad no es con el númnero de parámetros del modelo sino con algo que tiene que ver con la varianza en las log probabilidades de cada observación. Algunos interpretan a esta penalidad como el "número efectivo de parámetros".



