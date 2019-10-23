---
title: ""
output:
  html_document: default
layout: post
mathjax: true
---

## Modelos Simples con `JAGS`

En este práctico vamos a ajustar modelos con `JAGS`. Para eso tenemos que aprender a escribir las correspondientes funciones de likelihood y aprender a definir las previas de los parámetros. 

### Objetivos:

- Familiarizarse con la formulación de funciones de likelihood (modelos de datos)
- Simular datos a partir de funciones de likelihood
- Usar `JAGS` para estimar parámetros a partir de los datos simulados
- Ajustar modelos de crecimiento poblacional a los datos de bisontes en Yellowstone


Veamos por ejemplo un modelo de crecimiento logístico en tiempo discreto:

$$
N_t \sim \text{Normal} \left( N_{t-1} + r \times N_{t-1} \left(1 - \frac{N_{t-1}}{K} \right), \sigma^2 \right)
$$

Primero simulemos datos de acuerdo con este modelo

```R
set.seed(123)

r = 0.8       # Intrinsic growth rate
K = 5000      # carrying capacity
s = 500       # process error

# Simulation parameters:
NT = 20       # Time steps in each run
n0 = 700      # Initial population size

n = numeric(NT) + n0   

for(t in 2:NT){      
  n[t] = rnorm(1, n[t-1] + r * n[t-1] * (1 - n[t-1]/K), s)
}

plot(n, type = "o", ylim = c(0, 7000))

```

Ahora escribimos el modelo en leguaje `BUGS`.

```R
cat(file = "m1.bug",
    "
      model  {
           # Función de likelihood
           for( t in 2:NT){
                n[t] ~ dnorm (mu[t], tau) 
                mu[t] <- n[t-1] + r * n[t -1] * (1 - n[t-1]/K)
           }

           # Previas
           r ~ dnorm(0, 1)I(0, ) 
           K ~ dnorm(5000, 1e-06)I(0, )
           tau <- 1/(sigma*sigma) 
           sigma ~ dunif(0, 1000)
    }
")

```

**Pregunta**: ¿Qué tan informativas son estas previas?  

Para responder a esta pregunta podemos hacer un **prior predictive check**:

```R
# función para simular datos de una normal truncada en cero:
rtn <- function(n = 1, mu = 0, sd = 1){
  x = numeric(n)
  count = 0
  while(count < n) {
    tmp = rnorm(1, mu, sd)
    if(tmp > 0){
      count = count + 1
      x[count] = tmp
    }
  }
  return(x)
}

plot(n, type = "o", ylim = c(-1000, 10000))

nreps <- 100
r = rtn(nreps, 0, 0.5)
K = rtn(nreps, 5000, sqrt(1/1e-06))
s = runif(nreps, 0, 1000)
ns = matrix(NA, nreps, NT)

for(i in 1: 100){
  ns[i,1] = n0   
  for(t in 2: NT){      
    ns[i,t] = rnorm(1, ns[i,t-1] + r[i] * ns[i,t-1] * (1 - ns[i,t-1]/K[i]), s[i])
  }
  lines(ns[i,], col = rgb(0,0,0, 0.2))
}

```

Como vimos antes, para llamar a `JAGS`, cargamos el paquete `jagsUI`, definimos los datos, los parámetros y luego usamos la función `jags`. 

```R
library(jagsUI)

datos <- list(n = n, NT = NT)

params <- c("r", "K", "sigma")

# función para generar valores iniciales de los parámetros 
# (no confundir con las previas!)

inits <- function() list(r = runif(1, 0.5, 1), 
                         K = runif(1, 4000, 6000), 
                         sigma = runif(1, 1, 10))
```

Finalmente, definimos cuántas iteraciones queremos correr por cadena, cuántos valores vamos a ir descartado en una secuencia ("thin"), qué largo tiene el "burn in" y cuántas cadenas queremos correr.

```R
ni <- 5000  
nt <- 5     
nb <- 2500   
nc <- 3  

```

Ahora llamamos a `JAGS` para que genere las cadenas Markovianas

```R
m1.sim <- jags(data = datos, 
               inits = inits, 
               parameters.to.save = params, 
               model.file = "m1.bug", 
               n.chains = nc, 
               n.iter = ni, 
               n.burnin = nb, 
               n.thin = nt)

print(m1.sim)

```

Para este práctico, el objetivo es ajustar este modelo de crecimiento logístico en tiempo discreto a los datos de bisontes en Yellowstone. 

**Recuerden** que antes de hacer nada con los resultados de los análisis, tenemos que verificar que los Rhat son $\leq 1.1$ y que los n.eff son aceptables

1- Ajustar el modelo logístico a los datos de bisontes y comparar la exactitud (accuracy) con el modelo de Gompertz que ajustamos en [Bayes in practice](lectures/Bayes_in_practice). Tengan en cuenta que este modelo es con N y no con logN.

2- Ajustar un modelo logístico donde el logaritmo de la tasa de crecimiento es una función lineal de la precipitación de enero y comparar la exactitud de este modelo con las anteriores.
