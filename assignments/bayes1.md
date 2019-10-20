---
title: ""
output:
  html_document: default
layout: post
mathjax: true
---

# Modelos Simples con `JAGS`

En este práctico vamos a ajustar regresiones lineales. Para eso tenemos que aprender a escribir las correspondientes funciones de likelihood y aprender a definir las previas de los parámetros. 

### Objetivos:

- Familiarizarse con la formulación de funciones de likelihood (modelos de datos)
- Simular datos a partir de distintas funciones de likelihood
- Usar `JAGS` para estimar parámetros a partir de los datos simulados

Veamos por ejemplo un modelo de crecimiento logístico en tiempo discreto

```R
set.seed(123)

r = 0.8       # Intrinsic growth rate
K = 10        # carrying capacity
s = 0.5       # process error

# Simulation parameters:
NT = 20       # Time steps in each run
n0 = 0.5      # Initial population size

n = numeric(NT) + n0   

for(t in 2:NT){      
  n[t] = rnorm(1, n[t-1] + r * n[t-1] * (1 - n[t-1]/K), s)
}
```


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
           K ~ dnorm(0, 0.01)I(0, )
           tau <- 1/(sigma*sigma) 
           sigma ~ dunif(0, 1)
    }
")
```

**Pregunta**: ¿Qué tan informativas son estas previas?   

Como vimos antes, para llamar a `JAGS`, cargamos el paquete `jagsUI`, definimos los datos, los parámetros y luego usamos la función `jags`. 

```R
library(jagsUI)

datos <- list(n = n, NT = NT)

# vector con los parámetros para que JAGS guarde los valores de las cadenas Markovianas
params <- c("r", "K", "sigma")

# función para generar valores iniciales de los parámetros (no confundir con las previas!)
inits <- function() list(r = runif(1, 0.2, 1), 
                         K = runif(1, 6, 15), 
                         sigma = runif(1, 0, 1))
```

Finalmente, definimos cuántas iteraciones queremos correr por cadena, cuántos valores vamos a ir descartado en una secuencia ("thin"), qué largo tiene el "burn in" y cuántas cadenas queremos correr.

```R
ni <- 1000  
nt <- 1     
nb <- 500   
nc <- 3     
```

Ahora llamamos a `JAGS` para que genere las cadenas Markovianas

```R
m1.sim <- jags(data = datos, inits = inits, parameters.to.save = params, 
               model.file = "m1.bug", 
               n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt)

```


### Ejercicios

1. graficar la incertidumbre en $n$ para todos los $t$.

2. Modificar la simulación y el modelo de `BUGS` para que los $n$ tengan valores discretos.

3. Modificar la simulación y el modelo de `BUGS` para incluir error de observación.

