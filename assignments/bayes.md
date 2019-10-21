---
title: "Bayes"
output:
  html_document: default
layout: post
mathjax: true
---

# Regresiones Simples con `JAGS`

En este práctico vamos a ajustar regresiones lineales. Para eso tenemos que aprender a escribir las correspondientes funciones de likelihood y aprender a definir las previas de los parámetros. 

## Objetivos:

- Familiarizarse con la formulación de funciones de likelihood (modelos de datos)
- Simular datos a partir de distintas funciones de likelihood
- Usar `JAGS` para estimar parámetros a partir de los datos simulados

Empecemos con una relación lineal entre una variable predictora y una variable respuesta, y asumiendo una distribución normal para la variabilidad. Este es básicamente el modelo detrás de una regresión lineal simple. Más formalmente, podemos escribir el modelo de la siguiente manera:

$$
\begin{aligned}
& y_i \sim N(\mu_i, \sigma^{2}) \\
& \mu_i = \beta_0 + \beta_1 \times x_i\\
& \textrm{para } i \textrm{ de } 1:n \\
\end{aligned}
$$

Para simular datos de acuerdo con este modelo, primero tenemos que definir los parámetros (pendiente, ordenada al origen y desvío estándar), la variable predictora (también llamada covariable), y el número de observaciones. Noten que la distribución normal tiene dos parámetros, media y desvío. El "truco" para armar un modelo de regresión es hacer que la media dependa de la variable predictora. Como ahora la media es una función lineal de $x$, el modelo tiene tres parámetros ($\beta_0$, $\beta_1$, y $\sigma$). 

Definamos los parámetros, el número de observaciones y simulemos valores para la variable predictora (en este caso asumimos valores de una distribución uniforme entre $0$ y $20$):

```R
set.seed(1234)  
b0 <- 2  # Ordenada al origen (valor de la media cuando la predictora vale cero)
b1 <- 0.2    # Pendiente (cuánto cambia la media cuando la predictora cambia en una unidad)
s  <- 3    # Desvío estándar
n  <- 10   # Número de observaciones
x.sim <- runif(n, 0, 20) # covariable simulada
```

Ahora podemos calcular el valor esperado ($\mu_i$) para cada valor de $x$ y simular datos con una distribución Normal. Esto es hasta ahora igual a lo que hicimos en el práctico de probabilidades 

```R
m <- b0 + b1 * x.sim # media de la distribución normal
y.sim <- rnorm(n, mean = m, sd = s)  # datos simulados
```

Este modelo se puede ajustar fácilmente en `R` usando el comando `lm`, pero vamos a tomarnos el trabajo de escribir la función de likelihood y ajustarlo con `JAGS`. Para esto último tenemos que escribir el modelo en lenguaje `BUGS`, especificando la función de likelihood y las previas para todos los valores "no observados", que en este caso son los parámetros. Una cuestión importante es que en `JAGS` la distribución normal está formulada en base a la precisión en vez de desvío estándar. La precisión es $1/\sigma^2$ y normalmente se representa con la letra griega $\tau$ (tau). En el modelo que vamos a definir, ponemos como previa para el desvío una uniforme entre $0$ y $100$ y luego transformamos a valores de tau. Además, las previas que ponemos sobre `b0` y `b1` equivalen a una normal con media $0$ y varianza $=1/0.1 = 10$. 

```R
cat(file = "ml.bug",
    "
      model  {
           # Función de likelihood
           for( i in 1:n){
                y[i] ~ dnorm (mu[i], tau) 
                mu[i] <- b0 + b1 * x[i]
           }

           # Previas
           b0 ~ dnorm(0, 0.1) 
           b1 ~ dnorm(0, 0.1)
           tau <- 1/(sigma*sigma) 
           sigma ~ dunif(0, 100)
    }
    ")
```

**Pregunta**: ¿Qué tan informativas son estas previas?   

Como vimos antes, para llamar a `JAGS`, cargamos el paquete `jagsUI`, definimos los datos, los parámetros y luego usamos la función `jags`. En general, es importante centrar y estandarizar las variables predictoras de una regresión. Esto hace que las pendientes queden en unidades de desvió estándar de las predictoras (covariables) y que la ordenada al origen nos de el valor esperado de la variable de respuesta (y) cuando la predictora (x) está en su valor promedio. Para transformar (escalar) la variabe predictora, usamos la función `scale`

```R
library(jagsUI)

# defino las variables
x <- as.numeric(scale(x.sim))
y <- y.sim

# lista con los datos para pasarle a JAGS
datos <- list("x", "y", "n")

# vector con los parámetros para que JAGS guarde los valores de las cadenas Markovianas
params <- c("b0", "b1", "sigma")

# función para generar valores iniciales de los parámetros (no confundir con las previas!)
inits <- function() list(b0 = runif(1, 0, 1), 
                         b1 = runif(1, 0, 1), 
                         sigma = runif(1, 0, 10))
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
ml.sim <- jags(data = datos, inits = inits, parameters.to.save = params, 
               model.file = "ml.bug", 
               n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt)
```

Ahora veamos los resultados. Recuerden que primero tenemos que fijarnos si las cadenas convergieron (Rhat $\leq 1.1$) y además revisar que el `n.eff` sea suficientemente grande como para estimar con confianza propiedades de las posteriores (con un `n.eff` del orden de "cientos", podemos estimar media e intervalos de credibilidad aproximados. Algunas revistas son bastante exigentes y piden `n.eff` $\geq 4000$) pero en general, valores del orden de $1000$ son suficientes. 

```R
print(ml.sim)
```

Podemos graficar los datos y el modelo ajustado (en negro). Y ya que estamos compararlo con el resultado de `lm` (en gris). También podemos agregar al gráfico la relación verdadera entre $x$ y $\mu$ (en rojo). Noten que tenemos que transformar los valores de los coeficientes porque habíamos centrado y estandarizado la variable predictora.

```R
# generamos una secuencia de valores de x para hacer el gráfico
xvec <- seq(0, 20, length = 100) 

op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")

plot(x.sim, y, xlim = c(0, 20), pch = 21, bg = "gray")
lines(xvec, ml.sim$mean$b0 - (mean(x.sim) * ml.sim$mean$b1/sd(x.sim)) + ml.sim$mean$b1/sd(x.sim) * xvec, lwd = 3) #jags
lines(xvec, b0 + b1 * xvec, lwd = 3, col = 2) # la verdad
abline(lm(y ~ x.sim), lty = 2, col = "gray", lwd = 3) # lm

par(op)
```

Otra forma de ver qué aprendimos acerca de la relación entre $x$ e $y$ es ver la posterior de la pendiente. En este caso, como conocemos el valor verdadero de la pendiente, lo podemos incluir en el gráfico a ver cuánto difiere de lo que dice el análisis. De esta manera, podemos comprobar que nuestro procedimiento de generar muestras de la posterior a partir de cadenas Markovianas hace una buena estimación de la pendiente. 

```R
op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
hist(ml.sim$sims.list$b1/sd(x.sim), 60,  freq = FALSE, main = "", xlab = expression("b"[1]))
abline(v = b1, lwd = 3, lty = 2) #Valor verdadero de b1.
par(op)
```

Por otro lado vemos que con estos datos, tenemos cierta incertidumbre respecto de si la pendiente es poisitiva o negativa porque parte de la posteior es menor que cero. Para ver con más detalle qué porcentaje de la posterior es menor que cero podemos hacer `length(which(ml.sim$sims.list$b1 < 0)) / length(ml.sim$sims.list$b1)`, que en este caso da `r length(which(ml.sim$sims.list$b1 < 0)) / length(ml.sim$sims.list$b1)`.

¿Cómo compara este resultado con lo que obtendríamos si hacemos un análisis frecuentista? Podemos ajustar fácilmente este modelo usando la función `lm` de `R` para obtener:

```R
fit_lm <- lm(y~x)
summary(fit_lm)
```

Vemos que los coeficientes que estimamos son parecidos a los que encontramos con el análisis Bayesiano y que la pendiente no es significativamente diferente de $0$ para un alpha de $0.05$. Estos resultados no deberían sorprendernos porque las previas que usamos en el análisis Bayesiano no eran muy informativas. De hecho podemos tratar de usar los resultados de el modelo lineal para aproximar una medida de incertidumbre alrededor de la pendiente y compararla con la posterior. Para hacer esto, vamos a necesitar estimar el error estándar de la pendiente y suponer que podemos representar la incertidumbre con una distribución normal.

```R

op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
hist(ml.sim$sims.list$b1/sd(x.sim), 60,  freq = FALSE, main = "", xlab = expression("b"[1]))
curve(dnorm(x, fit_lm$coefficients[2]/sd(x.sim), se[2]/sd(x.sim)), add = TRUE, lwd = 2, col = 2)
abline(v = b1, lwd = 3, lty = 2) #Valor verdadero de b1.
par(op)

```


## Ejercicios

1. ¿De qué otra forma podemos estimar la fracción de la posterior que es menor que cero?

2. Mostrar gráficamente la posterior para la ordenada al origen y su valor verdadero.

3. ¿Qué pasa si para la previa de `b1` usamos una precisión de $10$? ¿Y si usamos una de $0.001$?

4. Repetir el análisis pero esta vez simulando $100$ observaciones. ¿Qué puede decir respecto a las posteriores de la ordenada al origen y la pendiente?

5. Hacer simulaciones y análisis como los de arriba pero reemplazando la distribución Normal por una Poisson. Para eso tienen que seguir los mismo pasos que hicimos con la distribución Normal. Presten atención al número de parámetros que tiene la distribución de Poison y qué valores pueden tomar.
