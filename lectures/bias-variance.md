---
title: "Sesgo y Varianza"
output:
  html_document: default
layout: post
mathjax: true
---

Comprender cómo distintas fuentes de errores producen sesgo y varianza en nuestras predicciones nos sirve para producir mejores modelos.

Imaginemos que podemos repetir muchas veces el proceso de colectar datos, ajustar un modelo a esos datos, y hacer predicciones. En cada una de estas réplicas tenemos una muestra aleatoria, los modelos que ajustamos a los datos van a generar una rango de predicciones. El **sesgo** mide qué tan lejos están las predicciones del valor verdadero. En tanto, la **varianza** mide qué tanto varían las distintas predicciones respecto a un punto en particular.

Suponiendo que queremos predecir una variable $Y$ a partir de covariables $X$, podemos asumir un modelo de datos (*data generating model*) del tipo

$$
Y = f(x) + \epsilon
$$

Donde el error $\epsilon$ tiene media cero y distribución normal $\epsilon \sim \text{N} \left(0, \sigma_{\epsilon} \right)$ 

Con una regresión o con algún otro método estadístico, podemos estimar un modelo $\hat{f} \left(X \right)$ para representar $f\left(X \right)$. En ese caso, el *expected squared prediction error* en un punto $x$ es:

$$
\text{Err} \left( x \right) = \text{E} \left[ \left( Y − \hat{f} \left(x \right) \right)^2 \right]
$$

de acuerdo con las reglas de probabilidad, podemos descomponer este error en componentes de sesgo y de varianza:

$$
\text{Err} \left( x \right) = \left( \text{E} \left[\hat{f} \left( x \right) \right] - f \left(x \right) \right)^2  + \text{E} \left[ \left( \hat{f} \left(x \right) - \text{E}\left[ \hat{f} \left(x \right) \right] \right)^2 \right] + \sigma^2_{\epsilon}
$$

Es decir:
$$
\text{Err} \left( x \right) = \text{Sesgo} + \text{Varianza} + \text{error irreducible}
$$

El error irreducible representa variabilidad no explicada por el modelo, aún cuando conocemos perfectamente $f\left(x\right)$. Esta variabilidad está fuera de nuestro control (así es la vida). Los componentes de sesgo y varianza podrían reducirse si conocemos el modelo que genera los datos y si tenemos muchos datos. Pero como normalmente trabajamos con pocos datos y con modelos imperfectos, tenemos un compromiso entre sesgo y varianza.

Veamos un ejemplo con datos simulados

```R
set.seed(123)
n <- 20
a <- 0
b1 <- 0.15
b2 <- -0.4
s <- 1

x1 <- runif(n, -3, 3)
x1 <- sort(x1) # para graficar
x2 <- runif(n, -3, 3)

m <- a + b1 * x1 + b2 * x2
y <- rnorm(n, mean = m, sd = s)

# simulamos más covariables
x3 <- runif(n, -3, 3)
x4 <- runif(n, -3, 3)
x5 <- runif(n, -3, 3)

# ajustamos modelos lineales
m1 <- lm(y ~ 1)
m2 <- lm(y ~ x1)
m3 <- lm(y ~ x1 + x3)
m4 <- lm(y ~ x1 + x3 + x4)
m5 <- lm(y ~ x1 + x3 + x4 + x5)

plot(x1, y)
lines(x1, predict(m1), col = rgb(0, 0, 0, 0.5), lwd = 2)
lines(x1, predict(m2), col = rgb(0.25, 0, 0.6, 0.5), lwd = 2)
lines(x1, predict(m3), col = rgb(0.5, 0, 0.6, 0.5), lwd = 2)
lines(x1, predict(m4), col = rgb(0.75, 0, 0.6, 0.5), lwd = 2)
lines(x1, predict(m5), col = rgb(1, 0, 0, 0.6), lwd = 2)
```

A partir de estos análisis podemos calcular el "training error" de cada modelo y comprobar que a medida que la complejidad del modelo aumenta, el error disminuye.

```R
tr_e = numeric(5)
tr_e[1] = mean((y - predict(m1))^2)
tr_e[2] = mean((y - predict(m2))^2)
tr_e[3] = mean((y - predict(m3))^2)
tr_e[4] = mean((y - predict(m4))^2)
tr_e[5] = mean((y - predict(m5))^2)

plot(tr_e, xlab = "model complexity", ylab = "error")
```

Veamos qué pasa si simulamos nuevos conjuntos de datos (conservando los valores de las predictoras)

```R
nrep <- 1000

f_hat <- array(NA, dim = c(nrep, n, 5))
pr_e <- matrix(NA, nrep, 5)

for(i in 1: nrep){

x1 <- runif(n, -3, 3)
x1 <- sort(x1) # para graficar
x2 <- runif(n, -3, 3)

m <- a + b1 * x1 + b2 * x2
y <- rnorm(n, mean = m, sd = s)

# simulamos más covariables
x3 <- runif(n, -3, 3)
x4 <- runif(n, -3, 3)
x5 <- runif(n, -3, 3)


  y_rep <- rnorm(n, mean = m, sd = s)
  m1 <- lm(y_rep ~ 1)
  m2 <- lm(y_rep ~ x1)
  m3 <- lm(y_rep ~ x1 + x2)
  m4 <- lm(y_rep ~ x1 + x2 + x3)
  m5 <- lm(y_rep ~ x1 + x2 + x3 + x4)
  
  f_hat[i, , 1] <- predict(m1)
  f_hat[i, , 2] <- predict(m2)
  f_hat[i, , 3] <- predict(m3)
  f_hat[i, , 4] <- predict(m4)
  f_hat[i, , 5] <- predict(m5)
  
  pr_e[i, 1] <- mean((y - predict(m1))^2)
  pr_e[i, 2] <- mean((y - predict(m2))^2)
  pr_e[i, 3] <- mean((y - predict(m3))^2)
  pr_e[i, 4] <- mean((y - predict(m4))^2)
  pr_e[i, 5] <- mean((y - predict(m5))^2)
}

# calculemos bias y variance para observaciones nuevas

B <- numeric(5)
V <- numeric(5)
Ef <- matrix(NA,n,5)
for(i in 1:5){
  Ef[ , i] <- colMeans(f_hat[, , i])
  tmp <- matrix(NA, nrep, n)
  for(j in 1:nrep){
    tmp[j, ] <- mean((f_hat[j, ,i] - Ef[,i])^2)
  }
  B[i] <- mean((Ef[, i] - m)^2)
  V[i] <- mean(tmp)
}

```
Podemos ver cómo cambian el sesgo y la varianza según la complejidad del modelo

```R

plot(B, type = "l", lwd = 2, xlab = "model complexity", ylim = c(0,1))
lines(V, col = 2, lwd = 2)

```

Veamos cómo es el error de predicción de nuestros modelos

```R

plot(tr_e, ylim = c(0.5,2), type = "l", lwd = 3, xlab = "model complexity", ylab = "error")
lines(colMeans(pr_e), col = 2, lwd = 3)

```
Quizás una mejor forma de entender este problema sea considerando que tenemos que tratar de evitar dos riesgos, el de *underfitting* y el de *overfitting*. Modelos que sub-ajustan van a ser poco sensibles a nuevos datos, mientras que los que sobre-ajustan van a ser demasiado sensibles a nuevos datos.

```R
plot(NULL, xlim = c(-3,3), ylim = c(-3,3))
for(i in 1: 50){
  lines(x1, f_hat[i,,1], col = rgb(0,0,0, 0.2))
  lines(x1, f_hat[i,,5], col = rgb(1,0,0, 0.2))
  lines(x1, f_hat[i,,3], col = rgb(0,1,0, 0.2))
}
lines(x1, a + b1 * x1 + b2 * x2, lwd = 2)

```

**Relación con la selección de modelos**
Cómo hacemos para encontrar el punto óptimo entre underfitting y overfitting?
Antes que nada, tenemos que definir cómo medir qué tan bueno o malo es un modelo. Recién vimos por ejemplo el error de predicción como una medida para juzgar a los modelos. Hay buenas razones para considerar medidas basadas en la teoría de la información.

Cómo medimos qué tan lejos estamos de nuestro objetivo?

* La teoría de la información provee una buena forma de medir distancia entre dos distribuciones de probabilidad

* La *devianza* mide distancia relativa a una precisión perfecta

* En general, a los estadísticos les importa la *out of sample deviance*



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

Veamos qué pasa con la devianza cuando usamos las estimaciones de una muestra para predecir nuevos datos.


```R
set.seed(123)
n <- 20
a <- 0
b1 <- 0.15
b2 <- -0.4
s <- 1

nrep <- 1000

dev_in  <- matrix(NA, nrep, 5)
dev_out <- matrix(NA, nrep, 5)

for(i in 1: nrep){

X_in  <- matrix(runif(n*4, -3, 3), n, 4)
X_out <- matrix(runif(n*4, -3, 3), n, 4)

y_in  <- rnorm(n, mean = a + b1 * X_in[,1] + b2 * X_in[,2], sd = s)
y_out <- rnorm(n, mean = a + b1 * X_out[,1] + b2 * X_out[,2], sd = s)

  m1 <- lm(y_in ~ 1)
  m2 <- lm(y_in ~ X_in[,1])
  m3 <- lm(y_in ~ X_in[,1:2])
  m4 <- lm(y_in ~ X_in[,1:3])
  m5 <- lm(y_in ~ X_in[,1:4])
  
  dev_in[i, 1] <- -2 * sum(dnorm(y_in, coef(m1), sd = s, log = TRUE))
  dev_in[i, 2] <- -2 * sum(dnorm(y_in, cbind(numeric(n)+1, X_in[,1]) %*% coef(m2), sd = s, log = TRUE))
  dev_in[i, 3] <- -2 * sum(dnorm(y_in, cbind(numeric(n)+1, X_in[,1:2]) %*% coef(m3), sd = s, log = TRUE))
  dev_in[i, 4] <- -2 * sum(dnorm(y_in, cbind(numeric(n)+1, X_in[,1:3]) %*% coef(m4), sd = s, log = TRUE))
  dev_in[i, 5] <- -2 * sum(dnorm(y_in, cbind(numeric(n)+1, X_in[,1:4]) %*% coef(m5), sd = s, log = TRUE))
  
  dev_out[i, 1] <- -2 * sum(dnorm(y_out, coef(m1), sd = s, log = TRUE))
  dev_out[i, 2] <- -2 * sum(dnorm(y_out, cbind(numeric(n)+1, X_out[,1]) %*% coef(m2), sd = s, log = TRUE))
  dev_out[i, 3] <- -2 * sum(dnorm(y_out, cbind(numeric(n)+1, X_out[,1:2]) %*% coef(m3), sd = s, log = TRUE))
  dev_out[i, 4] <- -2 * sum(dnorm(y_out, cbind(numeric(n)+1, X_out[,1:3]) %*% coef(m4), sd = s, log = TRUE))
  dev_out[i, 5] <- -2 * sum(dnorm(y_out, cbind(numeric(n)+1, X_out[,1:4]) %*% coef(m5), sd = s, log = TRUE))
}

xs = 1:5
sd_in <- numeric(5)
for(i in 1:5) sd_in[i] <- sd(dev_in[,i])
sd_out <- numeric(5)
for(i in 1:5) sd_out[i] <- sd(dev_out[,i])

plot(xs, colMeans(dev_in), xlim = c(1,5.5), ylim = c(40,90), xlab = "complejidad", ylab = "devianza")

points(xs+0.2, colMeans(dev_out), pch = 19)

segments(xs, colMeans(dev_in)-sd_in, xs, colMeans(dev_in)+sd_in)
segments(xs+0.2, colMeans(dev_out)-sd_out, xs+0.2, colMeans(dev_out)+sd_out)
```

Vemos que la devianza estimada con la muestra disminuye con la complejidad del modelo, mientras que la devianza de datos nuevos (out-of-sample) tiene un mínimo en el modelo 3. Además, la diferencia en devianza de la muestra contra la de nuveos datos es aproximadamente 2 $\times$ el número de parámetros del modelo. AIC hace justamente esa cuenta: $\text{devianza} + 2 k$ donde $k$ es el número de parámetros del modelo.

En general, el problema de *overfitting* ocurre porque como dice McElreath, nuestro modelo se entusiasma demasiado con la muestra. 

Otra cara de esa moneda son los errores tipo M y S. La idea es la siguiente. Si un efecto (real) es de poca magnitud y tenemos muestras chicas o ruidosas (mediciones con poca precisión), solamente vamos a detectar efectos estadísticamente significativos cuando la magnitud estimada del efecto es exagerada (error M) y esto puede ocurrir incluso con efectos de signo contrario al verdadero (error S). Una buena referencia para estos errores se puede ver [aquí](http://www.stat.columbia.edu/~gelman/research/published/retropower_final.pdf). O buscando en el blog de Gelman https://statmodeling.stat.columbia.edu/

Veamos de qué se trata haciendo unas simulaciones. Vamos a suponer que existe un efecto real entre una variable predictora y una respuesta, pero que la magnitud del efecto no es muy grande. Podemos simlular el caso de una regresión simple: 

```R
set.seed(123)

n = 20
b0 = 0.5
b1 = 0.1
sigma = 1 

x = runif(n, -3,3)
y = rnorm(n, b0 + b1 * x, sd = sigma)

plot(x, y)
curve(b0 + b1 * x, add = TRUE, lwd = 2, col = 2)
abline(lm(y~x))
```

Podemos hacer unas cuantas réplicas y ver cuándo tenemos estimaciones significativas para la pendiente (b1) y luego graficar las relaciones estimadas entre $x$ e $y$

```R
n.reps = 1000
n = 20
res = matrix(NA, n.reps, 2)
se = numeric(n.reps) # para guardar los errores estándar de la pendiente
ps = numeric(n.reps) # para guardar los valores de p de la pendiente

for(i in 1: n.reps){
  x = runif(n, -3,3)
  y = rnorm(n, b0 + b1 * x, sd = sigma)
  tmp = summary(lm(y~x))$coefficients
  se[i] = tmp[2,2]
  ps[i] = tmp[2,4]
  res[i, ] = tmp[,1]
}
```

Grafiquemos las regresiones que fueron significativas

```R

plot(NULL, xlim = c(-3, 3), ylim = c(-1, 3), xlab = "x", ylab="y")
for(i in 1: n.reps){
  if(ps[i] <= 0.05){
    curve(res[i,1] + res[i,2] * x, add = TRUE, col=rgb(0,0,0, 0.2))
  } 
} 

curve(b0 + b1 * x, add = TRUE, col = 2, lwd = 3)
```

Las técnicas de *regularización* buscan restringir de alguna manera los valores que pueden tomar los coeficientes y así tratar de evitar el overfitting. Por ejemplo, cuando ajustamos un modelo usando mínimos cuadrados, buscamos minimizar 

$$
\sum_{i = 1}^{n} \left( y_i - \beta_0 + \sum_{j = 1}^{p} \left( x_{ij} \beta_j \right)  \right)^2
$$

Los métodos de Ridge regression, LASSO y Elastic net penalizan esta cuenta (o el likelihood según la implementación) para evitar overfitting

Elastic net usa

$$
\sum_{i = 1}^{n} \left( y_i - \beta_0 + \sum_{j = 1}^{p} \left( x_{ij} \beta_j \right)  \right)^2 + \lambda \sum_{j = 1}^{p} \left[ \frac{1}{2} \left(1 - \alpha \right) \beta_{j}^2 + \alpha  \lVert \beta_j \rVert \right]
$$

Donde $\lambda$ controla el grado de penalidad y $\alpha$ cuanto de "ridge" o de "LASSO" usamos.

Como veremos más adelante, estas penalizaciones se pueden resolver de manera más elegante con métodos Bayesianos usando previas informativas.




