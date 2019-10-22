---
title: "Análisis Bayesianos"
output:
  html_document: default
layout: post
mathjax: true
---

Los análisis Bayesianos se basan en teoría de probabilidades. En general, trabajamos con **variables aleatorias**:

$$
P\left(X = x_i \right) = p_i
$$

La variable aleatoria $X$ toma el valor $x_i$ con probabilidad $p_i$. Para describir cómo cambian estas probabilidades usamos **distribuciones de probabilidad**. Estas pueden ser continuas o discretas.

Las distribuciones discretas se definen con la **función de masa de probabilidad** $f(x)$ que define los valores de probabilidad $p_i$ para cada posible valor $x_i$. Por definicón, estas probabilidades son no-negativas y no mayores de $1$. La suma de las probablidades de los eventos posibles tiene que ser igual a $1$. La **función acumulada de probabilidad** $F(x)$ nos da la probabilidad de que la variable aleatoria sea menor o igual a un valor particular $F(x_i) = P(X \leq x_i)$. Una diferencia importante entre las distribuciones discretas y contiunas es que para variables continuas tenemos infinitos valores posibles. Entonces ya no hablamos de la probabilidad de obtener un valor en particular sino de la **densidad de probabilidad** alrededor de un valor determinado. En general, no prestamos demasiada atención a los valores particulares de densidad de probabilidad sino a comparaciones relativas. La **función de densidad de probabilidad** está definida como $f(x) = d F(x) / d x$. Las densidades de probabilidad de variables continuas tienen que ser no-negativas, pero pueden ser mayores que $1$, pero deben integrar a $1$ sobre los valores posibles de la variable $X$.  

Cuando tenemos más de una variable aleatoria, podemos considerar **probabiliddes conjuntas** $P(x,y)$ que nos dan la probabilidad de obtener $x$ e $y$ simultáneamente. En el caso de que las variables sean independientes esa probabilidad es igual al producto $P(x) P(y)$. 

Podemos definir la **probabilidad marginal** como

$$
P(x) = \int P(x,y) dy
$$

Y podemos definir la probabilidad conjunta en función de la **probabilidad condicional**

$$
P(x,y) = P(x|y)P(y)
$$
$$
P(x,y) = P(y|x)P(x)
$$

A partir de estas condicionales, podemos derivar la regla de Bayes

$$
P(x,y) = P(x|y)P(y) = P(y|x)P(x)
$$

$$
P(x|y) = \frac{P(y|x)P(x)}{P(y)}
$$

Los anáilisis Bayesianos consideran a los parámetros ($\theta$) de un modelo como variables aleatorias y buscan caracterizar la distribución de pobabilidad de parámetros condicional en los datos observados:

$$
P(\theta | y) =  \frac{P(y| \theta)P(\theta)}{P(y)}
$$

Es decir que la probabilidad *posterior* de los parámetros $\theta$ dado que observamos los datos $y$ es igual al likelihood multiplicado por las previas y dividido por la probabilidad total de los datos. La función de likelihood nos da la probabilidad de observar los datos condicional al valor de los parámetros $P(y \lvert \theta)$. La previa de los parámetros $P(\theta)$ refleja los posibles valores de los parámetros de acuerdo con nuestras "creencias" previas, o los resultados de estudios anteriores, o lo que nos parece que tiene sentido para el sistema de estudio (en definitiva, en base a *información* previa). Lo importante es que definamos las previas **antes** de ver los datos. Finalmente, la probabilidad total de los datos se obtiene integrando la función de lilkelihood sobre los posibles valores de los parámetros:

$$
P(y) = \int P(y \vert \theta) d \theta
$$

Los análisis Bayesianos combinados con métodos numéricos permiten analizar modelos con muchos parámetros, niveles de variabilidad, varlores perdidos, variables "ocultas", etc. pero primero vamos a empezar por casos simples donde podemos calcular las posteriores directamente. 

Para entender bien cómo es todo el proceso, vamos a simular los datos. Imaginemos que queremos estudiar la remoción de frutos en $10$ plantas con $100$ frutos cada una y suponemos que un buen modelo para este tipo de datos es una distribución Binomial con una probabilidad de éxito fija:

```R
set.seed(123)
nobs <- 10    # número de observaciones (plantas)
frutos <- rep(100, nobs) # frutos disponibles
p.rem <- 0.2  # probabilidad de remoción por fruto
removidos <- rbinom(nobs, size = frutos, prob = p.rem)

```

En este caso, como el modelo de datos (cuántos frutos son removidos) es una Binomial, si usamos una distribución Beta como previa para el parámetro de probabilidad de éxito (`p.rem`) es posible obtener un resultado analítico para la posterior. En este caso, la posterior es otra distribución Beta pero con sus parámetros actualizados en base a las observaciones. Se dice entonces que la distribución Beta es la **conjugada** de la Binomial. Si la previa de la tasa de remoción por fruto es una distribución Beta con parámetros $\alpha$ y $\beta$, actualizamos los valores de $\alpha$ y $\beta$ en base a la cantidad de éxitos y fracasos obervados. La posterior de la tasa de remoción por fruto es entonces una Beta con $\alpha = \sum y$, $\beta = \sum (n-y)$ donde $y$ representa a los frutos removidos de los $n$ disponibles. Veamos como hacer esto en `R`. 

```R
# previas
alpha <- 2
beta  <- 2

pos.alpha <- alpha + sum(removidos[1:20])
pos.beta  <- beta + sum(frutos[1:20] - removidos[1:20])

#valor esperado de la posterior (según la distribución Beta)
pos.alpha / (pos.alpha + pos.beta)

# quantiles de la posterior
qbeta(c(0.025, 0.975), pos.alpha, pos.beta)

# ahora un gráfico

n = nobs

op <- par(cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, las = 1, bty = "n")
curve(dbeta(x, alpha + sum(removidos[1:n]) , beta + sum(frutos[1:n] - removidos[1:n])), 
      lwd = 2, ylab = "Densidad de probabilidad", xlab = "Probabilidad de remoción")
curve(dbeta(x, alpha, beta), lwd = 2, col = "gray", add = TRUE)
text(0.6, 2.5, "previa")
text(0.4, 9, "posterior")
par(op)

```

Con este script, podemos fijarnos cómo cambia la posterior si cambiamos la previa y si cambiamos el número de observaciones.

### MCMC

Vimos que para calcular la probabilidad marginal de los datos tenemos que calcular 
$$
\int P(y \vert \theta) d \theta
$$

En general, no podemos resolver estas integrales para los modelos que queremos ajustar (e.g. para una regresión simple con una ordenada al origen, una pendiente y un parámetro de varianza, tenemos que resolver una integral triple). Lo que hacemos es usar métodos de Markov Chain Monte Carlo para generar muestras de la distribución posterior.

Los enlaces de abajo muestran animaciones (hechas por Chi Feng) de algunos métodos de MCM

*   [Random Walk Metropolis Hastings](https://chi-feng.github.io/mcmc-demo/app.html?algorithm=RandomWalkMH&target=banana)
*   [Adaptive Metropolis Hastings](https://chi-feng.github.io/mcmc-demo/app.html?algorithm=AdaptiveMH&target=banana) [[1]](#ref-1)
*   [Hamiltonian Monte Carlo](https://chi-feng.github.io/mcmc-demo/app.html?algorithm=HamiltonianMC&target=banana) [[2]](#ref-2)
*   [No-U-Turn Sampler](https://chi-feng.github.io/mcmc-demo/app.html?algorithm=NaiveNUTS&target=banana) [[2]](#ref-2)
