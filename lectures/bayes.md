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

Para calcular la probabilidad marginal de los datos $y$ hacemos $\int P(y| \theta) d \theta$

En general, no podemos resolver estas integrales para los modelos que queremos ajustar (e.g. para una regresión simple con una ordenada al origen, una pendiente y un parámetro de varianza, tenemos que resolver una integral triple). Lo que hacemos es usar métodos de Markov Chain Monte Carlo para generar muestras de la distribución posterior.


Los enlaces de abajo muestran animaciones (hechas por Chi Feng) de algunos métodos de MCM

*   [Random Walk Metropolis Hastings](https://chi-feng.github.io/mcmc-demo/app.html?algorithm=RandomWalkMH&target=banana)
*   [Adaptive Metropolis Hastings](https://chi-feng.github.io/mcmc-demo/app.html?algorithm=AdaptiveMH&target=banana) [[1]](#ref-1)
*   [Hamiltonian Monte Carlo](https://chi-feng.github.io/mcmc-demo/app.html?algorithm=HamiltonianMC&target=banana) [[2]](#ref-2)
*   [No-U-Turn Sampler](https://chi-feng.github.io/mcmc-demo/app.html?algorithm=NaiveNUTS&target=banana) [[2]](#ref-2)
