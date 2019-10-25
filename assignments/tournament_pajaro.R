datos <- read.csv("~/forecasting-course-short/data/sanjoaquin_forage_train_set.csv")
covs <- read.csv("~/forecasting-course-short/data/tourn.csv")


X = matrix(NA, dim(datos)[1], 6 )

for(i in 1: dim(datos)[1]){
  tmp <- which(covs$yr == datos$year[i])
  X[i, ] <- colSums(covs[tmp,3:8])
}

yrs = 2009:2018

Xp = matrix(NA, 10, 6)
for(i in 1: 10){
  tmp <- which(covs$yr == yrs[i])
  Xp[i, ] <- colSums(covs[tmp,3:8])
}


y = datos$lbs_per_acre
df = cbind(y,X)
library(brms)

fit_hs <- brm(y ~ . , 
              data = df, 
              prior = prior(horseshoe(df = 1), class = "b"), 
              iter = 1000, chains = 3,
              control = list(adapt_delta = 0.999, max_treedepth = 15))

# betas_hs = fixef(fit_hs)

nsim = 1000 

pos <- posterior_samples(fit_hs)
pre = matrix(NA,nsim, 10)

for(i in 1: nsim){

    best_guess = pos$b_Intercept[i] +
      pos$b_V2[i] * Xp[,1] + 
      pos$b_V3[i] * Xp[,2] + 
      pos$b_V4[i] * Xp[,3] + 
      pos$b_V5[i] * Xp[,4] + 
      pos$b_V6[i] * Xp[,5] + 
      pos$b_V7[i] * Xp[,6]

    pre[i,] = rnorm(10, best_guess, pos$sigma[i]) 
  }


library(coda)
CI <- HPDinterval(as.mcmc(pre))
mp <- colMeans(pre)

plot(c(datos$year, yrs), c(datos$lbs_per_acre, mp), type = "l", 
     ylim = c(100,5000),xlab = "year", ylab = "log N")

lines(yrs, mp, col = "red", lty = "solid")
lines(yrs, CI[, 1], col = "red", lty = "dashed")
lines(yrs, CI[, 2], col = "red", lty = "dashed")

prediction = cbind(mp, CI)