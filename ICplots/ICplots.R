for (j in c(1, 1000, 10000)){
  set.seed(j)
  c       = 0.5
  a       = 0.5
  mu      = 0
  sigma   = 4
  N       = 1000
  epsilon = rnorm(N, mu, sigma)
  y       = c + epsilon[-1] + a * epsilon[-length(epsilon)]
  LogLik  = c()
  aic     = c()
  bic     = c()
  for (i in 1:15){
    model     = arima(y, order = c(i,0,0), include.mean = F)
    LogLik[i] = model$loglik
    aic[i]    = AIC(model, k = 2)
    bic[i]    = BIC(model)
  }

  dev.new()
  par(mfrow = c(3,1))
  plot(LogLik, xlab = "Number of parameters", ylab = "LogLik", lwd = 5, 
    pch = 19, xaxt = "n", yaxt = "n")
  axis(1, cex.axis = 1.5)
  axis(2, cex.axis = 1.5)
  plot(aic, xlab = "Number of parameters", ylab = "AIC", lwd = 5, 
    pch = 19, xaxt = "n", yaxt = "n")
  axis(1, cex.axis = 1.5)
  axis(2, cex.axis = 1.5)
  plot(bic, xlab = "Number of parameters", ylab = "BIC", lwd = 5, 
    pch = 19, xaxt = "n", yaxt = "n")
  axis(1, cex.axis = 1.5)
  axis(2, cex.axis = 1.5)
}
