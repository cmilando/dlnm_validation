source("00_fcns.R")
library(dlnm)

cb1.pm <- crossbasis(chicagoNMMAPS$pm10, lag=15, argvar=list(fun="lin"),
                       arglag=list(fun="poly",degree=4))

cb1.temp <- crossbasis(chicagoNMMAPS$temp, lag=3, argvar=list(df=5),
                         arglag=list(fun="strata",breaks=1))

library(splines)

model1 <- glm(death ~ cb1.pm + cb1.temp + ns(time, 7*14) + dow,
                family=quasipoisson(), chicagoNMMAPS)

pred1.temp <- crosspred(cb1.temp, model1, at=0:20, bylag=1, cumul=TRUE)

pred1.temp_local <- local_cp(cb1.temp, model1, at=0:20, bylag=1, cumul=TRUE)

plot(pred1.temp_local)

plot(pred1.temp, "overall")

plot(pred1.temp, "slices", var=20, ci="n", col=1, lwd=1.5,
       main="Lag-response curves for different temperatures, ref. 21C")

for(i in 1:3) lines(pred1.temp, "slices", var=c(5,10, 15)[i], col=i+1, lwd=1.5)


Xinv <- MASS::ginv(pred1.temp_local$Xpred_orig)
dim(Xinv)
dim(matrix(xcoef, ncol = 1))
xcoef <- pred1.temp$coefficients

dim(pred1.temp$matfit)

# right so there is s sneaky little reshape that happens here
# RIGHT this is how this is done, not with STAN
# so you need to reshape 
pred1.temp_local$Xpred_orig

matrix(pred1.temp_local$Xpred_orig %*% matrix(xcoef, ncol = 1),
       ncol = 4)

pred1.temp$matfit

# there we go
# ok
Xinv %*% matrix(pred1.temp$matfit, ncol = 1)
matrix(xcoef, ncol = 1)

# so you need to reshape matfit into a single column
# the zero there is the minimum value ...... ......

