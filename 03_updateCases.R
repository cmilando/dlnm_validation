
library(dlnm)

set.seed(123)
n = Ndays
x = 1:Ndays
aa = 10
bb  = 0.2
cc = 100 
vv = 23.25- 4
ytrue = aa * sin(bb / (2*pi)* (x  + cc)) + vv
y = ytrue + rnorm(n, sd = 0.5)
plot(x, ytrue, 'l')
points(x, y, col = 'red')
abline(h = 20, col = 'brown')

exposure_timeseries <- y

#####
# so first, make the shape and confidence interval
temp_range <- vector("numeric", 2)
temp_range[1] <- 0
temp_range[2] <- 30
temp_range
seq_temp_range = temp_range[1]:temp_range[2]
seq_temp_range

RR_range   <- c(1, 2)
RR_range

# vertex is the low point
h = temp_range[1]
k = log(RR_range[1])
x = temp_range[2]
y = log(RR_range[2])

#  vertx form of the quadratic equation y=a(x-h)^{2}+k
# i have the vertex
a = (y - k) / (x - h)^2
a
getlogy = function(xx) a*(xx - h)^2 + k
plot(seq_temp_range, getlogy(seq_temp_range))

# and the confidence intervals scale with population
# lb = getlogy(seq_temp_range) * 0.75
# ub = getlogy(seq_temp_range) * 1.25
#
# plot(exp(getlogy(seq_temp_range)))
# lines(exp(ub), col = 'red')
# lines(exp(lb), col = 'red')

# just try the fisher inversion first, that should work
# get logRR fit

#
x_values = seq_temp_range
x_fun = 'bs'
x_knots = quantile(exposure_timeseries, probs = c(0.5, 0.9))
x_degree = 2
x_intercept = F
maxlag = 5
nk = 2

## natural spline
## two knots
argvar <- list(fun=x_fun, degree = x_degree,
               knots = x_knots)

arglag <- list(fun=x_fun, knots=logknots(maxlag, nk=nk))

# because you are passing in the whole matrix, you don't need to
# group or anything
x_cen = which.min(exposure_timeseries)
x_cen

origbasis <- crossbasis(exposure_timeseries, maxlag, argvar, arglag)
dim(origbasis)

x_mat <- do.call(cbind, lapply(0:maxlag, \(x) dplyr::lag(exposure_timeseries, x)))

# so create the true crossbasis

# get a recentered basis
# origbasis <- onebasis(x = x_values, fun = x_fun, knots = x_knots,
#                       degree = x_degree, intercept = x_intercept,
#                       Boundary.knots = x_Boundary)

basiscen <- origbasis[x_cen, ]

newbasis <- scale(origbasis, center = basiscen, scale = FALSE)
dim(newbasis)

# get coefficients
# hmm you can't do this here because you dont have logRRfit for every day
# and you dont have that for every day because you dont have the lag values
# for every temperature
# but I suppose these is where you would put it in
# if you can create a logRRfit object
# that is what is being estimated

# is there a system of equations here for this?

# switch to modified

# this actually needs to be on DEATH not RRfit
death_baseline <- x1$death

# and this should be roughly determined by the quadratic above
# lets assign some weights
ww = c(1, 0.7, 0.4, 0.3, 0.2, 0.05) / 1.5
#ww = c(1,   ,   0,   0,   0,    0) / 1
#
death_updated <- death_baseline
for(i in 1:length(death_updated)) {
  death_updated[i] = round(death_baseline[i] * exp(sum(getlogy(x_mat[i, ]) * ww)))
}
x1$death_updated <- death_updated
# so now,

# ok so if you now use this to get deaths you should be able to get the output out
# looks good !
library(gnm)
m_sub <- gnm(death_updated ~ newbasis,
             data = x1,
             family = quasipoisson,
             eliminate = factor(strata))


cp <- crosspred(newbasis,
                m_sub,
                cen = temp_range[1],
                by = 0.05)

plot(cp)

plot(cp, "overall")


