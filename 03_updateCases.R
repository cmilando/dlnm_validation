
# because you are passing in the whole matrix, you don't need to
# group or anything
x_cen = which.min(exposure_timeseries)
x_cen

origbasis <- dlnm::crossbasis(exposure_timeseries,
                        argvar = list(fun = 'ns', knots = exp_knots),
                        arglag = list(fun = 'ns', knots = 2),
                        lag = 5)
# dim(origbasis)
# 
# x_mat <- do.call(cbind, lapply(0:maxlag, \(x) dplyr::lag(exposure_timeseries, x)))
# 
# # so create the true crossbasis
# 
# # get a recentered basis
# # origbasis <- onebasis(x = x_values, fun = x_fun, knots = x_knots,
# #                       degree = x_degree, intercept = x_intercept,
# #                       Boundary.knots = x_Boundary)
# 
basiscen <- origbasis[x_cen, ]

newbasis <- scale(origbasis, center = basiscen, scale = FALSE)
dim(newbasis)
# 
# # get coefficients
# # hmm you can't do this here because you dont have logRRfit for every day
# # and you dont have that for every day because you dont have the lag values
# # for every temperature
# # but I suppose these is where you would put it in
# # if you can create a logRRfit object
# # that is what is being estimated
# 
# # is there a system of equations here for this?
# 
# # switch to modified
# 
# # this actually needs to be on DEATH not RRfit
# death_baseline <- x1$death
# 
# # and this should be roughly determined by the quadratic above
# # lets assign some weights
# ww = c(1, 0.7, 0.4, 0.3, 0.2, 0.05) / 1.5
#ww = c(1,   ,   0,   0,   0,    0) / 1
#
set.seed(12345)
death_updated <- numeric(nrow(x1))
options(warn = 1)
for(i in 1:length(death_updated)) {
  # so first get the expected value
  # E[y] = A * exp(B0 + B1*cb1 + ...)
  deaths_expected_value = x1$death[i] * exp(sum(newbasis[i, ] * beta))
  #print(deaths_expected_value)
  # then use this in a poisson draw
  # governed by  .... variance and dispersion ??
  death_updated[i] = rpois(1, deaths_expected_value)
  # death_updated[i] = round(deaths_expected_value)
}
x1$death_updated <- death_updated
# so now,



