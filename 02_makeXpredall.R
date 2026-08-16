# ok now make Xpred

x = seq(0, 30, by = 0.5)
l = seq(0, 5, by = 1)

xpred_base = tidyr::expand_grid(x= x, l = l)
xpred_base = xpred_base[, c("x", "l")]
setDT(xpred_base)

setorderv(xpred_base, "x")
xpred_base

# ok now each gets a onebasis
exp_knots = quantile(exposure_timeseries, probs = c(.5, .9))z

exposure_basis = onebasis(xpred_base$x, 
                          fun = 'ns',
                          knots = exp_knots)
# exposure_basis <- as.data.table(exposure_basis)
# exposure_basis$x = xpred_base$x
# exposure_basis$l = xpred_base$l
# exposure_basis

maxlag = max(l)
lag_basis = onebasis(xpred_base$l,
                     fun = 'ns',
                     knots = )

cp_basis = crossbasis(
  x = xpred_base$x, 
  argvar = list(fun = 'ns', knots = exp_knots),
  arglag = list(fun = 'ns', knots = 2),
  lag = 5
)

dim(cp_basis)

Xpred = dlnm:::mkXpred('cb', cp_basis, 
    at = x, predvar = x, predlag = l, cen = min(x))

Xpred

# xpred_base_l = split(xpred_base, f = xpred_base$i)
# # xpred_base_l = xpred_base_l
# # dim(xpred_base_l[[1]])
# # 
# # for(i in 2:length(xpred_base_l)) {
# #   xpred_base_l[[i]] = xpred_base_l[[i]] + xpred_base_l[[i-1]]
# # }
# # 
# # xpred_all = do.call(rbind, xpred_base_l)
# # xpred_all[, c('x', 'l', 'i')] = xpred_base[, c('x', 'l', 'i')]
# # xpred_base_l = split(xpred_all, f = xpred_all$i)
# # xpred_base_l[[1]]
# # dim(xpred_base_l[[1]])
# 
# # ok sow solve for each 1x 
# cumfiti = 5
# 
# Xpred = xpred_base_l[[cumfiti]]
# xcols <- names(Xpred)[grep("^V", names(Xpred))]
# Xpred_mat <- as.matrix(Xpred[, ..xcols])
# 
# dim(Xpred_mat)
# 
# dim(RR_mat)
# dim(t(RR_mat))

RR_mat

tRR_mat <- t(RR_mat)
tRR_mat_flat <- matrix(tRR_mat, ncol = 1)
tRR_mat_flat

Xpred_mat

# ok so here its the solver step
# since this is poisson, you will need to find the 
# beta that minimize this
# exp(XpredALL[i] * Beta) = cumfit[i]
#  Beta = INVERSE(Expred[i]) * log(cumFit[i])
# no the t(RR_mat) needs to have its columns reversed
beta = MASS::ginv(Xpred) %*% log(tRR_mat_flat)
beta
# # butttt since this isn't OLS, you have to solve this here instead.
# # like which one 
# 
# # confusing is that what is enforcing all the lags to have the same Beta
# 
# # no I'm doing the wrong thing here I think right
# # because the likelihood is against the outcome
# # whereas for this is against R
# 
# # because your 3d shapes are not based on data there is no guarantee
# # that there will be a beta that matches
# # so in this step you are imputing what the closest one would be 
# # that would match the curve that you drew
# 
# # i think fine to use STAN here
# # - target is log(RR)
# library(cmdstanr)
# 
# stan_data <- list(
#   maxlag = as.integer(maxlag),
#   Xdim = as.integer(length(x)),
#   Vdim = as.integer(nrow(beta)),
#   logRRmat = log(tRR_mat),
#   XPredAll = do.call(rbind, xpred_base_l)[, ..xcols]
# )
# 
# mod <- cmdstanr::cmdstan_model("findBeta.stan")
# 
# fit_mcmc <- mod$sample(
#   data = stan_data,
#   seed = 123,
#   chains = 4,
#   parallel_chains = 4,
#   refresh = 500 # print update every 500 iters
# )
# 
# cat(" ...mcmc draws... \n")
# mcmc_array <- fit_mcmc$draws()
# 
# stan_summary <- fit_mcmc$summary()
# stan_summary
# 
# # ok now check
# rr <- grep("beta", stan_summary$variable)
# xcoef = as.matrix(stan_summary$mean[rr])
# xcoef

Xpred_mat %*% beta
rebuild_log_RRmat <- matrix(Xpred %*% beta, ncol = maxlag + 1)
head(rebuild_log_RRmat)
head(log(tRR_mat))

rebuild_RRmat = exp(rebuild_log_RRmat)
head(rebuild_RRmat)
rebuild_RRmat <- as.data.table(rebuild_RRmat)
names(rebuild_RRmat) = as.character(0:maxlag)
rebuild_RRmat$x = x
rebuild_RRmat <- melt(rebuild_RRmat, id.vars = 'x')
names(rebuild_RRmat)[2:3] <- c('l', 'RR')
rebuild_RRmat$l <- type.convert(rebuild_RRmat$l, as.is = T)
head(rebuild_RRmat)
rebuild_RRmat

ggplot(rebuild_RRmat) +
  geom_line(aes(x = x, y = RR, color = factor(l)))

ggplot(rebuild_RRmat) +
  geom_surface_3d(mapping= aes(x = x, y = l, z = RR, 
                               fill = RR)) +
  coord_3d() + scale_fill_gradient2()

