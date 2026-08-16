
# ok so if you now use this to get deaths you should be able to get the output out
# looks good !
library(gnm)
m_sub <- gnm(death_updated ~ newbasis,
             data = x1,
             family = quasipoisson,
             eliminate = factor(strata))

# coefficients look pretty close
cbind(beta, coef(m_sub))

# how does crosspred look
cp <- dlnm::crosspred(newbasis,
                m_sub,
                cen = min(exposure_timeseries),
                by = 0.5)

tail(cp$matRRfit)
tail(cp$matRRlow)
tail(cp$matRRhi)

# pretty darn close !
dlnm::plot.crosspred(cp)

# and this also looks good
dlnm::plot.crosspred(cp, "overall", ci = 'area',ci.level = 0.95)
