
# ok so if you now use this to get deaths you should be able to get the output out
# looks good !
library(gnm)
m_sub <- gnm(death_updated ~ newbasis,
             data = x1,
             family = quasipoisson,
             eliminate = factor(strata))

summary(m_sub)

# coefficients look pretty close
cbind(beta, coef(m_sub))

# how does crosspred look
cp <- dlnm::crosspred(newbasis,
                m_sub,
                cen = min(x1$tmaxF),
                by = 1, 
                bylag = 0.5)

xcen = cp$predvar[which.min(cp$allRRfit)]

cp <- dlnm::crosspred(newbasis,
                      m_sub,
                      cen = xcen,
                      by = 1, 
                      bylag = 0.5)

tail(cp$matRRfit)
tail(cp$matRRlow)
tail(cp$matRRhi)

# pretty darn close !
dlnm::plot.crosspred(cp)

# and this also looks good
dlnm::plot.crosspred(cp, "overall", ci = 'area', ci.level = 0.95)



# plot ts?
death_pred <- exp(predict(m_sub))
death_pred

x1$death_pred = c(rep(NA, times= maxlag), death_pred)
head(x1)

ggplot(subset(x1, year(date) < 1981)) + 
  geom_point(aes(x = date, y = death_updated)) +
  geom_line(aes(x = date, y = death_pred), col = 'red') 
