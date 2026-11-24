library(data.table)
library(ggplot2)

set.seed(123)

# ============================================================
# Set up the TIME SERIES
# ============================================================

dt <- CJ(
  area = 1:1,
  year = 2010:2014,
  day = 1:365
)

dt[, date := as.IDate(paste(year, "01", "01", sep = "-")) + day - 1]

dt[, year  := year(date)]
dt[, month := month(date)]
dt[, dow   := weekdays(date)]

# Join area-level variables
# dt <- area_dt[dt, on = "area"]

# ============================================================
# TIME-VARYING EXPOSURE X
# ============================================================

# Seasonal exposure + daily noise
dt[, X :=  20 + 10 * sin(2 * pi * day / 365) + rnorm(.N, 0, 3)]

# plot
ggplot(subset(dt, area == 1)) +
  geom_line(aes(x = date, y = X))

dt[, gt30 := X >= 30]
head(dt)

# ============================================================
# OUTCOME - NO AC RELATIONSHIP
# ============================================================

# Stratum-specific baseline
dt[, alpha := 100]

ggplot(subset(dt, area == 1)) +
  geom_line(aes(x = date, y = alpha))

# 
beta_true = log(2)

# Expected outcome
# log Y = b0 + b1*x + ....
dt[, mu := alpha * exp(beta_true * gt30)]

# Outcome
dt[, Y := rpois(.N, mu)]

# plot
ggplot(subset(dt, area == 1)) +
  geom_line(aes(x = date, y = Y))

# 
library(gnm)
fit0 <- gnm(Y ~ gt30, family = quasipoisson(), data = dt)
summary(fit0)
exp(coef(fit0))


# ============================================================
# OUTCOME - has modification by AC 
# ============================================================

# add an AC relationship
dt[, AC_prev := (year - 2009)/6]
head(dt)

ggplot(subset(dt, area == 1)) +
  geom_line(aes(x = date, y = AC_prev))

# Stratum-specific baseline
dt[, alpha := 100]

ggplot(subset(dt, area == 1)) +
  geom_line(aes(x = date, y = alpha))

# 
beta_true = log(2 - dt$AC_prev)

# Expected outcome
# log Y = b0 + b1*x + ....
dt[, mu := alpha * exp(beta_true * gt30)]

# Outcome
dt[, Y := rpois(.N, mu)]

# plot
ggplot(subset(dt, area == 1)) +
  geom_line(aes(x = date, y = Y))

# for GRACE -- what is the right way to model this in stage 1
library(gnm)
fit0 <- gnm(Y ~ gt30 * AC_prev, family = quasipoisson(), data = dt)
summary(fit0)
exp(coef(fit0))




