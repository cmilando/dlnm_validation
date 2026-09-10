library(data.table)
library(ggplot2)

set.seed(123)

# Quasi-DAG showing whats happening in a 2-stage context
# where _i indicates the area-level
#      Z
#     / \
#    ↓   ↓
#   C    β_i
#        |
#        │ modifies
#        ↓
#  X ─────────→ Y

# ============================================================
# PARAMETERS
# ============================================================

n_area <- 100
n_year <- 5
n_days <- 365

beta0  <- log(1.02)      # baseline effect of X
beta_Z <- log(1.05)      # true modification by Z

rho_CZ <- 0.60           # correlation between C and Z

sigma_beta <- 0.03       # unexplained between-area heterogeneity

# ============================================================
# AREA-LEVEL VARIABLES
# ============================================================

# create the aera database 
area_dt <- data.table(area = 1:n_area, Z = rnorm(n_area))

# C correlated with Z, but C has NO causal role (next step)
# so the larger rho_CZ is, the higher the correlation
area_dt[, C := rho_CZ * Z + sqrt(1 - rho_CZ^2) * rnorm(.N)]

# True area-specific X effect
# IMPORTANT because it has no C term
area_dt[, beta_true := beta0 + beta_Z * Z + rnorm(.N, 0, sigma_beta)]

# ============================================================
# Set up the TIME SERIES
# ============================================================

dt <- CJ(
  area = 1:n_area,
  year = 2010:(2010 + n_year - 1),
  day = 1:n_days
)

dt[, date := as.IDate(paste(year, "01", "01", sep = "-")) + day - 1]

dt[, month := month(date)]
dt[, dow   := weekdays(date)]

# Join area-level variables
dt <- area_dt[dt, on = "area"]

# ============================================================
# TIME-VARYING EXPOSURE X
# ============================================================

# Seasonal exposure + daily noise
dt[, X :=  20 + 10 * sin(2 * pi * day / 365) + rnorm(.N, 0, 3)]

# plot
ggplot(subset(dt, area == 1)) +
  geom_line(aes(x = date, y = X))

# ============================================================
# CASE-CROSSOVER STRATA
# ============================================================

dt[, stratum := paste(area, year, month, dow, sep = ":")]

# ============================================================
# OUTCOME
# ============================================================

# Stratum-specific baseline
dt[, alpha := rnorm(.N, 0, 0.2), by = stratum]

# Expected outcome
dt[, mu := exp(alpha + beta_true * X)]

# Outcome
dt[, Y := rpois(.N, mu)]

# plot
ggplot(subset(dt, area == 1)) +
  geom_line(aes(x = date, y = Y))

# create stratum totals
dt[, stratum_total := sum(Y), by = stratum]

# ============================================================
# List of Stage 1 models
# ============================================================

library(gnm)

stage1 <- dt[, {
  
  fit <- gnm(
    Y ~ X,
    eliminate = factor(stratum),
    family = quasipoisson(),
    data = .SD,
    keep = stratum_total > 0
  )
  
  b <- coef(fit)["X"]
  se <- sqrt(vcov(fit)["X", "X"])
  
  .(beta_hat = b,
    se = se)
  
}, by = area]

# Adding C and Z
stage1 <- area_dt[stage1, on = "area"]

# forest plot
ggplot(stage1) +
  geom_pointrange(aes(x = exp(beta_hat), 
                      xmin = exp(beta_hat - qnorm(0.975) * se),
                      xmax = exp(beta_hat + qnorm(0.975) * se),
                      y = factor(1),
                      group = area),
                  shape = 1, size = 0.25,
                  position = 'jitterdodge')

# ============================================================
# Meta-regression
#
# 1. showing how C looks if you don't adjust for Z
#
# 2. showing adjustment for C and Z
# 
# 3. showing just adjustment by Z
#
# ============================================================

library(mixmeta)

# 1. meta regression if you don't take into account Z
m_C <- mixmeta(beta_hat ~ C, S = se^2, data = stage1)

# this shows that C is significant
summary(m_C)


# 2. meta regression if you do take Z into account
m_CZ <- mixmeta(beta_hat ~ C + Z, S = se^2, data = stage1)

# this shows that C is actually not significant
# and Z is pretty close to the true of log(1.05)
summary(m_CZ)


# 3. meta regression if you do take Z into account
m_Z <- mixmeta(beta_hat ~ Z, S = se^2, data = stage1)

# the Z coefficient here is very similar to meta-regression 2 above
# pretty close to the true of log(1.05)
summary(m_Z)


