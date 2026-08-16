# -----------------------------------------------------------------------------
# /////////////////////////////////////////////////////////////////////////////
# Code for ZP for Weekly Flooding
# doing conditional Quasi-Poisson on week, two-stage
# with sliding strata defined as in the RParks pre-print
#
# ToDo:
#   - HOW TO HANDLE DOUBLY EXPOSED DURING OVERLAPS?
#
# /////////////////////////////////////////////////////////////////////////////
# -----------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gnm)
library(splines)
library(future)
library(future.apply)
library(patchwork)
library(sf)
library(tigris)
library(zoo)
library(progressr)
library(dlnm)


set.seed(123)
plan(multisession)
handlers(global = TRUE)
handlers("progress")

# -----------------------------------------------------------------------------
# /////////////////////////////////////////////////////////////////////////////
# CREATE SHAPEFILES AND REGIONS
# /////////////////////////////////////////////////////////////////////////////
# -----------------------------------------------------------------------------

# source https://www.mass.gov/info-details/massgis-data-2020-us-census-towns#downloads
ma_towns <- sf::read_sf("data-raw/MA_cities_towns/CENSUS2020TOWNS_SHP/CENSUS2020TOWNS_POLY.shp")

ggplot(ma_towns) + geom_sf()

ggplot(ma_towns) + geom_sf(aes(fill = COUNTY20))

usethis::use_data(ma_towns, overwrite = T)

town_mapping <- ma_towns %>% st_drop_geometry() %>%
  select(TOWN20, COUNTY20)


# counties
# https://www.mass.gov/info-details/massgis-data-counties#downloads


library(dplyr)

ma_counties <- ma_towns |>
  select(-CNECTAFP20, -NECTAFP20, -NCTADVFP20) |>
  group_by(COUNTY20) |>
  summarise(
    across(where(is.numeric), median, na.rm = TRUE),
    across(where(is.character), ~ names(which.max(table(.x)))),
    geometry = st_union(geometry),
    .groups = "drop"
  )

ma_counties

usethis::use_data(ma_counties, overwrite = T)


# -----------------------------------------------------------------------------
# /////////////////////////////////////////////////////////////////////////////
# CREATE EXPSOURE DATA
# /////////////////////////////////////////////////////////////////////////////
# -----------------------------------------------------------------------------
# expsoure data
exposure1 <- readRDS("data-raw/prism_zcta_2010_2023_MA.Rds")
x351 <- unique(exposure1$ZCTA5CE10)[1:351]
map1 <- unique(ma_towns$TOWN20)
names(map1) = x351

exposure1$TOWN20 <- map1[exposure1$ZCTA5CE10]
exposure1 <- exposure1 %>% filter(!is.na(TOWN20)) %>%
  filter(year %in% 2010:2020) %>% arrange(TOWN20) %>%
  select(-ZCTA5CE10, -tmin_C, -tmean_C, -ppt, -year, -month, -day)

names(exposure1)[1] <- 'date'
head(exposure1)
ma_exposure <- exposure1

# values can be both missing and NA, include both
set.seed(1234)
rr <- sample(1:nrow(ma_exposure), size = 0.02 * nrow(ma_exposure), replace = F)
ma_exposure <- ma_exposure[-rr, ]

rr <- sample(1:nrow(ma_exposure), size = 0.01 * nrow(ma_exposure), replace = F)
ma_exposure$tmax_C[rr] <- NA

ma_exposure <- left_join(ma_exposure, town_mapping)

# remove TYRINGHAM
ma_exposure <- subset(ma_exposure, TOWN20 != 'TYRINGHAM')

usethis::use_data(ma_exposure, overwrite = T)

exp_mat <- make_exposure_matrix(ma_exposure)


# -----------------------------------------------------------------------------
# /////////////////////////////////////////////////////////////////////////////
# CREATE SPATIALLY CORRELATED BETAS
# with a little help from the Oracle
# /////////////////////////////////////////////////////////////////////////////
# -----------------------------------------------------------------------------

get_sub_populations <- function(RR_max, pop_denom, xseed = 4) {
  
  library(RWmisc)
  
  ma_centroids <- sf::st_centroid(ma_towns)
  ma_centroids <- projectUTM(ma_centroids)
  XY <- st_coordinates(ma_centroids)
  N <- nrow(XY)
  
  # --- helper: Matérn covariance (ν = 1.5; smooth but not too smooth) ----------
  matern32 <- function(d, rho) {
    # ν = 3/2 Matérn: (1 + sqrt(3) d/rho) exp(-sqrt(3) d/rho)
    a <- sqrt(3) * d / rho
    (1 + a) * exp(-a)
  }
  
  # --- build covariance matrices for two fields (different ranges) -------------
  D <- as.matrix(dist(XY))
  dim(D)
  
  # process variance
  sigma2_g <- 100
  
  # nugget (measurement noise)
  tau2_g   <- 0.1
  
  # spatial range (larger => smoother over space)
  rho_g    <- 100000/2
  
  # build the covariance matrix
  # add a tiny jitter for numerical stability if needed
  Cg <- sigma2_g * matern32(D, rho_g) + diag(tau2_g, N) + diag(1e-10, N)
  
  # --- simulate Gaussian random fields via Cholesky ----------------------------
  Lg <- chol(Cg)
  
  # 4 is ok, 23 is ok
  set.seed(xseed)
  zg <- as.numeric(t(Lg) %*% rnorm(N))
  
  # map to [0,1] (optional; keeps the "feel" of your original scales)
  ## green  <- (zg - min(zg)) / diff(range(zg))
  beta <- scales::rescale(zg, to = c(1.0, RR_max))
  
  ma_towns$RR_true <- beta
  
  # ggplot(ma_towns) + geom_sf(aes(fill = RR_true)) + scale_fill_viridis_c()
  
  # -----------------------------------------------------------------------------
  # /////////////////////////////////////////////////////////////////////////////
  # CREATE DUMMY CASE DATA
  # /////////////////////////////////////////////////////////////////////////////
  # -----------------------------------------------------------------------------
  
  # set cases based on a year trend
  set_cases <- function(baseline = 100,
                        variance = baseline * 0.05,
                        baseline_yr = 2010,
                        year_beta = 1.2,
                        RR = 2) {
    
    # make the skeleton you need later
    get_summer_dt <- function(yy) {
      st = make_date(yy, 5, 1)
      ed = make_date(yy, 9, 30)
      dt = seq.Date(st, ed, by = 'day')
      return(dt)
    }
    
    all_dt <- lapply(2010:2020, get_summer_dt)
    
    all_dt <- do.call(c, all_dt)
    
    df <- data.frame(date = all_dt)
    
    df$year <- year(df$date)
    
    df <- df %>% arrange(date)
    df_l <- split(df, f = df$year)
    n_years <- length(df_l)
    
    for(yr_i in 1:n_years) {
      
      # *****
      # yr_i = 1
      # *****
      
      n_rows <- nrow(df_l[[yr_i]])
      
      # need to work in log-space so the coefficients work out
      df_l[[yr_i]]$death_true =
        # baseline + how much to increase by year
        baseline * (year_beta)^(df_l[[yr_i]]$year - baseline_yr)
      
      # add some random noise
      v1 <- sample(c(-1, 0, 1), size = n_rows, replace = T)
      v2 <- rpois(n_rows, variance)
      df_l[[yr_i]]$v1 <- v1
      df_l[[yr_i]]$v2 <- v2
      df_l[[yr_i]]$death = df_l[[yr_i]]$death_true + v1 * v2
      
      # make sure its an integer
      df_l[[yr_i]]$death <- round(df_l[[yr_i]]$death)
      
      
    }
    
    df <- do.call(rbind, df_l)
    
    df$RR <- RR
    
    # p1 <- ggplot(df) +
    #   geom_point(aes(x = date, y = death))
    #
    # print(p1)
    
    return(df)
    
  }
  
  set.seed(123)
  city_case_data <- lapply(1:nrow(ma_towns), \(i) {
    set_cases(baseline = max(round(ma_towns$POP2020[i]/pop_denom), 1),
              year_beta = runif(1, 0.95, 1.05),
              RR = ma_towns$RR_true[i])
  })
  
  for(i in 1:nrow(ma_towns)) {
    city_case_data[[i]]$TOWN20 <- ma_towns$TOWN20[i]
  }
  
  city_case_data <- do.call(rbind, city_case_data)
  head(city_case_data)
  
  
  
  #' ===========================================================================
  #' Alright so now you need to do some of the same stuff you did for Lucy
  #' To come up with the coefficients
  #' So define a shape and a confidence interval
  #' based on the RR_true
  #' then reverse out the vcov
  #' will this work for the orig cross-basis? doesn't have to be perfect
  #' just has to be ok
  #' ===========================================================================
  
  town_names <- unique(city_case_data$TOWN20)
  town_data_updated <- vector("list", length(town_names))
  
  get_updated_death_data <- function(i) {
    
    # so first, make the shape and confidence interval
    this_town_name = town_names[i]
    cat(this_town_name, '\t')
    
    this_town_exposure <- subset(exp_mat, TOWN20 == this_town_name)
    if(nrow(this_town_exposure) == 0) return(NULL)
    this_town_data <- subset(city_case_data, TOWN20 == this_town_name)
    if(nrow(this_town_data) == 0) return(NULL)
    
    temp_range <- range(this_town_exposure$tmax_C)
    temp_range[1] <- floor(temp_range[1])
    temp_range[2] <- ceiling(temp_range[2])
    temp_range
    seq_temp_range = temp_range[1]:temp_range[2]
    seq_temp_range
    
    RR_range   <- c(1, this_town_data$RR[1])
    RR_range
    
    # vertex is the low point
    h = temp_range[1]
    k = log(RR_range[1])
    x = temp_range[2]
    y = log(RR_range[2])
    
    #  vertx form of the quadratic equation y=a(x-h)^{2}+k
    # i have the vertex
    a = (y - k) / (x - h)^2
    getlogy = function(xx) a*(xx - h)^2 + k
    
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
    x_knots = quantile(this_town_exposure$tmax_C, probs = c(0.5, 0.9))
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
    x_mat <- this_town_exposure[,c('tmax_C', paste0('Templag',1:maxlag))]
    head(x_mat)
    if(any(is.na(x_mat))) stop()
    
    x_cen = which.min(x_mat$tmax_C)
    x_cen
    
    origbasis <- crossbasis(x_mat, maxlag, argvar, arglag)
    dim(origbasis)
    
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
    this_town_data$dow <- lubridate::wday(this_town_data$date, label = T)
    this_town_data$month <- lubridate::month(this_town_data$date, label = T)
    this_town_data$year <- lubridate::year(this_town_data$date)
    
    this_town_data$strata <- paste0(this_town_data$TOWN20, ":", this_town_data$year, ":",
                                    this_town_data$month, ":", this_town_data$dow)
    
    # this actually needs to be on DEATH not RRfit
    death_baseline <- this_town_data$death
    
    # and this should be roughly determined by the quadratic above
    # lets assign some weights
    ww = c(1, 0.7, 0.4, 0.3, 0.2, 0.05) / 1.5
    #ww = c(1,   ,   0,   0,   0,    0) / 1
    #
    death_updated <- death_baseline
    for(i in 1:length(death_updated)) {
      death_updated[i] = round(death_baseline[i] * exp(sum(getlogy(x_mat[i, ]) * ww)))
    }
    this_town_data$death_updated <- death_updated
    # so now,
    
    # ok so if you now use this to get deaths you should be able to get the output out
    # looks good !!
    # library(gnm)
    # m_sub <- gnm(death_updated ~ newbasis,
    #              data = this_town_data,
    #              family = quasipoisson,
    #              eliminate = factor(strata))
    #
    #
    # cp <- crosspred(newbasis,
    #                 m_sub,
    #                 cen = temp_range[1],
    #                 by = 0.05)
    #
    # plot(cp, "overall", main = this_town_name)
    
    return(this_town_data)
  }
  
  my_fcn <- function(p_all) {
    p <- progressor(along = p_all)
    xx <- future_lapply(p_all, function(i) {
      p(sprintf("x=%s", town_names[i]))
      get_updated_death_data(i)
    })
    return(xx)
  }
  
  # town_data_updated <- my_fcn(1)
  
  town_data_updated <- my_fcn(1:351)
  
  # for(i in 1:351) {
  #   xx <- get_updated_death_data(i)
  # }
  
  town_data_updated <- do.call(rbind, town_data_updated)
  
  # now clean and save
  town_data_updated <- town_data_updated %>%
    select(date, TOWN20, death_updated) %>%
    rename(daily_deaths = death_updated)
  
  row.names(town_data_updated) <- NULL
  
  return(town_data_updated)
}

mage1 <- get_sub_populations(RR_max = 1.42, pop_denom = 2000, xseed = 4)
mage1$age_grp <- '0-17'
mage1$sex <- 'M'

fage1 <- get_sub_populations(RR_max = 1.38, pop_denom = 2000, xseed = 23)
fage1$age_grp <- '0-17'
fage1$sex <- 'F'

mage2 <- get_sub_populations(RR_max = 1.12, pop_denom = 2000, xseed = 23)
mage2$age_grp <- '18-64'
mage2$sex <- 'M'

fage2 <- get_sub_populations(RR_max = 1.13, pop_denom = 2000, xseed = 4)
fage2$age_grp <- '18-64'
fage2$sex <- 'F'

mage3 <- get_sub_populations(RR_max = 1.25, pop_denom = 2000, xseed = 4)
mage3$age_grp <- '65+'
mage3$sex <- 'M'

fage3 <- get_sub_populations(RR_max = 1.28, pop_denom = 2000, xseed = 23)
fage3$age_grp <- '65+'
fage3$sex <- 'F'

ma_deaths <- rbind(mage1, mage2, mage3, fage1, fage2, fage3)

# now randomly choose a small % of days to remove
# lets say , 10% of days where counts are < 5
set.seed(12345)
xrr <- which(ma_deaths$daily_deaths < 5)
rr <- sample(xrr, size = round(0.1 * length(xrr)), replace = F)
length(rr) / nrow(ma_deaths)
ma_deaths <- ma_deaths[-rr, ]

ma_deaths <- left_join(ma_deaths, town_mapping)

## remove any that are < 0
xrr <- which(ma_deaths$daily_deaths < 0)
ma_deaths <- ma_deaths[-xrr, ]

usethis::use_data(ma_deaths, overwrite = T)
