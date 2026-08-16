# set cases based on a year trend
get_baseline_cases <- function(baseline = 100,
                               variance = 0.05,
                      baseline_yr = 2010,
                      year_beta = 1.2) {
  
  
  variance = baseline * variance
  
  # make the skeleton you need later
  get_summer_dt <- function(yy) {
    st = lubridate::make_date(yy, 5, 1)
    ed = lubridate::make_date(yy, 9, 30)
    dt = seq.Date(st, ed, by = 'day')
    return(dt)
  }
  
  all_dt <- lapply(2010:2020, get_summer_dt)
  all_dt <- do.call(c, all_dt)
  
  df <- data.frame(date = all_dt)
  df$year <- lubridate::year(df$date)
  
  df_l <- split(df, f = df$year)
  n_years <- length(df_l)
  
  for(yr_i in 1:n_years) {
    
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
  
  return(df[, c('date', 'death')])
  
}

x1 <- get_baseline_cases()
head(x1)
Ndays <- nrow(x1)
plot(x1$date, x1$death)


x1$dow <- lubridate::wday(x1$date, label = T)
x1$month <- lubridate::month(x1$date, label = T)
x1$year <- lubridate::year(x1$date)

x1$strata <- paste0(x1$TOWN20, ":", x1$year, ":",
                    x1$month, ":", x1$dow)

