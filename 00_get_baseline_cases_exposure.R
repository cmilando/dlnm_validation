library(data.table)

# set cases based on a year trend
get_baseline_cases <- function(
    baseline,
    sd,
    year_growth,
    dow_effect,
    city,
    baseline_yr = 1980,
    end_yr = 1999
) {

  set.seed(123)
  
  # make the skeleton you need later
  get_year_dt <- function(yy) {
    st = lubridate::make_date(yy, 1, 1)
    ed = lubridate::make_date(yy, 12, 31)
    dt = seq.Date(st, ed, by = 'day')
    return(dt)
  }
  
  all_dt <- lapply(baseline_yr:end_yr, get_year_dt)
  all_dt <- do.call(c, all_dt)
  
  # make dt
  df <- data.table(date = as.IDate(all_dt), city = city)
  df[,year := year(all_dt)]
  df[,dow := wday(all_dt)]
  df[,month := month(all_dt)]
  df[,idx := 1:nrow(df)]
  
  # add year trend but compounded daily
  # (1 + Growth Rate)^(1/365)-1
  daily_growth = (1 + year_growth)^(1/365) - 1
  df[,death := baseline * exp(daily_growth * idx)]
  
  # add day of week trend
  dow_sine <- function(x) {
    A = dow_effect # amplitude
    B = 2*pi/7 # frequency
    C = 0 # phase shift
    D = 1 # vertical shift
    A * sin(B*x + C) + D
  }
  # plot(dow_sine(1:7), type = 'l')
  # points(dow_sine(1:7))
  df[,death := death * dow_sine(idx)]
  
  # add noise based on sd
  # applied in log space to make sure its positive
  noise = rnorm(nrow(df), sd = sd)
  df[, death := exp(log(death) + noise)]
  
  # round to integer
  df[, death := round(death)]
  
  # add strata
  df[, strata := paste0(city, ":", year, ":", month, ":", dow)]

  return(df)
  
}

x1 <- get_baseline_cases(baseline = 1000, 
                         sd = 0.01,
                         year_growth = 0.02,
                         dow_effect = 0.05,
                         city = 'BOSTON')

head(x1)
Ndays <- nrow(x1)
plot(x1$date, x1$death)



