# get a temperature timeseries

# https://psl.noaa.gov/data/timeseries/daily/
# - station Data
# - station ID 1044
# - daily Maximum temperature
# - 1980 - 1999
# - Mean
# - Jan - Dec

# https://psl.noaa.gov/cgi-bin/data/timeseries/getdaily.pl?typeTS=1&tstype=1&id=1044&variable=tmax&variablencep=None&leveln=2000&variabletwentcv3=None&levelt3=2000&variabletwentcv2=None&levelt=2000&variablenclim=None&variableera5=None&levelera5=2000&lat1=&lon1=&mon1=1&mon2=12&ipos%5B1%5D=1980&ipos%5B2%5D=1999&anom=0&gettimevar=get_timeseries&ranktype=2&number=0&value=0&typeval=1&highlow=1&lineORbar=line&use0ref=no&Submit=Get+Dates%2FTimeseries

temp_data <- read.table("getdailystat73.149.185.213.228.6.19.51", 
                               sep = ",", colClasses = c('numeric', 
                                                         rep('character', 3)))
head(temp_data)
names(temp_data) <- c('tmaxF', 'year', 'month', 'day')

library(data.table)
setDT(temp_data)


temp_data[, dtstr := paste0(trimws(year), "-", 
                                   trimws(month), "-", 
                                   trimws(day))]
temp_data[, date := as.IDate(dtstr)]
temp_data[, month := NULL]
temp_data[, day := NULL]
temp_data[, dtstr := NULL]
temp_data[, year := year(date)]
temp_data$city = 'BOSTON'

temp_data

# x1 <- temp_data[
#   x1, on = c('date', 'city', 'year')
# ]
# 
# x1

# plot(x1$date, x1$tmaxF)

## ok well now model this so you can control this too
library(splines)
temp_basis <- dlnm::onebasis(temp_data$tmaxF,
                       fun = 'ns', df = 7 * 19)
dim(temp_basis)

temp_data$yday = yday(temp_data$date)
temp_data$yr = year(temp_data$date)

temp_m1 <- lm(tmaxF ~ 
                ns(yday, df = 4) + 
                ns(yr, df = 2), data = temp_data)
summary(temp_m1)
temp_pred = predict(temp_m1)
temp_data$predT = temp_pred
temp_data$idx = 1:nrow(temp_data)

# now also fit a sine curve to this
A = (max(temp_pred) - min(temp_pred))/2 # amplitude
B = 2*pi/365 # 2 pi / period
D = (max(temp_pred) + min(temp_pred))/2 # vertical shift
C = 4.44 # phase shift
year_growth = 0.01
daily_growth = (1 + year_growth)^(1/365) - 1
sinTemp = (A * sin(B*temp_data$idx + C) + D) * exp(daily_growth * temp_data$idx)
sinTempRand = sapply(sinTemp, \(x) runif(n = 1, min = x - 20, max = x + 20))
sinTempRand

plot(temp_data$date, temp_data$tmaxF,
     xlim = c(temp_data$date[1], temp_data$date[1000])) 

lines(temp_data$date, temp_data$predT, col = 'red')
lines(temp_data$date, sinTemp, col = 'blue')
lines(temp_data$date, sinTempRand, col = 'green')
