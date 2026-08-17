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

x1 <- temp_data[
  x1, on = c('date', 'city', 'year')
]

x1

plot(x1$date, x1$tmaxF)
