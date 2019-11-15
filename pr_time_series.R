# FORCASTING PARKRUN ATTENDENCE USING TIME SERIES DATA #

#===
# SETUP
#===

rm(list=ls())

library(tidyr)
library(dplyr)
library(lubridate)
library(forecast)
#install.packages("forecast")

#===
# Reading in Data
#===

setwd("C:/Users/Robert/Google Drive/Other Projects/PARKRUN/Parkrun 201920")   # home
#setwd("C:/Users/cmp16ras/Google Drive/Other Projects/PARKRUN/Parkrun 201920")  # work

# read in locations data with longitudes & lattitudes
locations <- read.csv(file = "parkrun_locations18.csv")

# read in events data with attendances for each event.
events    <- read.csv(file = "parkrun_events0418.csv")

# read in deprivation data
deprivation <- read.csv(file = "parkrun_deprivation18.csv")

# read in weather data
weather <- read.csv(file="weatherdata.csv")

#===
# Cleaning
#===
# remove NAs from locations data
locations <- locations[seq(2,nrow(locations),2),]

# change event name from Club to course so matches locations.
colnames(events)[1] <- "course"

#merge two datasets, 567 courses present in both datasets. Second merge includes deprivation info
df <- merge(locations,events,by="course")
df <- merge(df, deprivation, by="course",all=TRUE)

# sort dataframe by week
df <- df[order(as.Date(df$date, format="%d/%m/%Y")),]

# choose event, in this case bushy park
eventlist <- events %>% select(course) %>% unique()
event <- "bushy"

# limit time series to one event chosen.
df <- df %>% 
          select(course, date, participants,imd_sc) %>% 
          filter(course == event) %>%
          mutate(tsdate = as.Date(date,'%d/%m/%Y'),
                 tsyear = as.numeric(format(tsdate,'%Y')),
                 tsweek = week(tsdate),
                 tswkday = wday(tsdate,week_start = 1)) %>%
          filter(tswkday == 6)

# merge dataframes to add in weather data
weather <- weather %>%
  filter(Address == "london bushy park") %>%
  mutate(tsdate = as.Date(Period),
         tsyear = as.numeric(format(tsdate,'%Y')),
         tsweek = week(tsdate),
         tswkday = wday(tsdate,week_start = 1)) %>%
  filter(tswkday == 6) %>%
  select(tsdate, Address, Precipitation, Temperature, Wind.Speed)

df <- merge(df,weather,by="tsdate") 

# plot values - looks correct.
plot(y = df$participants,
     x = df$tsdate,
     type = "l",
     xlab = "Date",
     ylab = "Temperature",col="red")
lines(y = df$Temperature*10,
      x = df$tsdate,type = "l",col="blue")

# create time series using ts function
parkrunts <- ts(data  = df$participants, 
                start = c(df$tsyear[1], df$tsweek[1]), 
                end   = c(df$tsyear[length(df$tsyear)], df$tsweek[length(df$tsweek)]), 
                deltat = 1/52)

# show the model fit with trend, seasonal variation and error.
fit <- stl(x = parkrunts, s.window=51)
plot(fit,main = "Finishers at Bushy Parkrun")

# simple exponential - models level
fit1 <- HoltWinters(parkrunts, beta=FALSE, gamma=FALSE)
plot(fit1)
# double exponential - models level and trend
fit2 <- HoltWinters(parkrunts, gamma=FALSE)
plot(fit2)
# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(parkrunts,seasonal = "mult")
plot(fit3)


#===
# Forcasting using model 3
#===

# forcast forward 52 periods
plot(forecast(fit2, 52))

#===
# ACTUAL DATA -how correct was I?
#===

# read in bushy parkrun data.
temp <- read.csv("bushyparkrun20182019.csv") %>% 
              mutate(tsdate = as.Date(Date,'%d/%m/%Y'),
                     tsyear = as.numeric(format(tsdate,'%Y')),
                     tsweek = week(tsdate),
                     tswkday = wday(tsdate,week_start = 1)) %>%
              filter(tswkday == 6,
                     tsdate > parkrunts %>% time() %>% max()) %>%
              arrange(tsdate)
              
# create the time-series dataset.
bushyts <- ts(data  = temp$Finishers, 
                    start = c(temp$tsyear[1], temp$tsweek[1]), 
                    end   = c(temp$tsyear[length(temp$tsyear)], temp$tsweek[length(temp$tsweek)]), 
                    frequency = length(temp$Finishers))

# plot the predicted vs actual data.
plot(forecast(fit3, 40),
     include = 200,
     main = "Bushy Parkrun")
lines(x = bushyts,type="p",col= "red")
lines(x = bushyts,type="l",col= "red")
legend(x = 2015, y = 1600,
       lty = c(1,1),
       legend = c("actual", "prediction"),
       col = c("red","blue"),
       border = NA,
       bty = "n",
       cex = 0.6)


# add real data to see how close I was:
as.data.frame(forecast(fit3, 50))
as.data.frame(parkrunactual )





#===
# ARCHIVE
#===

# create a time-series dataset for weather
weatherts <- ts(data  = weather$Temperature, 
                start = c(weather$tsyear[1], weather$tsweek[1]), 
                end   = c(weather$tsyear[length(weather$tsyear)], weather$tsweek[length(temp$tsweek)]), 
                deltat = 1/52)

# plot a fit
weatherfit <- stl(x = weatherts, s.window=52)
plot(weatherfit,main = "Temperature Bushy Park")


#===
# MERGE PARKRUN AND WEATHER DATA
#===

temp <- as.data.frame(weatherfit$time.series) %>%
  mutate(date = weather$tsdate)
length(weather$tsdate)
length(weatherfit$time.series)/3
weatherfit$time.series %>% rownames
class(weatherfit)  $tsdate <- rownames(weatherfit)
merged.data <- rbind(weatherfit, fit)

lm(merged.data$participants ~ merged.data$tsdate)

#merged.ts   <- merge(weatherts,parkrunts,by=colnames)
Arima()
plot(parkrunts,weatherts)

ts.plot(weatherts,merged.ts$participants)




















# save a numeric vector containing 72 monthly observations

scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12)

# subset the time series (June 2014 to December 2014)
myts2 <- window(myts, start=c(2014, 6), end=c(2014, 12))

# plot series
plot(myts)


souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
Read 84 items
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)

plot.ts(kingstimeseries)

plot.ts(birthstimeseries)

plot.ts(souvenirtimeseries)

logsouvenirtimeseries <- log(souvenirtimeseries)

plot.ts(logsouvenirtimeseries)

library("TTR")

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)

plot.ts(kingstimeseriesSMA3)

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)

plot.ts(kingstimeseriesSMA8)

birthstimeseriescomponents <- decompose(birthstimeseries)

birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal

plot(birthstimeseriesseasonallyadjusted)


rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
Read 100 items

rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)


rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts


rainseriesforecasts$fitted
plot(rainseriesforecasts)

HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)
