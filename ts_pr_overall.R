# FORCASTING PARKRUNUK ATTENDENCE USING TIME SERIES DATA #

#===
# SETUP
#===

rm(list=ls())

library(tidyr)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
#install.packages("forecast")

#===
# Reading in Data
#===

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

# limit time series to one event chosen, and only saturdays
df <- df %>% 
  select(course, date, participants,imd_sc) %>% 
  #filter(course == event) %>%
  mutate(tsdate = as.Date(date,'%d/%m/%Y'),
         tsyear = as.numeric(format(tsdate,'%Y')),
         tsweek = week(tsdate),
         tswkday = wday(tsdate,week_start = 1)) %>%
  filter(tswkday == 6) 

total <- aggregate(x = df$participants, FUN = sum, by = list(df$tsdate))

df <- data.frame(tsdate = total$Group.1,
                 participants = total$x)

# merge dataframes to add in weather data
weather <- weather %>%
  filter(Address == "london bushy park") %>%
  mutate(tsdate = as.Date(Period),
         tsyear = as.numeric(format(tsdate,'%Y')),
         tsweek = week(tsdate),
         tswkday = wday(tsdate,week_start = 1)) %>%
  filter(tswkday == 6) %>%
  select(tsdate, Address, Precipitation,Precipitation.Cover, Temperature, Wind.Speed)

df <- merge(df,weather,by="tsdate") 

# plot values - looks correct.
plot(y = df$participants,
     x = df$tsdate,
     type = "l",
     xlab = "Date",
     ylab = "Temperature",col="red")

plot(y = df$Temperature/mean(df$Temperature),
     x = df$tsdate,type = "l",col="blue")

# create time series using ts function
parkrunts <- ts(data  = df$participants, 
                start = c(year(df$tsdate[1]), week(df$tsdate[1])), 
                end   = c(year(last(df$tsdate)), week(last(df$tsdate))), 
                deltat = 1/52)

# show the model fit with trend, seasonal variation and error.
fit <- stl(x = parkrunts, s.window=51)
plot(fit,main = "Finishers at parkrun")

# simple exponential - models level
fit1 <- HoltWinters(parkrunts, beta=FALSE, gamma=FALSE)
plot(fit1)
# double exponential - models level and trend
fit2 <- HoltWinters(parkrunts, gamma=FALSE)
plot(fit2)
# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(parkrunts,seasonal = "multiplicative")
plot(fit3)


#===
# Forcasting using model 3
#===

# forcast forward 52 periods
plot(forecast(fit3, 52))

#load validation data
validation <- read.csv(file = "pr_website.csv") %>% 
  mutate(date = as.Date(date,'%d/%m/%Y'))


validation.ts <- ts(data  = validation$athletes[validation$date > last(df$tsdate)], 
                    start = c(year(last(df$tsdate)), week(last(df$tsdate))), 
                    end   = c(year(last(validation$date)), week(last(validation$date))), 
                    deltat = 1/52)

# plot the predicted vs actual data.
plot(forecast(fit3, 100),
     include = 300,
     main = "Weekly parkrun attendance UK",
     xlab = "Date",
     ylab = "Number of Finishers")
lines(x=validation.ts,type = "b",col="red")
legend(x = 2014, y = 100000,
       lty = c(1,1),
       legend = c("historical data", "forecast", "actual"),
       col = c("black","blue","red"),
       border = NA,
       bty = "n",
       cex = 0.6)
<<<<<<< HEAD

#=== 
# WHOLE TIME SERIES
#===

whole.ts <- ts(data  = validation$athletes, 
                    start = c(year(first(validation$date)), week(first(validation$date))), 
                    end   = c(year(last(validation$date)), week(last(validation$date))), 
                    deltat = 1/52)

fit <- stl(x = whole.ts, s.window=51)
plot(fit,main = "Finishers at parkrun")

stl(x = whole.ts, s.window=51)


=======
>>>>>>> 71bf4ff3b46ecda17b5fbd84e40a391b15bcc7d0
