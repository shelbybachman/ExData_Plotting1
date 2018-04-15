# exploratory data analysis course
# week 1 project

# this script creates plot 1
# (a histogram of global active power)
# last updated 09.04.2018

# initialization
rm(list = ls())
library(rprojroot)
library(data.table)
library(tidyverse)
library(lubridate)

# function to save course directory location
path <- function(x) find_root_file(x, criterion = has_file('Exploratory Data Analysis.Rproj'))

# read data
data <- fread(path('household_power_consumption.txt'))

# make any cells marked ? equal to NA
data[data=='?'] <- NA

# subset data to include only relevant dates
# we will only be using data from dates 2007-02-01 and 2007-02-02
dates <- c('2/2/2007', '1/2/2007') # dates are natively in d/m/yyyy format
data <- data %>%
  filter(Date %in% dates)

# create a dateTime variable which includes both of date and time
# for plots against time
data <- data %>%
  rowwise() %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
dateTime <- data$Date + hms(data$Time)
data$dateTime = ymd_hms(dateTime)

# make relevant variables numeric
data <- data %>%
  ungroup() %>%
  mutate(Global_active_power = as.numeric(Global_active_power),
         Global_reactive_power = as.numeric(Global_reactive_power),
         Voltage = as.numeric(Voltage),
         Global_intensity = as.numeric(Global_intensity),
         Sub_metering_1 = as.numeric(Sub_metering_1),
         Sub_metering_2 = as.numeric(Sub_metering_2),
         Sub_metering_3 = as.numeric(Sub_metering_3))

# create plot 1, a histogram of global active power
png(filename = path('plot1.png'), width = 480, height = 480) # open device (PNG file)
hist(data$Global_active_power, # create the histogram
     xlab = 'Global Active Power (kilowatts)',
     ylab = 'Frequency',
     main = 'Global Active Power',
     ylim = c(0,1200),
     col = 'red',
     axes = FALSE)
axis(1, c(0,2,4,6))
axis(2, seq(from = 0, to = 1200, by = 200))
dev.off() # close device

