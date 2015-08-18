library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(ggmap)

## Reading in the data
chicagoMVT <- read.csv('motor_vehicle_theft.csv', stringsAsFactors = FALSE)

## Converting the date to a recognizable format
chicagoMVT$Date <- strptime(chicagoMVT$Date, format = '%m/%d/%Y %I:%M:%S %p')

## Getting the day and hour of each crime
chicagoMVT$Day <- weekdays(chicagoMVT$Date)
chicagoMVT$Hour <- chicagoMVT$Date$hour

## Sorting the weekdays
dailyCrimes <- as.data.frame(table(chicagoMVT$Day, chicagoMVT$Hour))
names(dailyCrimes) <- c('Day', 'Hour', 'Freq')
dailyCrimes$Hour <- as.numeric(as.character(dailyCrimes$Hour))
dailyCrimes$Day <- factor(dailyCrimes$Day, ordered = TRUE, 
                         levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

## Plotting the number of crimes each day (line graph)
png(filename = 'dailyCrimes.png', width = 800, height = 600, units = 'px')
ggplot(dailyCrimes, aes(x = Hour, y = Freq)) + geom_line(aes(group = Day, color = Day)) + 
  xlab('Hour') + ylab('Number of thefts') + ggtitle('Daily number of Motor Vehicle Thefts')
dev.off()

## Plotting the number of crimes each day (heat map)
png(filename = 'heatmap.png', width = 800, height = 600, units = 'px')
ggplot(dailyCrimes, aes(x = Hour, y = Day)) + geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name = 'Total Motor Vehicle Thefts', low = 'black', high = 'red') + 
  theme(axis.title.y = element_blank(), plot.background = element_rect(fill = 'gray13', color = 'white'),
        panel.background = element_rect(fill = 'black', color = 'white'))
dev.off()

## Removing empty locations and splitting Location into Latitude and Longitude
chicagoMVT$Location[chicagoMVT$Location == ''] <- NA
chicagoMVT <- na.omit(chicagoMVT)
chicagoMVT <- chicagoMVT %>% extract(Location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
chicagoMVT$Longitude <- round(as.numeric(chicagoMVT$Longitude), 2)
chicagoMVT$Latitude <- round(as.numeric(chicagoMVT$Latitude), 2)

## Get Chicago map
chicago <- get_map(location = 'chicago', zoom = 11)
png(filename = "Chicago.png", width = 800, height = 600, units = "px")
ggmap(chicago)
dev.off()

## Get crime locations
locationCrimes <- as.data.frame(table(chicagoMVT$Longitude, chicagoMVT$Latitude))
names(locationCrimes) <- c('long', 'lat', 'Frequency')
locationCrimes$long <- as.numeric(as.character(locationCrimes$long))
locationCrimes$lat <- as.numeric(as.character(locationCrimes$lat))
locationCrimes <- subset(locationCrimes, Freq > 0)

## Plotting the location heatmap
png(filename = "Chicagomap.png", width = 800, height = 600, units = "px")
ggmap(chicago) + geom_tile(data = locationCrimes, aes(x = long, y = lat, alpha = Frequency),
                           fill = "red") + theme(axis.title.y = element_blank(), axis.title.x = element_blank())
dev.off()

