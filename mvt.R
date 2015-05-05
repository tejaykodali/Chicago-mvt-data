library(ggplot2)
library(maps)
library(ggmap)

## Reading in the data
mvt = read.csv("mvt.csv", stringsAsFactors = FALSE)

## Converting the Date to a recognizable format
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

## Sorting the weekdays
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered = TRUE,
                               levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

## Plotting the number of crimes each day (line graph)
png(filename = "WeekdayCrimes.png", width = 800, height = 600, units = "px")
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group = Var1, color = Var1)) + 
        xlab("Hour") + ylab("Number of Motor Vehicle Thefts") + 
        ggtitle("Daily number of Motor Vehicle Thefts")
dev.off()

## Plotting the number of crimes each day (heatmap)
png(filename = "Heatmap.png", width = 800, height = 600, units = "px")
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + 
        scale_fill_gradient(name = "Total Motor Vehicle Thefts", low = "white", high = "red") + 
        theme(axis.title.y = element_blank())
dev.off()

## Getting Chicago map
chicago = get_map(location = "chicago", zoom = 11)
LocationCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
LocationCounts$long = as.numeric(as.character(LocationCounts$Var1))
LocationCounts$lat = as.numeric(as.character(LocationCounts$Var2))
LocationCounts = subset(LocationCounts, Freq > 0)

## Plotting the location heatmap
png(filename = "Chicagomap.png", width = 800, height = 600, units = "px")
ggmap(chicago) + geom_tile(data = LocationCounts, aes(x = long, y = lat, alpha = Freq),
                           fill = "red") + theme(axis.title.y = element_blank(), axis.title.x = element_blank())
dev.off()

