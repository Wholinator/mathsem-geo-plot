library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(sp)
library(dplyr)
data <- read_excel("C:/Users/s524063/Desktop/data.xlsx")
data <- na.omit(data)

#install.packages("plyr")

data.split <- split(data, data$year)

data.2010 <- data.split$'2010'
data.2011 <- data.split$'2011'
data.2012 <- data.split$'2012'
data.2013 <- data.split$'2013'
data.2014 <- data.split$'2014'
data.2015 <- data.split$'2015'

#data$name <- NULL
#pairs(data)
#data.2010$name <- NULL
#pairs(data.2010)
#names(data.2010)
lm.fit <- lm(data$Per.Capita.Income ~ ., data=data)
summary(lm.fit)

#plot(data$year, data$Per.Capita.Income)

even <- function(x) x%%2 == 0
odd <- function(x) x%%2 != 0
states <- map_data("state")
county <- map_data("county")
county <- subset(county, region %in% c("ohio", "minnesota", "missouri", "indiana", "tennessee", "illinois", "kentucky", "nebraska", "kansas", "arkansas"))
states <- subset(states, region %in% c("ohio", "minnesota", "missouri", "indiana", "tennessee", "illinois", "kentucky", "nebraska", "kansas", "arkansas"))
ggplot(data=county) + geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + coord_fixed(1.3)

coData <- inner_join(county, newCounty, by=c("region" = "state", "subregion" = "county"))

#coData2 <- inner_join(countyData, )

coData$employees
countyData$empPop <- ct


countyData = data.2010
countyData$First.quarter.payroll <- NULL
#countyData$county <- unlist(strsplit(tolower(countyData$name), ' '))[1]
#gets the county name from the stuff like wut
countyData$county <- unlist(strsplit(tolower(countyData$name), ', '))[odd(1:1272)]

View(countyData)


axes <- theme(axis.text = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              axis.title = element_blank()
)

plotting <- base + geom_polygon(data = coData, aes(fill=emppopCh), color="white") +
                   geom_polygon(color = "black", fill = NA) +
                   theme_bw() + 
                   axes

#plotting + scale_fill_gradientn(colours = rev(rainbow(7)), trans = "log10")
plotting + scale_fill_gradientn(colours = rev(rainbow(7)), colors = c("red", "green"), values = scales::rescale(c(-30, 0, 0, 40)))

base <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")


df <- subset(states)
counties <- map_data("county")
base <- ggplot(data)

ggplot(data = states) + geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + coord_fixed(1.3)

usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "red") + coord_fixed(1.3)
