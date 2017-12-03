df <- read.csv("/Users/vivekkalyanarangan/Desktop/Forest Fires/forestfires.csv")

# Subsetting without factor variables
df_2 <- subset(df,select=-c(month,day))
cor_mat <- cor(df_2)

# Automatically
lst=c()
i<-1
for(colname in colnames(df)){
  if(class(df[[colname]])=="numeric"){
    lst[[i]] = colname
    i=i+1
  }
}
cor_mat <- cor(df[lst])

# Scatter plot
?plot
plot(df$DMC, df$DC, main="Scatterplot Example", 
     xlab="DMC ", ylab="DC", pch=19)

# Basic Scatterplot Matrix
pairs(~DMC+DC+wind+rain,data=df, 
      main="Simple Scatterplot Matrix")

df<-forestfires
attach(df)

# 3D Scatterplot
#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(wind,rain,temp, main="3D Scatterplot")


# Interactive 3D Scatterplot
install.packages("rgl")
# devtools::install_github("rgl", "trestletech", "js-class") # for mac only
# start XQuartz - for mac only
library(rgl)
#options(rgl.useNULL=TRUE)
plot3d(wind, DMC, DC, col="red", size=3)

# Boxplot
boxplot(X~Y,data=df, main="Boxplot", 
        xlab="X", ylab="Y")

# Simple Bar Plot 
counts <- table(df$temp)
barplot(counts, main="Temperature Distribution", 
        xlab="Temp")

#  Simple Horizontal Bar Plot
counts <- table(df$temp)
barplot(counts, main="Temperature Distribution", horiz=TRUE)

# Stacked Bar Plot with Colors and Legend
counts <- table(df$X, df$Y)
barplot(counts, main="Distribution by X and Y",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts))

# Grouped Bar Plot
counts <- table(df$X, df$Y)
barplot(counts, main="Distribution by X and Y",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

# Histogram Probability Dist
hist(df$wind, 
     main="Histogram for Air Passengers", 
     xlab="Passengers", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(df$wind))

# Simple Pie Chart
library(plyr)
library(dplyr)

?summarise
df_pivot <- summarize(group_by(df,month),wind=sum(wind))
slices <- df_pivot[["wind"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["month"]], main="Pie Chart of Wind")

# MAP PLOT
airports <- read.csv("/Users/vivekkalyanarangan/Desktop/Map/airports.dat")
head(airports)
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
head(airports)

routes <- read.csv("/Users/vivekkalyanarangan/Desktop/Map/routes.dat")
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")
head(routes)

departures <- ddply(routes, .(sourceAirportID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"

airportD <- merge(airports, departures, by.x = "ID", by.y = "sourceAirportID")

# install.packages("ggmap")
#install.packages("devtools")
#devtools::install_github("dkahle/ggmap") # for mac only
#devtools::install_github("hadley/ggplot2") # for mac only
library(ggmap)
map <- get_map(location = 'Europe', zoom = 4)

mapPoints <- ggmap(map) +
     geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportD, alpha = .5)

mapPoints

detach(df)


# Run each plot and try to study and enhance
# Scatterplot matrix of DMC,DC,wind,rain,temp
# 3D Scatterplot of wind,rain,area
# Interactive 3D Scatterplot of wind,rain,area
# Boxplot of X and Y
# Simple bar plot of temp, wind, rain [horizontal and vertical]
# Grouped bar plot of X and Y
# Histogram of probability distribution of X, Y, wind, temp, area along with line density
# Histogram of frequency distribution of X, Y, wind, temp, area
# Pie Chart of area, wind, rain, temp by month
# Pie Chart of area, wind, rain, temp by day
# Map Plot of sourceAirportID
# Map Plot of destinationAirportID

#### ASSIGNMENT ####
# EXPORT each plot to a png/pdf programmatically and through UI