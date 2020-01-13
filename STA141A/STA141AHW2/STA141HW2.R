#1
data <- read.csv("~/Desktop/airfare.csv")
data$city_id1 <- as.factor(data$city_id1)
data$city_id1 <-as.factor(data$city_id2)
i = split(data, data$table)
table1a = i$`1a`
table6 = i$`6`
#2
table(data$year)
table(data$quarter)
data$NAs <- apply(is.na(data), 1, sum)
table1a$NAs <- apply(is.na(table1a), 1, sum)
table6$NAs <- apply(is.na(table6), 1, sum)

sevenNAs1ayear <- table(table1a$year[table6$NAs > 6])
barplot(sevenNAs1ayear,xlab= "year", ylab = "frequency", main ="flights between pairs of airports with missing data")
sevenNAs1aquarter<-table(table1a$quarter[table6$NAs > 6])
barplot(sevenNAs1aquarter,xlab= "quarter", ylab = "frequency", main ="flights between pairs of airports with missing data")
sevenNAs6year <- table(table6$year[table6$NAs > 6])
barplot(sevenNAs6year,xlab= "year", ylab = "frequency", main ="flights between pairs of cities with missing data")
sevenNAs6quarter <- table(table6$quarter[table6$NAs > 6])
barplot(sevenNAs6quarter,xlab= "quarter", ylab = "frequency", main ="flights between pairs of cities with missing data")
#3
cities12017<- table(table6$city1[table6$year %in% "2017"])
cities22017 <-table(table6$city2[table6$year %in% "2017"])
mergedCities2017 <- merge(cities12017,cities22017, by="Var1")
mergedCities2017$connections <- rowSums(mergedCities2017[, c("Freq.x", "Freq.y")], na.rm=T)
mostConnections2017 <- sort(mergedCities2017$connections, decreasing = T)[1:10]
mergedCities2017$Var1[mergedCities2017$connections %in% mostConnections2017]
leastConnections2017 <- sort(mergedCities2017$connections, decreasing = F)
leastConnections2017 <- mergedCities2017$Var1[mergedCities2017$connections == 0]
leastConnections2017
cities12007<- table(table6$city1[table6$year %in% "2007" & table6$quarter == 1])
cities22007 <-table(table6$city2[table6$year %in% "2007" & table6$quarter == 1])
mergedCities2007 <- merge(cities12007,cities22007, by="Var1")
mergedCities2007$connections <- rowSums(mergedCities2007[, c("Freq.x", "Freq.y")], na.rm=T)
mostConnections2007 <- sort(mergedCities2007$connections, decreasing = T)[1:10]
mergedCities2007$Var1[mergedCities2007$connections %in% mostConnections2007]
leastConnections2007 <- sort(mergedCities2007$connections, decreasing = F)
leastConnections2007 <- mergedCities2007$Var1[mergedCities2007$connections == 0]
leastConnections2017
cities11997<- table(table6$city1[table6$year %in% "1997" & table6$quarter == 1])
cities21997 <-table(table6$city2[table6$year %in% "1997" & table6$quarter == 1])
mergedCities1997 <- merge(cities11997,cities21997, by="Var1")
mergedCities1997$connections <- rowSums(mergedCities1997[, c("Freq.x", "Freq.y")], na.rm=T)
mostConnections1997 <- sort(mergedCities1997$connections, decreasing = T)[1:10]
mergedCities1997$Var1[mergedCities1997$connections %in% mostConnections1997]
leastConnections1997 <- sort(mergedCities1997$connections, decreasing = F)
leastConnections1997 <- mergedCities1997$Var1[mergedCities1997$connections == 0]
leastConnections1997
#4
tab1a <- aggregate(table1a$passengers ~ table1a$quarter+table1a$year, table1a, FUN = sum)
tab <- aggregate(data$passengers ~ data$quarter+data$year, data, FUN = sum)
pass <- aggregate(data$passengers ~ data$quarter, data, FUN = sum)
barplot(pass$`data$passengers`, pass$`data$quarter`, xlab = "quarter", ylab = "number of passengers")
data$quarterbyyear <- paste(data$year,data$quarter,sep="-")
data$quarterbyyear <- as.factor(data$quarterbyyear)
passengerByQ <- aggregate(data$passengers ~ data$quarterbyyear, data, FUN = sum, na.rm=T)
plot(passengerByQ, type="p", xlab = "quarter", ylab = "number of total passengers", main ="number of passengers per quarter")

pass <- aggregate(data$passengers ~ data$quarter, data, FUN = sum)
barplot(pass$`data$passengers`, pass$`data$quarter`, xlab = "quarter", ylab = "number of passengers")
#5
library(readxl)
cpi <- read_xlsx("~/Desktop/cpi_1996_2017.xlsx")
cpi <-cpi[12:33,0:13] #gets rid of unnecessary data
cpi <- data.frame(cpi)

cpi <- cpi[,seq(4,13,3)] #getting 4 numbers, one for each quarter
frame1 <- data.frame(year=rep(c(1996:2017),times=4), quarter=rep(c(1:4),each=22), cpi = stack(cpi))
new <- merge(frame1,table6, by.x=c("year","quarter"), all=TRUE)
new$cpi.values<- as.numeric(new$cpi.values)
new$real17_fare <- new$lg_fare * (new$cpi.values[new$year == 2017 & new$quarter == 3]/new$cpi.values)
#6
new$real17_fareq1 <- new$lg_fare * (200.091/new$cpi.values)
new$quarterbyyear <- paste(new$year,new$quarter,sep="-")
new$quarterbyyear <- as.factor(new$quarterbyyear)
plot(aggregate(new$real17_fareq1 ~ new$quarterbyyear, new, FUN = mean), xlab="time (quarter by year)", ylab = "average fare", main="average airfare in real (quarter 1, 2017) dollars over time")
plot(aggregate(new$real17_fareq1 ~ new$quarterbyyear, new, FUN = mean), type = "l", xlab="time (quarter by year)", ylab = "average fare", main="average airfare in real (quarter 1, 2017) dollars over time", xlim=c(0,20))
plot(aggregate(new$real17_fareq1 ~ new$quarterbyyear, new, FUN = mean), type = "l", xlab="time (quarter by year)", ylab = "average fare", main="average airfare in real (quarter 1, 2017) dollars over time", xlim=c(70,90))
#7
fare1a <- aggregate(table1a$fare ~ table1a$miles, table1a, FUN=mean)
plot(fare1a, xlab = "miles", ylab="fare", main="airfare vs. distance for flights between pairs of airport")
plot(aggregate(table6$fare ~ table6$miles, table6, FUN=mean), xlab = "miles", ylab="fare", main="airfare vs. distance for flights between pairs of cities")
lm(aggregate(table1a$fare ~ table1a$miles, table1a, FUN=mean))
lm(aggregate(table6$fare ~ table6$miles, table6, FUN=mean))
#8
plot(aggregate(table1a$passengers ~ table1a$miles, table1a, FUN=mean), xlab = "miles", ylab="number of passengers", main="passengers vs. distance for flights between pairs of airport")
plot(aggregate(table6$passengers ~ table6$miles, table6, FUN=mean), xlab = "miles", ylab="number of passengers", main="passengers vs. distance for flights between pairs of cities")
lm(aggregate(table1a$passengers ~ table1a$miles, table1a, FUN=mean))
lm(aggregate(table6$passengers ~ table6$miles, table6, FUN=mean))