#1
baybiketrip <- read.csv("~/Desktop/Downloads/bikes/sf_bikeshare_trips.csv")
#summary(baybiketrip)
baybiketrip$start_date = as.POSIXlt(baybiketrip$start_date)
baybiketrip$end_date = as.POSIXlt(baybiketrip$end_date)
baybiketrip$trip_id = as.factor(baybiketrip$trip_id)
baybiketrip$bike_number = as.factor(baybiketrip$bike_number)
baybiketrip$start_station_id = as.factor(baybiketrip$start_station_id)
saveRDS(baybiketrip, "sf_bikeshare_trips.rds")

baybikeshare <- read.csv("~/Desktop/Downloads/bikes/sf_bike_share_stations.csv")
#summary(baybikeshare)

baybikeshare$station_id = as.factor(baybikeshare$station_id)
baybikeshare$name = as.factor(baybikeshare$name)
baybikeshare$installation_date = as.POSIXlt(baybikeshare$installation_date)


saveRDS(baybikeshare, "sf_bike_share_stations.rds")

#2
#install.packages("ggmap")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("sf")
#install.packages("lubridate")
library(ggmap)
library(ggplot2)

library(lubridate)
library(readr)
library(sf)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("devtools")
devtools::install_github("dkahle/ggmap")
#devtools::install_github("hadley/ggplot2")
#install.packages("geosphere")
library("geosphere")
baystation <- readRDS("sf_bike_share_stations.rds")
baytripdf <-readRDS("sf_bikeshare_trips.rds")
baystationdf <- subset(baystation, baystation$landmark=="San Francisco" &duplicated(baystation$station_id)==FALSE)
#summary(baytripdf)
#summary(baystationdf)
#ggplot(baystationdf, aes(longitude, latitude)) 
startTab <- table(baytripdf$start_station_id)
startFrame <- as.data.frame(startTab)
baystationdf <- merge(baystationdf, startFrame, by.x = "station_id", by.y = c("Var1"))
#library(ggrepel)
loc = sapply(baystationdf[c("longitude", "latitude")], function(longitude) mean(range(longitude)))
m = get_map(loc, zoom = 14)
agg.data <- aggregate(cbind(longitude,latitude) ~ name, data = baystationdf, mean) #agg.data only save the data that is unique
ggmap(m, xlab = "Longitude", ylab="Latitude", legend="right") + geom_point(data=baystationdf, aes(x=longitude, y=latitude, size=Freq), alpha =  I(1/3)) + geom_text_repel(data = agg.data, aes(x = longitude, y = latitude, label = name),size=2) + scale_size(range = c(1, 8))
#geom_density_2d(aes(x = longitude, y = latitude), baysharedf)
#3
#q316 <- read.csv("~/Desktop/Downloads/bikes/2016_q3_la_metro_trips.csv")
#q416 <- read.csv("~/Desktop/Downloads/bikes/2016_q4_la_metro_trips.csv")
#q117 <- read.csv("~/Desktop/Downloads/bikes/2017_q1_la_metro_trips.csv")
#q217 <- read.csv("~/Desktop/Downloads/bikes/2017_q2_la_metro_trips.csv")
#q317 <- read.csv("~/Desktop/Downloads/bikes/2017_q3_la_metro_trips.csv")
#names(q316) <- gsub("station_id", "station", names(q316))
#q316$start_time <- parse_date_time(q316$start_time, orders = c("y-m-d H:M:S", "m/d/y H:M"))
#q316$end_time <- parse_date_time(q316$end_time, orders = c("y-m-d H:M:S", "m/d/y H:M"))
#q316$trip_id = as.factor(q316$trip_id)
#q316df <- as.data.frame(q316)
#saveRDS(q316, "q316.rds")
#path = c("~/Desktop/Downloads/bikes/2016_q3_la_metro_trips.csv","~/Desktop/Downloads/bikes/2016_q4_la_metro_trips.csv", "~/Desktop/Downloads/bikes/2017_q1_la_metro_trips.csv", "~/Desktop/Downloads/bikes/2017_q2_la_metro_trips.csv","~/Desktop/Downloads/bikes/2017_q3_la_metro_trips.csv")

download_dir<-"~/Desktop/Downloads/bikes/"
pathLAIndex <- grep("la_metro",list.files(download_dir))
pathLAIndex <- grep("\\.csv$",list.files(download_dir)[pathLAIndex])
relPathLA <- list.files(download_dir)[pathLAIndex]
fullPathLA <- paste0(download_dir,list.files(download_dir)[pathLAIndex])

labikes <- lapply(1:5, function(x) {
  labike <- read.csv(fullPathLA[x])
  names(labike) <- gsub("station_id", "station", names(labike))
  labike$start_time<-parse_date_time(labike$start_time,c("m/d/y H:M","y-m-d H:M:S"))
  labike$end_time<-parse_date_time(labike$end_time,c("m/d/y H:M","y-m-d H:M:S"))
  #labike$start_time <- parse_date_time(labike$start_time, orders = c("y-m-d H:M:S", "m/d/y H:M"))
  #labike$end_time <- parse_date_time(labike$end_time, orders = c("y-m-d H:M:S", "m/d/y H:M"))
  labike$trip_id = as.factor(labike$trip_id)
  labike$bike_id = as.factor(labike$bike_id)
  labike <- as.data.frame(labike)
  #relPatLA <- gsub("(.+bikes/|\\.csv$)","",fullPathLA) #parsing through the full path name to give simple file name when saving to RDS
  #saveRDS(labikedf, file = paste0(relPatLA[x],".rds"))
})

labikestrip <- do.call(rbind, labikes)

# Write a second function that loads, tidies, and saves the Los Angeles bike share station data.
labikeshare<- read.csv("~/Desktop/Downloads/bikes/metro-bike-share-stations-2017-10-20.csv")
labikeshare$Station_ID <- as.factor(labikeshare$Station_ID)
labikeshare$Go_live_date <- parse_date_time(labikeshare$Go_live_date, orders = c("y-m-d", "m/d/y"))
saveRDS(labikeshare, "metro-bike-share-stations-2017-10-20.rds")
lastation <- readRDS("~/Desktop/Downloads/bikes/metro-bike-share-stations-2017-10-20.rds")
lastationdf <- subset(lastation, lastation$Region=="DTLA" &duplicated(lastation$Station_ID)==FALSE)
#making a new data frame that has all the data of trip plus counts the frequency of start station 
startTabLA <- table(labikestrip$start_station)
startFrameLA <- as.data.frame(startTabLA)
names(startFrameLA) <- c("start_station","frequency")
labikedf <- merge(labikestrip, startFrameLA, by = "start_station", na.rm=TRUE)

labikedf <- na.omit(labikedf)

m = get_map(location = c(lon = median(labikedf$start_lon), lat = median(labikedf$start_lat)), zoom = 14)
agg.data <- aggregate(cbind(start_lon,start_lat) ~ start_station, data = labikedf, mean) #agg.data only save the data that is unique

agg.datamerge <- merge(agg.data, lastationdf, by.x = "start_station", by.y = "Station_ID")
ggmap(m, xlab = "Longitude", ylab="Latitude", legend="right") + geom_point(data=labikedf, aes(x=start_lon, y=start_lat, size=frequency), alpha =(1/10))  + geom_text_repel(data = agg.datamerge, aes(x=start_lon, y=start_lat, label = Station_Name),size=2) + scale_size(range = c(1, 8))


##5
sfstation <- readRDS("sf_bike_share_stations.rds")

sfstationdf <- subset(sfstation, sfstation$landmark=="San Francisco")
sfids<-unique(sfstationdf$station_id)
sftripsid <-as.numeric(baytripdf$start_station_id) %in% sfids & as.numeric(baytripdf$end_station_id) %in% sfids
sftrips <- baytripdf[sftripsid,]
station_lon<-aggregate(longitude~station_id,sfstationdf,median)
station_lat<-aggregate(latitude~station_id,sfstationdf,median)

sfstartlonlat <- merge(station_lon,station_lat, by = "station_id")
names(sfstartlonlat) <- c("start_station_id", "startmedlon", "startmedlat")
sftrips <- merge(sftrips, sfstartlonlat, by = "start_station_id")

sfendlonlat <- merge(station_lon,station_lat, by = "station_id")
names(sfendlonlat) <- c("end_station_id", "endmedlon", "endmedlat")
sftrips <- merge(sftrips, sfendlonlat, by = "end_station_id")
#frequency of start station
sftrips <- merge(sftrips, data.frame(table(sftrips$start_station_id)), by.x = "start_station_id", by.y = "Var1")

#distance
sftrips$distance<-distGeo(data.frame(sftrips$startmedlon, sftrips$startmedlat),data.frame(sftrips$endmedlon, sftrips$endmedlat))
# parsing by time of day
sftrips$start_date <- as.POSIXct(sftrips$start_date)
sftrips$timeofday<-cut(hour(sftrips$start_date),c(-1,8,16,25),c("12AM-8AM","8AM-4PM","4PM-12AM"))
ggplot(data = sftrips, aes (x= timeofday, y=duration_sec)) + geom_boxplot(outlier.shape=NA) + labs(x="Time of Day", y= "duration of trip in seconds", title = "How duration of a trip change in a day in SF") + scale_y_continuous(limits = quantile(sftrips$duration_sec, c(0.1, 0.9)))
ggplot(data = sftrips, aes (x= timeofday, y=distance)) + geom_boxplot() + labs(x="Time of Day", y= "distance of trip", title = "How distance of a trip change in a day in SF")
ggplot(data = sftrips, aes (x= timeofday, y=Freq)) + geom_boxplot() + labs(x="Time of Day", y= "frequency of trips", title = "How  frequency of trips change in a day in SF")

####LA

lastation <-readRDS("metro-bike-share-stations-2017-10-20.rds")
lastationdf <- subset(lastation, lastation$Region=="DTLA")
dtla_ids<-unique(lastation$Station_ID)

intra_metro_trips<-as.numeric(labikedf$start_station) %in% dtla_ids & as.numeric(labikedf$end_station) %in% dtla_ids
latrips<-labikedf[intra_metro_trips,]
lastation$Station_ID=factor(lastation$Station_ID)
#Make these numeric
latrips$start_lon<-as.numeric(latrips$start_lon)
latrips$start_lat<-as.numeric(latrips$start_lat)
latrips$end_lon<-as.numeric(latrips$end_lon)
latrips$end_lat<-as.numeric(latrips$end_lat)

#Get the station locations, use median because some long/lat differ for a station
station_lon<-aggregate(start_lon~start_station,latrips,median)
station_lat<-aggregate(start_lat~start_station,latrips,median)
end_lon<-aggregate(end_lon~end_station,latrips,median)
end_lat<-aggregate(end_lat~end_station,latrips,median)
station_loc<-data.frame(Station_ID=station_lon[,1],startmedlon=station_lon[,2],startmedlat=station_lat[,2])
end_loc <- data.frame(Station_ID=end_lon[,1],endmedlon=end_lon[,2],endmedlat=end_lat[,2])
#add location to df
latrips <- merge(latrips, station_loc, by.x = "start_station", by.y = "Station_ID")
latrips <- merge(latrips, end_loc, by.x = "end_station", by.y = "Station_ID")

#frequency of start station
lastation <- merge(lastation, data.frame(table(latrips$start_station)), by.x = "Station_ID", by.y = "Var1")

#distance
latrips$distance<-distGeo(data.frame(latrips$startmedlon, latrips$startmedlat),data.frame(latrips$endmedlon, latrips$endmedlat))
# parsing by time of day
latrips$timeofday<-cut(hour(latrips$start_time),c(-1,8,16,25),c("12AM-8AM","8AM-4PM","4PM-12AM"))
ggplot(data = latrips, aes (x= timeofday, y=duration)) + geom_boxplot() + labs(x="Time of Day", y= "duration of trip", title = "How duration of a trip change in a day in DTLA")
ggplot(data = latrips, aes (x= timeofday, y=duration)) + geom_boxplot(outlier.shape=NA) + labs(x="Time of Day", y= "duration of trip", title = "How duration of a trip change in a day in DTLA") + scale_y_continuous(limits = quantile(sftrips$duration_sec, c(0.1, 0.9)))
ggplot(data = latrips, aes (x= timeofday, y=distance)) + geom_boxplot() + labs(x="Time of Day", y= "distance of trip", title = "How distance of a trip change in a day in DTLA with outliers")
ggplot(data = latrips, aes (x= timeofday, y=distance)) + geom_boxplot(outlier.shape=NA) + labs(x="Time of Day", y= "distance of trip", title = "How distance of a trip change in a day in DTLA without outliers") + scale_y_continuous(limits = quantile(latrips$distance, c(0.1, 0.9)))
ggplot(data = latrips, aes (x= timeofday, y=frequency)) + geom_boxplot() + labs(x="Time of Day", y= "frequency of trips", title = "How frequency of trips change in a day in DTLA")

###6 
sftrips$bearing <- bearing(data.frame(sftrips$startmedlon,sftrips$startmedlat), data.frame(sftrips$endmedlon, sftrips$endmedlat))

ggplot(data = sftrips, aes (x= timeofday, y=bearing)) + geom_boxplot() + labs(x="Time of Day", y= "frequency of trips", title = "How median bearing change in a day in SF")