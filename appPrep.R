library(dplyr)
library(tidyverse)
library(rgdal)
library(maps)
library(xts)


#1. Load Ug Map, District maps
#load ug map
ug_shp <- readOGR("data/ugmaps", "uga_admbnda_adm0_UBOS_v2")
#load districts map
#ug_districts <- readOGR("data/ugmaps","District_2016")
#make CRS same
#ug_districts <- spTransform(ug_districts, proj4string(ug_shp))
#update dataframe
# dist_centroids <- coordinates(ug_districts)
# dist_centroids <- as.data.frame(dist_centroids)
# names(dist_centroids) <- c("long","lat")
# ug_districts@data <- cbind(ug_districts@data, dist_centroids)
# ug_districts@data <- ug_districts@data[,c(1,2,5,6,4)]
# ug_districts@data$Shape_Area <- ug_districts@data$Shape_Area/1000000
# colnames(ug_districts@data)[2] <- "DISTRICT"
# colnames(ug_districts@data)[5] <- "AREA_SQKM"
#write.csv(ug_districts@data, "district.data.csv")
ug_districts <- read.csv("data/district.data.csv", check.names=FALSE)

#1. Load Rainfall Data
#load national weather data 
arc2.by.year <- read.csv("data/arc2.rainfall.ug.annual.csv", check.names=FALSE)
arc2.by.month <- read.csv("data/arc2.rainfall.ug.monthly.csv", check.names=FALSE)
arc2.by.season <- read.csv("data/arc2.rainfall.ug.seasonal.csv", check.names = FALSE)

arc2.by.year.totals <- gather(summarise_each(arc2.by.year[,c(4:ncol(arc2.by.year))], funs(sum)))
names(arc2.by.year.totals) <- c("year","totals")
arc2.by.year.means <- gather(summarise_each(arc2.by.year[,c(4:ncol(arc2.by.year))], funs(mean)))
names(arc2.by.year.means) <- c("year","means")
arc2.by.year.summary <- left_join(arc2.by.year.totals,arc2.by.year.means,by="year")


#load weather data by district
arc2.by.district.tots <- read.csv("data/arc2.rainfall.by.district.daily.totals.csv", check.names=FALSE)
arc2.by.district.mean <- read.csv("data/arc2.rainfall.by.district.daily.means.csv", check.names=FALSE)
tmax.by.district <- read.csv("data/tmax.by.district.daily.means.csv", check.names=FALSE)

group_rf_tots_by_year <- as.data.frame(arc2.by.district.tots[,c(2,4:ncol(arc2.by.district.tots))] %>% group_by(year) %>% summarise_each(funs(sum)))
group_rf_zero_by_year <- as.data.frame(arc2.by.district.tots[,c(2,4:ncol(arc2.by.district.tots))] %>% group_by(year) %>% summarise_each(funs(sum(.==0))))
rm(arc2.by.district.tots)
group_rf_mean_by_year <- as.data.frame(arc2.by.district.mean[,c(2,4:ncol(arc2.by.district.mean))] %>% group_by(year) %>% summarise_each(funs(mean)))
rm(arc2.by.district.mean)
group_tmax_mean_by_year <- as.data.frame(tmax.by.district[,c(3,5:ncol(tmax.by.district))] %>% group_by(year) %>% summarise_each(funs(mean)))
group_tmax_hots_by_year <- as.data.frame(tmax.by.district[,c(3,5:ncol(tmax.by.district))] %>% group_by(year) %>% summarise_each(funs(sum(.>29))))
rm(tmax.by.district)

#rf_ts <- read.csv("data/arc2.rainfall.ug.daily.csv", check.names=FALSE)
#rf_ts_totals <- gather(summarise_each(rf_ts[,c(5:ncol(rf_ts))], funs(sum)))
#write.csv(rf_ts_totals, "arc2.rainfall.ug.daily.totals.csv")
rf_ts <- read.csv("data/arc2.rainfall.ug.daily.totals.csv", check.names=FALSE)
dy_ts <- xts(x=rf_ts$value, order.by=as.Date(rf_ts$key))

UG <- map_data("world") %>% filter(region=="Uganda")