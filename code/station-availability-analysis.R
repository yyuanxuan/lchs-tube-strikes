# station availability analysis
# section 4.2

# load the packages
library(tidyverse)
library(fst)
library(sf)
library(tmap)
library(lubridate)
library(ggspatial)
library(egg)
library(RColorBrewer)

#load data sets
load("data/london_tube_data.Rdata")
load('data/bike_stations.Rdata')
bike_avai<-read_csv("data/availability_data.csv", col_types = cols(t = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% as_tibble()

unique(bike_avai$t %>% date())

# function for examining/reporting time series of low-availability at lchs bike stations
low_bikeNdock_avai_time_series_func<-function(bike_avai,station_dist_line_data,non_str_day1,non_str_day2,str_day,avai_threshold,dayofweek){
  if(dayofweek=="weekday"){
    y_breaks <- c(0,25,50,75,100)
    y_lim <- c(0,100)
  } else {
    y_breaks <- c(0,25,50,75,100)
    y_lim <- c(0,100)
  }
  bike_avai %>% mutate(date = date(t),
                       hour = hour(t)) %>% 
    filter(date %in% date(as.POSIXct(c(paste(non_str_day1,"00:00:00"),
                                       paste(non_str_day2,"00:00:00"),
                                       paste(str_day,"00:00:00"))))) %>%
    filter(hour %in% c(7:21))%>%
    mutate(stationId = as.character(stationId)) %>%
    left_join(station_dist_line_data  %>% 
                dplyr::select(station_id,dist2tube) %>%
                mutate(station_id=as.character(station_id)), by=c("stationId"="station_id")) %>%
    st_as_sf() %>% st_drop_geometry() %>%
    mutate(dist_cate = cut(dist2tube,breaks = seq(0,1000,by=250))) %>%
    filter(!is.na(dist_cate)) %>%
    mutate(dock_size=availableBikes+availableDocks) %>%
    mutate(low_avail_bikes=case_when(
      availableBikes<=(dock_size*avai_threshold)~1,
      availableBikes>(dock_size*avai_threshold)~0
    )) %>%
    mutate(low_avail_docks=case_when(
      availableDocks<=(dock_size*avai_threshold)~ 1,
      availableDocks>(dock_size*avai_threshold)~0
    )) %>% 
    mutate(low_avail_bikesNdocks=low_avail_bikes+low_avail_docks)%>%
    mutate(strike_day=case_when(
      date == str_day ~ "Strike Day",
      date != str_day ~ "Non-Strike Day")) %>%
    group_by(dist_cate,hour,strike_day,stationId) %>%
    summarise(low_avail_count = sum(low_avail_bikesNdocks)) %>% 
    group_by(dist_cate,hour,strike_day) %>%
    summarise(low_avail_count_ave=mean(low_avail_count))%>%
    mutate(low_avail_count_ave=case_when(
      strike_day=="Strike Day" ~ low_avail_count_ave/1,
      strike_day!="Strike Day" ~ low_avail_count_ave/2)) %>%
    mutate(type=paste(strike_day,dist_cate)) %>% 
    ggplot(aes(x=hour,y=low_avail_count_ave,color=type))+
    geom_line(lwd=0.8)+
    geom_point(size=1.4)+
    scale_color_manual(values=c(rev(brewer.pal(n=5,"Blues")[2:5]),rev(brewer.pal(n=5,"Reds")[2:5])))+
    theme_bw()+
    scale_x_continuous(breaks = seq(7,23,by=2))+
    scale_y_continuous(name="service pressure")+
    labs(color= "day and distance to tube")
  
}

#Network Level Strike

# calculate distance to tube network
# initiate the dataframe
station_dist_tube_network<- bike_stations %>% st_transform(crs=27700)
# add the distance column
station_dist_tube_network$dist2tube<-station_dist_tube_network %>% 
  st_distance(tube_stations_sf %>% st_transform(crs=27700)) %>% 
  as.data.frame() %>%
  apply(1,FUN = min)

avail_network_strike<-ggarrange(low_bikeNdock_avai_time_series_func(bike_avai,station_dist_tube_network,"2015-07-02","2015-07-16","2015-07-09",0.15,"weekday")+
                       ggtitle("(a) Weekday network-level")+
                       #theme(plot.title = element_text(size=10),legend.position = "none"),
                       theme(plot.title = element_text(size=10),
                             legend.position = c(1,-0.3),
                             #legend.title.align = 5,
                             legend.direction = "horizontal",
                             plot.margin=unit(c(0,0,1.5,0),"cm")),
                     low_bikeNdock_avai_time_series_func(bike_avai,station_dist_tube_network,"2015-02-21","2015-03-28","2015-03-07",0.15,"weekend")+
                       ggtitle("(b) Weekend network-level")+
                       theme(plot.title = element_text(size=10),axis.title.y = element_blank(), legend.position = "none"),
                     nrow=1
                     #ncol=1
)
avail_network_strike

ggsave("figures/Low_Avai_Bike&Dock_timeseries_NetworkLVL.png",plot = avail_network_strike,
       width = 26,height = 9,units = "cm",dpi=300)


# Line level analysis
# Strike line region's change and compare it to network level

bike_avai<-read_csv("data/availability_data.csv", col_types = cols(t = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% as_tibble()

# calculate distance to tube network
# initiate the dataframe
bike_station_dist_piccadilly<-bike_stations %>% 
  dplyr::select(station_id,geometry) %>%
  mutate(station_id=as.character(station_id))

# add dist column
bike_station_dist_piccadilly$dist2tube<-bike_station_dist_piccadilly %>% st_transform(crs=27700) %>% 
  st_distance(tube_stations_PiccadillyLine_sf %>% st_transform(crs=27700)) %>%
  as.data.frame() %>%
  apply(1,FUN = min) 

bike_station_dist_CW<-bike_stations %>% 
  dplyr::select(station_id,geometry) %>%
  mutate(station_id=as.character(station_id))

bike_station_dist_CW$dist2tube<-bike_station_dist_CW %>% st_transform(crs=27700) %>% 
  st_distance(rbind(tube_stations_CentralLine_sf,
                    tube_stations_WaterlooNCity_sf) %>% st_transform(crs=27700) ) %>%
  as.data.frame() %>%
  apply(1,FUN = min) 


avail_line_strike<-ggarrange(
  # Piccadilly line strike compare
  # 1. network level weekday
  low_bikeNdock_avai_time_series_func(bike_avai,bike_station_dist_piccadilly,"2015-07-02","2015-07-16","2015-07-09",0.15,"weekday")+
    scale_y_continuous(name="service pressure",limits = c(1,6))+
    ggtitle("(a) Weekday network-level strike:\n     Around Piccadilly line")+
    theme(plot.title = element_text(size=10),
          legend.position = "none"
    ),
  
  # Piccadilly line strike compare
  # 3. line level weekday
  low_bikeNdock_avai_time_series_func(bike_avai,bike_station_dist_piccadilly,"2018-10-04","2018-09-20","2018-09-27",0.15,"weekday")+
    scale_y_continuous(name="service pressure",limits = c(1,6))+
    ggtitle("(b) Weekday Piccadilly line strike:\n     Around Piccadilly line")+theme(plot.title = element_text(size=10),legend.position = "none",axis.title.y = element_blank()),
  
  
  # CW line strike compare
  # 1. network level weekday
  low_bikeNdock_avai_time_series_func(bike_avai,bike_station_dist_CW,"2015-07-02","2015-07-16","2015-07-09",0.15,"weekday")+
    scale_y_continuous(name="service pressure",limits = c(1,6))+
    ggtitle("(c) Weekday network-level strike:\n     Around Central line and Waterloo & City line ")+
    theme(plot.title = element_text(size=10),
          legend.position = c(1,-0.3),
          #legend.title.align = 5,
          legend.direction = "horizontal",
          plot.margin=unit(c(0,0,1.5,0),"cm")),
  
  # CW line strike compare
  # 4. line level weekday
  low_bikeNdock_avai_time_series_func(bike_avai,bike_station_dist_CW,"2018-10-12","2018-10-19","2018-10-05",0.15,"weekday")+
    scale_y_continuous(name="timestamps count ",limits = c(1,6))+
    ggtitle("(d) Weekday Central line and Waterloo & City line strike:\n     Around Central line and Waterloo & City line")+theme(plot.title = element_text(size=10),legend.position = "none",axis.title.y = element_blank()),
  
  nrow=2

)
avail_line_strike

ggsave("figures/Low_Avai_Bike&Dock_timeseries_LineLVL.png",plot = plot_save5,
       width = 26,height = 18,units = "cm",dpi=300)




