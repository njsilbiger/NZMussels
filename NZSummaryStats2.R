
## Calculate and export Mean number of days per year where temperature #####
# exceeds LT50  for > 6 hours. Means are based on temperature data while ###
# exposed to air and collected from 1997 - 2009.                        ###
# Created by Nyssa Silbiger                                           #####
# Temperature modeling by Allison Smith
# https://github.com/kallisons/NZMusselTemperatures/tree/master/processedoutput
# Edited on 5/11/2018                                    ##################

#Calculated LT50 values for NR and BT populations were, respectively, 31.8 and 33.3ºC for Perna, 
# 31.8 and 33.1ºC for Aulacomya, and 35.2 and 37.9ºC for Mytilus  

library(tidyverse)

############################# BT historical ####################################

# find all the filenames
path.p<-'processedoutput/historical/NZSIBT/'
file.names<-basename(list.files(path = path.p, pattern = "txt$", recursive = TRUE)) #list all csv file names in the folder and subfolders

# create an empty tibble to fill in
DaysAboveLT50BT<-tibble('Pern'=rep(NA, length(file.names)),'Aul'=rep(NA, length(file.names)),'Myt'=rep(NA, length(file.names)), 'TideHeight' = seq(50,250,10))
DaysAboveLT50BT.sd<-tibble('Pern'=rep(NA, length(file.names)),'Aul'=rep(NA, length(file.names)),'Myt'=rep(NA, length(file.names)), 'TideHeight' = seq(50,250,10))

# loop over all the tide height files
for (i in 1:length(file.names)){

  # read in the file names one at a time
data<-read.table(paste0(path.p,file.names[i]), header = TRUE)

# remove all data that is not during low tide
data<-data[data$tideflag==1,]

Date<-data$Date
# extract the years
data<-data %>% separate(Date, c("Year", "Day","Month"), sep = "\\-")
# add the full date back
data$Date<-Date

# remove 2010 data since it is not complete
data<-data[!data$Year==2010,]

# remove all rows with NAs
data<-data[complete.cases(data),]

# these are the Lt50s for box thumb
temps<-c(33.3,33.1,37.9)

# function to count how many hours above a temperature
count_hours <-function(x,y){
  length(which(x>y))/2
}

# function to count how many days per year are above 6 hours
count_6hour<-function(x){
  length(which(x>=6))
}

# j is index for the 3 LT50s and i is index fot the tide height
for (j in 1:length(temps)){
  DaysAboveLT50BT[i,j]<- data %>% 
  group_by(Date, Year)%>% # group by date and year
  summarise(counthours = count_hours(Temp,temps[j]))%>% # count how many hours above LT50 per day
  group_by(Year)%>% # now group by year
  summarise(countdays = count_6hour(counthours))%>% # now average number of days per year
  summarise(meandays = mean(countdays))
  
}
# for SD
# j is index for the 3 LT50s and i is index fot the tide height
for (j in 1:length(temps)){
  DaysAboveLT50BT.sd[i,j]<- data %>% 
    group_by(Date, Year)%>% # group by date and year
    summarise(counthours = count_hours(Temp,temps[j]))%>% # count how many hours above LT50 per day
    group_by(Year)%>% # now group by year
    summarise(countdays = count_6hour(counthours))%>% # now average number of days per year
    summarise(SDdays = sd(countdays))
  
}
}

# write out the data
DaysAboveLT50BT<-DaysAboveLT50BT%>%arrange(desc(TideHeight)) # arrange the tide height in descending order
write.csv(DaysAboveLT50BT, 'SummaryOutput/DaysAboveLT50BT.csv')
DaysAboveLT50BT.sd<-DaysAboveLT50BT.sd%>%arrange(desc(TideHeight)) # arrange the tide height in descending order
write.csv(DaysAboveLT50BT.sd, 'SummaryOutput/DaysAboveLT50BT.sd.csv')

######################## BT Future ######################################
# find all the filenames
path.p<-'processedoutput/future/NZSIBT/'
file.names<-basename(list.files(path = path.p, pattern = "txt$", recursive = TRUE)) #list all csv file names in the folder and subfolders

# create an empty tibble to fill in
DaysAboveLT50BT_future<-tibble('Pern'=rep(NA, length(file.names)),'Aul'=rep(NA, length(file.names)),'Myt'=rep(NA, length(file.names)), 'TideHeight' = seq(50,250,10))
DaysAboveLT50BT_future.sd<-tibble('Pern'=rep(NA, length(file.names)),'Aul'=rep(NA, length(file.names)),'Myt'=rep(NA, length(file.names)), 'TideHeight' = seq(50,250,10))

for (i in 1:length(file.names)){
  
  # read in the file names one at a time
  data<-read.table(paste0(path.p,file.names[i]), header = TRUE)
   
  # remove all data that is not during low tide
  data<-data[data$tideflag==1,]
  
  Date<-data$Date
  # extract the years
  data<-data %>% separate(Date, c("Year", "Day","Month"), sep = "\\-")
  # add the full date back
  data$Date<-Date
  
  # remove 2010 data since it is not complete
  data<-data[!data$Year==2010,]
  
  # remove all rows with NAs
  data<-data[complete.cases(data),]

  # j is index for the 3 LT50s and i is index fot the tide height
  for (j in 1:length(temps)){
    DaysAboveLT50BT_future[i,j]<- data %>% 
      group_by(Date, Year)%>% # group by date and year
      summarise(counthours = count_hours(Temp,temps[j]))%>% # count how many hours above LT50 per day
      group_by(Year)%>% # now group by year
      summarise(countdays = count_6hour(counthours))%>% # now average number of days per year
      summarise(meandays = mean(countdays))
    
  }
  #Sd
  for (j in 1:length(temps)){
    DaysAboveLT50BT_future.sd[i,j]<- data %>% 
      group_by(Date, Year)%>% # group by date and year
      summarise(counthours = count_hours(Temp,temps[j]))%>% # count how many hours above LT50 per day
      group_by(Year)%>% # now group by year
      summarise(countdays = count_6hour(counthours))%>% # now average number of days per year
      summarise(SDdays = sd(countdays))
    
  }
}

# write out the data
DaysAboveLT50BT_future<-DaysAboveLT50BT_future%>%arrange(desc(TideHeight)) # arrange the tide height in descending order
write.csv(DaysAboveLT50BT_future, 'SummaryOutput/DaysAboveLT50BT_future.csv')
DaysAboveLT50BT_future.sd<-DaysAboveLT50BT_future.sd%>%arrange(desc(TideHeight)) # arrange the tide height in descending order
write.csv(DaysAboveLT50BT_future.sd, 'SummaryOutput/DaysAboveLT50BT_futuresd.csv')

############################# NR historical ####################################

# find all the filenames
path.p<-'processedoutput/historical/NZSINR/'
file.names<-basename(list.files(path = path.p, pattern = "txt$", recursive = TRUE)) #list all csv file names in the folder and subfolders

# create an empty tibble to fill in
DaysAboveLT50NR<-tibble('Pern'=rep(NA, length(file.names)),'Aul'=rep(NA, length(file.names)),'Myt'=rep(NA, length(file.names)), 'TideHeight' = seq(50,250,10))
DaysAboveLT50NR.sd<-tibble('Pern'=rep(NA, length(file.names)),'Aul'=rep(NA, length(file.names)),'Myt'=rep(NA, length(file.names)), 'TideHeight' = seq(50,250,10))

for (i in 1:length(file.names)){
  
  # read in the file names one at a time
  data<-read.table(paste0(path.p,file.names[i]), header = TRUE)

  # remove all data that is not during low tide
  data<-data[data$tideflag==1,]
  
  Date<-data$Date
  # extract the years
  data<-data %>% separate(Date, c("Year", "Day","Month"), sep = "\\-")
  # add the full date back
  data$Date<-Date
  
  # remove 2010 data since it is not complete
  data<-data[!data$Year==2010,]
  
  # remove all rows with NAs
  data<-data[complete.cases(data),]
  
  temps<-c(31.8,31.8,35.2) # LT50 for NR for each species

  # j is index for the 3 LT50s and i is index fot the tide height
  for (j in 1:length(temps)){
    DaysAboveLT50NR[i,j]<- data %>% 
      group_by(Date, Year)%>% # group by date and year
      summarise(counthours = count_hours(Temp,temps[j]))%>% # count how many hours above LT50 per day
      group_by(Year)%>% # now group by year
      summarise(countdays = count_6hour(counthours))%>% # now average number of days per year
      summarise(meandays = mean(countdays))
    
  }


### SD#####
for (j in 1:length(temps)){
  DaysAboveLT50NR.sd[i,j]<- data %>% 
    group_by(Date, Year)%>% # group by date and year
    summarise(counthours = count_hours(Temp,temps[j]))%>% # count how many hours above LT50 per day
    group_by(Year)%>% # now group by year
    summarise(countdays = count_6hour(counthours))%>% # now average number of days per year
    summarise(SDdays = sd(countdays))
  
}
}
# write out the data
DaysAboveLT50NR<-DaysAboveLT50NR%>%arrange(desc(TideHeight)) # arrange the tide height in descending order
write.csv(DaysAboveLT50NR, 'SummaryOutput/DaysAboveLT50NR.csv')

# write out the data
DaysAboveLT50NR.sd<-DaysAboveLT50NR.sd%>%arrange(desc(TideHeight)) # arrange the tide height in descending order
write.csv(DaysAboveLT50NR.sd, 'SummaryOutput/DaysAboveLT50NRsd.csv')


######################## NR Future ######################################
# find all the filenames
path.p<-'processedoutput/future/NZSINR/'
file.names<-basename(list.files(path = path.p, pattern = "txt$", recursive = TRUE)) #list all csv file names in the folder and subfolders

# create an empty tibble to fill in
DaysAboveLT50NR_future<-tibble('Pern'=rep(NA, length(file.names)),'Aul'=rep(NA, length(file.names)),'Myt'=rep(NA, length(file.names)), 'TideHeight' = seq(50,250,10))
DaysAboveLT50NR_future.sd<-tibble('Pern'=rep(NA, length(file.names)),'Aul'=rep(NA, length(file.names)),'Myt'=rep(NA, length(file.names)), 'TideHeight' = seq(50,250,10))

for (i in 1:length(file.names)){
  
  # read in the file names one at a time
  data<-read.table(paste0(path.p,file.names[i]), header = TRUE)
  
  # remove all data that is not during low tide
  data<-data[data$tideflag==1,]
  
  Date<-data$Date
  # extract the years
  data<-data %>% separate(Date, c("Year", "Day","Month"), sep = "\\-")
  # add the full date back
  data$Date<-Date
  
  # remove 2010 data since it is not complete
  data<-data[!data$Year==2010,]
  
  # remove all rows with NAs
  data<-data[complete.cases(data),]
  
  # j is index for the 3 LT50s and i is index fot the tide height
  for (j in 1:length(temps)){
    DaysAboveLT50NR_future[i,j]<- data %>% 
      group_by(Date, Year)%>% # group by date and year
      summarise(counthours = count_hours(Temp,temps[j]))%>% # count how many hours above LT50 per day
      group_by(Year)%>% # now group by year
      summarise(countdays = count_6hour(counthours))%>% # now average number of days per year
      summarise(meandays = mean(countdays))
    
  }
  #sd
  for (j in 1:length(temps)){
    DaysAboveLT50NR_future.sd[i,j]<- data %>% 
      group_by(Date, Year)%>% # group by date and year
      summarise(counthours = count_hours(Temp,temps[j]))%>% # count how many hours above LT50 per day
      group_by(Year)%>% # now group by year
      summarise(countdays = count_6hour(counthours))%>% # now average number of days per year
      summarise(SDdays = sd(countdays))
    
  }
}

# write out the data
DaysAboveLT50NR_future<-DaysAboveLT50NR_future%>%arrange(desc(TideHeight)) # arrange the tide height in descending order
write.csv(DaysAboveLT50NR_future, 'SummaryOutput/DaysAboveLT50NR_future.csv')

DaysAboveLT50NR_future.sd<-DaysAboveLT50NR_future.sd%>%arrange(desc(TideHeight)) # arrange the tide height in descending order
write.csv(DaysAboveLT50NR_future.sd, 'SummaryOutput/DaysAboveLT50NR_futuresd.csv')


