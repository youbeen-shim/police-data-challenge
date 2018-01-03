########################################## Police Data Challenge ##########################################

library(tidyverse)
library(lubridate)
library(stringr)
#library(ggmap)

######################### Data Cleaning #########################
##### import datasets
setwd("~/Desktop/Catherine Sun/Fall 2017/STAT 4310/Project")
calls <- read_csv("911_Calls_for_Service.csv")
arrests <- read_csv("BPD_arrests.csv")
victims <- read_csv("BPD_victims.csv")
stations <- read_csv("Police Stations.csv")
guns <- read_csv("Gun_Offenders.csv")
officers <- read_csv("Employee.csv")

#force <- read_csv("Use of Force.csv")
#arrests <- read_csv("Arrests.csv")
#victims <- read_csv("Victim Based Crime1.csv")

##### finalizing datasets
#datetime and separating into date and time columns
#locations separated into long and lat
#----- digits=0
calls_final<-calls %>% 
  mutate(datetime=mdy_hms(callDateTime, tz=Sys.timezone())) %>%
  filter(!is.na(callDateTime) & !is.na(location)) %>% #remove na's of datetime & location
  separate(location, c('lat', 'long'), sep=",") %>% #separating location
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=0), long = round(long, digits=0)) %>%
  select(datetime, priority, district, incidentLocation, long, lat, description) %>%
  filter(year(datetime)>=2015) #only 2015-2017

arrests_final<-arrests %>% #ArrestDate & ArrestTime already datetime
  mutate(datetime=mdy_hms(paste(ArrestDate, ArrestTime), tz=Sys.timezone())) %>%
  filter(!is.na(datetime) & !is.na(`Location 1`)) %>% #remove na's of datetime & location
  separate(`Location 1`, c('lat', 'long'), sep=", ") %>% #separating Location 1
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=0), long = round(long, digits=0)) %>%
  select(datetime, Age, Sex, Race, ArrestLocation, IncidentLocation, District, Neighborhood, 
         long, lat, IncidentOffense, ChargeDescription) %>%
  filter(year(datetime)>=2015, (!is.na(lat) & !is.na(long)))

victims_final<-victims %>% #CrimeDate & CrimeTime are characters
  mutate(datetime=mdy_hms(paste(CrimeDate, CrimeTime), tz=Sys.timezone())) %>%
  filter(!is.na(datetime) & !is.na(`Location 1`)) %>% #remove na's of datetime & location
  separate(`Location 1`, c('lat', 'long'), sep=", ") %>% #separating Location 1
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=0), long = round(long, digits=0)) %>%
  select(datetime, Location, District, Neighborhood, long, lat, Description, Weapon) %>% #Inside/Outside not found
  filter(year(datetime)>=2015) #only 2015-2017

guns_final<-guns %>% #CrimeDate & CrimeTime are characters
  mutate(datetime=mdy_hms(created_date, tz=Sys.timezone())) %>%
  filter(!is.na(datetime) & !is.na(`Location 1`), city=="Baltimore") %>% #remove na's of datetime & location
  mutate(lat = round(Latitude, digits=0), long = round(Longitude, digits=0)) %>%
  select(datetime, full_address, district, neighborhood, long, lat, Date_Of_Birth, sex, race) %>% #Inside/Outside not found
  filter(year(datetime)>=2015) #only 2015-2017

stations_final<-stations

officers_final<-officers %>%
  filter(AGENCY=="BPD")
#------------------ FINE TUNING ------------------
#location = 4 decimals places
calls_final1<-calls %>% 
  mutate(datetime=mdy_hms(callDateTime, tz=Sys.timezone())) %>%
  separate(location, c('lat', 'long'), sep=",") %>% #separating location
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=4), long = round(long, digits=4)) %>%
  select(datetime, priority, district, incidentLocation, long, lat, description) %>%
  filter(year(datetime)>=2015) #only 2015-2017

arrests_final1<-arrests %>% #ArrestDate & ArrestTime already datetime
  mutate(datetime=mdy_hms(paste(ArrestDate, ArrestTime), tz=Sys.timezone())) %>%
  separate(`Location 1`, c('lat', 'long'), sep=", ") %>% #separating Location 1
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=4), long = round(long, digits=4)) %>%
  select(datetime, Age, Sex, Race, ArrestLocation, IncidentLocation, District, Neighborhood, 
         long, lat, IncidentOffense, ChargeDescription) %>%
  filter(year(datetime)>=2015, (!is.na(lat) & !is.na(long)))

victims_final1<-victims %>% #CrimeDate & CrimeTime are characters
  mutate(datetime=mdy_hms(paste(CrimeDate, CrimeTime), tz=Sys.timezone())) %>%
  separate(`Location 1`, c('lat', 'long'), sep=", ") %>% #separating Location 1
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=4), long = round(long, digits=4)) %>%
  select(datetime, Location, District, Neighborhood, long, lat, Description, Weapon) %>% #Inside/Outside not found
  filter(year(datetime)>=2015) #only 2015-2017

#location = 0 decimal places
calls_final2<-calls %>% 
  mutate(datetime=mdy_hms(callDateTime, tz=Sys.timezone())) %>%
  separate(location, c('lat', 'long'), sep=",") %>% #separating location
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=0), long = round(long, digits=0)) %>%
  select(datetime, priority, district, incidentLocation, long, lat, description) %>%
  filter(year(datetime)>=2015) #only 2015-2017

arrests_final2<-arrests %>% #ArrestDate & ArrestTime already datetime
  mutate(datetime=mdy_hms(paste(ArrestDate, ArrestTime), tz=Sys.timezone())) %>%
  separate(`Location 1`, c('lat', 'long'), sep=", ") %>% #separating Location 1
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=0), long = round(long, digits=0)) %>%
  select(datetime, Age, Sex, Race, ArrestLocation, IncidentLocation, District, Neighborhood, 
         long, lat, IncidentOffense, ChargeDescription) %>%
  filter(year(datetime)>=2015, (!is.na(lat) & !is.na(long)))

victims_final2<-victims %>% #CrimeDate & CrimeTime are characters
  mutate(datetime=mdy_hms(paste(CrimeDate, CrimeTime), tz=Sys.timezone())) %>%
  separate(`Location 1`, c('lat', 'long'), sep=", ") %>% #separating Location 1
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=0), long = round(long, digits=0)) %>%
  select(datetime, Location, District, Neighborhood, long, lat, Description, Weapon) %>% #Inside/Outside not found
  filter(year(datetime)>=2015) #only 2015-2017

#------------------ FINE TUNING ------------------
#digits = 1
count(unique(calls_final)) #2896633 
count(unique(arrests_final)) #35841
count(unique(victims_final)) #131227
#digits = 4
count(unique(calls_final1)) #2896696
count(unique(arrests_final1)) #35841
count(unique(victims_final1)) #131262

#testing location to maximize data
temp<-calls_final %>% #testing how close long and lat have to be between datasets
  inner_join(arrests_final, c("long", "lat", "date", "time"))
temp %>% filter((abs(long.y-long.x)<=0.05) & (abs(lat.y==lat.x)<=0.05))
#treat calls from same location but different datetime as separate calls
#----- digits=5
calls_final5<-calls %>% 
  mutate(datetime=mdy_hms(callDateTime, tz=Sys.timezone())) %>%
  filter(!is.na(callDateTime) & !is.na(location)) %>% #remove na's of datetime & location
  separate(location, c('lat', 'long'), sep=",") %>% #separating location
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=5), long = round(long, digits=5)) %>%
  select(datetime, priority, district, incidentLocation, long, lat, description) %>%
  filter(year(datetime)>=2015) #only 2015-2017

arrests_final5<-arrests %>% #ArrestDate & ArrestTime already datetime
  mutate(datetime=mdy_hms(paste(ArrestDate, ArrestTime), tz=Sys.timezone())) %>%
  filter(!is.na(datetime) & !is.na(`Location 1`)) %>% #remove na's of datetime & location
  separate(`Location 1`, c('lat', 'long'), sep=", ") %>% #separating Location 1
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=5), long = round(long, digits=5)) %>%
  select(datetime, Age, Sex, Race, ArrestLocation, IncidentLocation, District, Neighborhood, 
         long, lat, IncidentOffense, ChargeDescription) %>%
  filter(year(datetime)>=2015, (!is.na(lat) & !is.na(long)))

victims_final5<-victims %>% #CrimeDate & CrimeTime are characters
  mutate(datetime=mdy_hms(paste(CrimeDate, CrimeTime), tz=Sys.timezone())) %>%
  filter(!is.na(datetime) & !is.na(`Location 1`)) %>% #remove na's of datetime & location
  separate(`Location 1`, c('lat', 'long'), sep=", ") %>% #separating Location 1
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=5), long = round(long, digits=5)) %>%
  select(datetime, Location, District, Neighborhood, long, lat, Description, Weapon) %>% #Inside/Outside not found
  filter(year(datetime)>=2015) #only 2015-2017

guns_finals5<-guns %>% #CrimeDate & CrimeTime are characters
  mutate(datetime=mdy_hms(created_date, tz=Sys.timezone())) %>%
  filter(!is.na(datetime) & !is.na(`Location 1`), city=="Baltimore") %>% #remove na's of datetime & location
  mutate(lat = round(Latitude, digits=5), long = round(Longitude, digits=5)) %>%
  select(datetime, full_address, district, neighborhood, long, lat, Date_Of_Birth, sex, race) %>% #Inside/Outside not found
  filter(year(datetime)>=2015) #only 2015-2017

stations_final5<-stations

officers_final5<-officers %>%
  filter(AGENCY=="BPD")

##### explore
calls_final %>% group_by(year(datetime)) %>%
  summarize(count=n()) #2015-2017
arrests_final %>% group_by(year(datetime)) %>%
  summarize(count=n())
victims_final %>% group_by(year(datetime)) %>%
  summarize(count=n())

##### Joining Datasets
#digits = 0
count(unique(calls_final)) #2896604
count(unique(arrests_final)) #35841
count(unique(victims_final)) #131227
#treat calls from same location but different datetime as separate calls

#digits = 5
count(unique(calls_final5)) #2896716 
count(unique(arrests_final5)) #35841
count(unique(victims_final5)) #130474

#----- innerjoining
calls.arrests<-calls_final %>%
  inner_join(arrests_final, c("long", "lat", "datetime")) %>%
  filter(tolower(incidentLocation)==tolower(IncidentLocation)) %>%
  select(datetime, priority, long, lat, incidentLocation, district, Neighborhood, ArrestLocation, 
         Age, Sex, Race, description, ChargeDescription, IncidentOffense)
#change all strings to lowercase

calls.victims<-calls_final %>%
  inner_join(victims_final, c("long", "lat", "datetime")) %>%
  filter(tolower(incidentLocation)==tolower(Location)) %>%
  select(datetime, priority, long, lat, incidentLocation, district, Neighborhood, description, Description, 
         Weapon)

arrests.victims<-arrests_final %>%
  inner_join(victims_final, c("long", "lat", "datetime")) %>%
  filter(tolower(IncidentLocation)==tolower(Location)) %>%
  select(datetime, long, lat, IncidentLocation, District.x, Neighborhood.x, ArrestLocation, Age, Sex, Race, 
         IncidentOffense, ChargeDescription, Description, Weapon)

######################### Data Analysis #########################
arrests_final %>% group_by(Sex) %>%
  summarize(count=n())
arrests_final %>% group_by(Sex, Race) %>%
  summarize(count=n())

#----- neighborhoods & districts 
calls_final %>% group_by(district) %>%
  summarize(count=n())

arrests_final %>% group_by(District, Neighborhood) %>%
  summarize(count=n())

victims_final %>% group_by(District, Neighborhood) %>%
  summarize(count=n())

######################### Visualizations #########################
##### CALLS & ARRESTS
#----- most dangerous neighborhoods (proportion graph) -- NOT FINISHED
#https://www.roadsnacks.net/these-are-the-10-worst-baltimore-neighborhoods/
calls.arrests %>% mutate(Neighborhood=tolower(Neighborhood)) %>% 
  group_by(district, Neighborhood) %>%
  filter(Neighborhood %in% c("monument street area", "orangeville", "cherry hill", "fairfield", 
                             "greenmount east", "greater rosemont", "madison-eastend", "berea", 
                             "grove park", "west baltimore")) %>%
  ggplot(aes(x=factor(Neighborhood))) + 
  xlab("District of Baltimore") + ylab("Number of Calls") + 
  labs(title="Number of Calls per District per Neighborhood") + 
  geom_bar(stat="count", position="dodge") + 
  theme_minimal()

##### CALLS
calls_visual<-calls_final %>% 
  select(datetime, priority, district) %>%
  mutate(hour=hour(datetime)) %>% #new column for hour of the day
  mutate(day=wday(datetime, label=TRUE, abbr=TRUE)) %>% #new column for day of week
  mutate(priority=factor(priority))

#----- Priority Level of Calls to BPD Per Time
calls_visual %>% group_by(hour, priority) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=hour, y=count)) + geom_line(aes(color=priority), size=1) + theme_minimal() +
  xlab("Time of the Day (Military Time)") + ylab("Number of Calls") +
  labs(title="Priority Level of Calls to Baltimore Police Throughout the Day") + 
  scale_color_brewer(name="Level of Priority", palette="Set1")

#----- Priority Level of Calls to BPD Per Day
calls_visual %>% group_by(day, priority) %>%
  ggplot(aes(x=factor(day), fill=factor(priority))) + 
  xlab("Day of the Week") + ylab("Number of Calls") + 
  labs(title="Priority Level of Calls to Baltimore Police For Each Day") + 
  geom_bar(stat="count", position="dodge") + 
  theme_minimal()

#----- # Calls to BPD Per Hour Per Day
calls_visual %>% group_by(day, hour) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=hour, y=count)) + geom_line(aes(color=day), size=1) + theme_minimal() +
  xlab("Time of the Day (Military Time)") + ylab("Number of Calls") +
  labs(title="Number of Calls to Baltimore Police Throughout the Day For Each Day of the Week") + 
  scale_color_brewer(name="Day of the Week", palette="Set1")

##### ARRESTS
arrests_visual<- arrests_final %>% 
  select(datetime, Age, Race, Sex, District) %>%
  mutate(hour=hour(datetime)) %>% #new column for hour of the day
  mutate(day=wday(datetime, label=TRUE, abbr=TRUE)) #new column for day of week

#----- What proportion of races have been getting arrested in each district?
arrests_visual %>%
  group_by(Race, District) %>%
  summarize(count=n()) %>%
  filter(District == "Southern" | District == "Central" | District == "Western") %>%
  ggplot(aes(x=District, fill=Race)) +
  geom_bar(stat="count", position=position_dodge(), color="black") +
  labs(title="Amount of Different Races \n getting Arrested in Different Baltimore Districts") +
  xlab("Baltimore Districts") + ylab("Amount of People Arrested") + theme_minimal()

#----- Based on info below, let's look at same as above but for the three most dangerous districts
arrests_visual %>%
  group_by(Race, District) %>%
  filter(District == "Southern" | District == "Central" | District == "Western") %>%
  ggplot(aes(x=District, fill=Race)) +
  geom_bar(stat="count", position=position_dodge(), color="black") +
  labs(title="Amount of Different Races \n getting Arrested in the Dangerous Baltimore Districts") +
  xlab("Dangerous Baltimore Districts") + ylab("Amount of People Arrested") + theme_minimal()

##### VICTIMS: datetime, Location, District, Neighborhood, long, lat, Description, Weapon
victims_visual <- victims_final %>%
  select(datetime, District, Weapon) %>%
  mutate(hour=hour(datetime)) %>% #new column for hour of the day
  mutate(day=wday(datetime, label=TRUE, abbr=TRUE)) #new column for day of week

#----- What kind of weapons involved in crime were found known dangerous districts?
# https://dubofflawgroup.com/blog/violent-crimes/
# Investigate individual districts known to be most violent:
# Southern, Centeral, Western.. later compare to "safe" places
victims_visual %>%
  group_by(District, Weapon) %>%
  #summarize(count=n()) %>%
  filter(District == "SOUTHERN" | District == "CENTRAL" | District == "WESTERN") %>%
  filter(Weapon != "NA") %>% # want to look at just weapons right now
  ggplot(aes(x=District, fill = Weapon)) + 
  geom_bar(stat="count", position=position_dodge(), color="black") +
  labs(title="Weapons involved in Crimes from Dangerous \n Baltimore Districts") +
  xlab("Dangerous Baltimore Districts") + 
  ylab("Counts of Weapon Use") + theme_minimal() 

##### MAPS
library(ggmap)
baltimore <- get_map("baltimore", zoom =12, maptype = "roadmap")

victims_final_map <- victims %>%
  mutate(datetime=mdy_hms(paste(CrimeDate, CrimeTime), tz=Sys.timezone())) %>%
  # filter at start instead of end to not waste time
  filter(year(datetime)>=2015) %>%
  # Additional cleaning: got rid of more NA values
  filter(!is.na(datetime) & !is.na(`Location 1`) &  !is.na(Weapon) & !is.na(District)) %>%
  separate(`Location 1`, c('lat', 'long'), sep=", ") %>%
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>%
  # Below is the only change I made, lat and log now has 5 decimal points
  mutate(lat = round(lat, digits=5), long = round(long, digits=5)) %>%
  # Mutate to identify districts
  mutate(Groupings = factor(District)) %>%
  mutate(Groupings = as.numeric(Groupings, levels=c("NORTHWESTERN","SOUTHWESTERN","CENTRAL","EASTERN",
                                                    "SOUTHERN","SOUTHEASTERN","WESTERN","NORTHERN","NORTHEASTERN")))  %>%
  select(datetime, Location, District, Neighborhood, long, lat, Description, Weapon, Groupings)

#----- firearm victims by hour 
victims_map <- victims_final_map %>%
  select(datetime, Weapon, long, lat) %>%
  mutate(hour=hour(datetime)) %>%
  filter(Weapon == "FIREARM")

ggmap(baltimore) +
  geom_point(aes(x=long,y=lat,color=factor(hour)), data=victims_map, size=1) +
  theme_void()

#----- crimes by weapon
ggmap(baltimore) +
  geom_point(aes(x=long,y=lat,color=Weapon), data=victims_final_map, size=.7) +
  theme_void()

#----- districts
ggmap(baltimore) +
  geom_polygon(data=victims_final_map, aes(x=long, y=lat, fill=District, group=Groupings), color = NA)

#----- stations
ggmap(baltimore) +
  stat_density_2d(aes(x=long, y=lat, fill=..level..), data=victims_final_map, geom = "polygon", alpha=.45) +
  geom_polygon(aes(x = long, y = lat, group = group), data = area.points, color = colors[9], fill = colors[6], alpha = 0.5) +
  geom_point(aes(x=-76.608363,y=39.28998,color="Central"), size=3.20968*3) +
  geom_point(aes(x=-76.644862,y=39.34465,color="Northern"), size=2.92746*3) +
  geom_point(aes(x=-76.582786,y=39.340745,color="NorthEastern"), size=4.26857*3) +
  geom_point(aes(x=-76.685428,y=39.344630,color="NorthWestern"), size=2.91492*3) +
  geom_point(aes(x=-76.573456,y=39.310015,color="Eastern"), size=2.73589*3) +
  geom_point(aes(x=-76.617214,y=39.2528705,color="Southern"), size=3.24564*3) +
  geom_point(aes(x=-76.547454,y=39.287755,color="SouthEastern"), size=3.17723*3) +
  geom_point(aes(x=-76.663885,y=39.278366,color="SouthWestern"), size=3.22828*3) +
  geom_point(aes(x=-76.644900,y=39.300671,color="Western"), size=2.76957*3) +
  labs(title="Density of Crime and Police Stations")
  theme_void()

guns_final[!is.na(guns_final$district),]%>%
  group_by(district, race)%>%
  ggplot(aes(x=district))+
  labs(title="Gun Offender's District and Race") +
  geom_bar(aes(fill=race), position="dodge")

ggmap(baltimore)+
  labs(title="Gun Offenders by Location and Race")+
  geom_point(data=guns_final, aes(x=long, y=lat, colour=race)) #didnt get points for this graph