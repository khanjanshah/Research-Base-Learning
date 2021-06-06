library(tidyverse)
library(leaflet)
library(stringr)
library(rgdal)
library(lubridate)
library(forecast)
library(DT)
library(prophet)
library(caret)

rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

LACrime = read_csv("D:/VIT/VIT2020SEM2/SET-2/Done/Crime_Data_2010_2017.csv")


# seperating lat and long

LACrime_latlong = gsub('\\(','',LACrime$Location)
LACrime_latlong = gsub('\\)','',LACrime_latlong)

LACrime_latlong = str_split(LACrime_latlong,",")

LACrime_latlong = do.call(rbind.data.frame, LACrime_latlong)

colnames(LACrime_latlong) = c("latitude","longitude")

LACrime$latitude = as.numeric(as.character(LACrime_latlong$latitude))
LACrime$longitude = as.numeric(as.character(LACrime_latlong$longitude))

rm(LACrime_latlong)

LACrime = LACrime %>%
  select(-Location)

# Rename the Fields to remove Spaces in between Fields
LACrime = LACrime %>%
  rename(DRNumber = `DR Number`) %>%
  rename(DateReported = `Date Reported`) %>%
  rename(DateOccurred = `Date Occurred`) %>%
  rename(TimeOccurred = `Time Occurred`) %>%
  rename(AreaID = `Area ID`) %>%
  rename(AreaName = `Area Name`) %>%
  rename(ReportingDistrict = `Reporting District`) %>%
  rename(CrimeCode = `Crime Code`) %>%
  rename(CrimeCodeDescription = `Crime Code Description`) %>%
  rename(MOCodes = `MO Codes`) %>%
  rename(VictimAge = `Victim Age`) %>%
  rename(VictimSex = `Victim Sex`) %>%
  rename(VictimDescent = `Victim Descent`) %>%
  rename(PremiseCode = `Premise Code`) %>%
  rename(PremiseDescription = `Premise Description`) %>%
  rename(WeaponUsedCode = `Weapon Used Code`) %>%
  rename(WeaponDescription = `Weapon Description`) %>%
  rename(StatusCode = `Status Code`) %>%
  rename(StatusDescription = `Status Description`) %>%
  rename(CrimeCode1 = `Crime Code 1`) %>%
  rename(CrimeCode2 = `Crime Code 2`) %>%
  rename(CrimeCode3 = `Crime Code 3`) %>%
  rename(CrimeCode4 = `Crime Code 4`) %>%
  rename(CrossStreet = `Cross Street`) 


# month of crime                                                     

LACrime$MonthOfCrime =  month.abb[month(mdy(LACrime$DateOccurred))]

LACrime %>%
  group_by(MonthOfCrime) %>%
  summarise(CountIncidents = n()) %>%
  mutate(MonthOfCrime = reorder(MonthOfCrime,CountIncidents)) %>%
  
  ggplot(aes(x = MonthOfCrime,y = CountIncidents)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = MonthOfCrime, y = 1, label = paste0("(",CountIncidents,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Month Of Crime', y = 'Count of Incidents', 
       title = 'Count of Incidents') +
  coord_flip() + 
  theme_bw()

# Day of crime

LACrime$DayOfCrime =  wday(mdy(LACrime$DateOccurred),label = TRUE)

LACrime %>%
  group_by(DayOfCrime) %>%
  summarise(CountIncidents = n()) %>%
  mutate(DayOfCrime = reorder(DayOfCrime,CountIncidents)) %>%
  
  ggplot(aes(x = DayOfCrime,y = CountIncidents)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = DayOfCrime, y = 1, label = paste0("(",CountIncidents,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Day Of Crime', y = 'Count of Incidents', 
       title = 'Count of Incidents') +
  coord_flip() + 
  theme_bw()

# Time of crime
## Most crimes 
LACrime %>%
  group_by(TimeOccurred) %>%
  summarise(CountIncidents = n()) %>%
  arrange(desc(CountIncidents)) %>%
  mutate(TimeOccurred = as.integer(TimeOccurred)) %>%
  mutate(TimeOccurred = reorder(TimeOccurred,CountIncidents)) %>%
  head(10) %>%
  
  ggplot(aes(x = TimeOccurred,y = CountIncidents)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = TimeOccurred, y = 1, label = paste0("(",CountIncidents,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Time Of Crime', y = 'Count of Incidents', 
       title = 'Count of Incidents') +
  coord_flip() + 
  theme_bw()

## Least crimes

LACrime %>%
  group_by(TimeOccurred) %>%
  summarise(CountIncidents = n()) %>%
  arrange(desc(CountIncidents)) %>%
  mutate(TimeOccurred = as.integer(TimeOccurred)) %>%
  mutate(TimeOccurred = reorder(TimeOccurred,CountIncidents)) %>%
  tail(10) %>%
  
  ggplot(aes(x = TimeOccurred,y = CountIncidents)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = TimeOccurred, y = 1, label = paste0("(",CountIncidents,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Time Of Crime', y = 'Count of Incidents', 
       title = 'Count of Incidents') +
  coord_flip() + 
  theme_bw()

## type at 1200 hrs.

LACrime %>%
  filter(TimeOccurred == 1200) %>%
  group_by(CrimeCodeDescription) %>%
  summarise(CountIncidents = n()) %>%
  arrange(desc(CountIncidents)) %>%
  mutate(CrimeCodeDescription = reorder(CrimeCodeDescription,CountIncidents)) %>%
  head(10) %>%
  
  ggplot(aes(x = CrimeCodeDescription,y = CountIncidents)) +
  geom_bar(stat='identity',colour="white", fill = c("red")) +
  geom_text(aes(x = CrimeCodeDescription, y = 1, label = paste0("(",CountIncidents,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'white',
            fontface = 'bold') +
  labs(x = 'Time Of Crime', y = 'Count of Incidents', 
       title = 'Type of Crime at 1200 hrs') +
  coord_flip() + 
  theme_bw()

## sex of victims

LACrime %>%
  filter(!is.na(VictimSex)) %>%
  group_by(VictimSex) %>%
  tally() %>%
  ungroup() %>%
  mutate(VictimSex = reorder(VictimSex,n)) %>%
  
  ggplot(aes(x = VictimSex,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = VictimSex, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Sex', y = 'Count of Incidents', 
       title = 'Count of Incidents and Sex') +
  coord_flip() + 
  theme_bw()

## Age and sex of victims

breaks = seq(0,100,5)

ListSex = c("M","F")

LACrime %>%
  filter( VictimSex %in% ListSex ) %>%
  ggplot(aes(VictimAge)) +
  geom_histogram(binwidth = 5,fill = c("red")) +
  facet_wrap(~ VictimSex) +
  scale_x_continuous(limits = c(0, 100),breaks=breaks ) +
  labs(x = 'Victim Age', y = 'Count of Crimes', 
       title = 'Age , Sex and Crimes') +
  theme_bw()

# sex and type of crime

LACrime %>%
  filter(!is.na(VictimSex)) %>%
  group_by(VictimSex,CrimeCodeDescription) %>%
  tally() %>%
  ungroup() %>%
  mutate(VictimSex = reorder(VictimSex,n)) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  
  ggplot(aes(x = VictimSex,y = n, fill =CrimeCodeDescription)) +
  geom_bar(stat='identity') +
  labs(x = 'Sex and Crime Description', y = 'Count of Incidents', 
       title = 'Count of Incidents and Sex and Crime Description') +
  coord_flip() + 
  theme_bw() + theme(legend.position="top")

# crime type for persons aged 70 and above

LACrime %>%
  filter(!is.na(VictimSex)) %>%
  filter(VictimAge >= 70) %>%
  group_by(VictimSex,CrimeCodeDescription) %>%
  tally() %>%
  ungroup() %>%
  mutate(VictimSex = reorder(VictimSex,n)) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  
  ggplot(aes(x = VictimSex,y = n,fill = CrimeCodeDescription)) +
  geom_bar(stat='identity') +
  labs(x = 'Sex and Crime Description', y = 'Count of Incidents', 
       title = 'Count of Incidents and Sex and Crime Description for Persons 70 and above') +
  coord_flip() + 
  theme_bw() + theme(legend.position="top")



# crime at different premises
LACrime %>%
  filter(!is.na(PremiseDescription)) %>%
  group_by(PremiseDescription) %>%
  tally() %>%
  ungroup() %>%
  mutate(PremiseDescription = reorder(PremiseDescription,n)) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  
  ggplot(aes(x = PremiseDescription,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = PremiseDescription, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'PremiseDescription', y = 'Count of Incidents', 
       title = 'Count of Incidents and PremiseDescription') +
  coord_flip() + 
  theme_bw()

# Types of crime at different premises

LACrime %>%
  filter(!is.na(PremiseDescription)) %>%
  group_by(PremiseDescription,CrimeCodeDescription) %>%
  tally() %>%
  ungroup() %>%
  mutate(PremiseDescription = reorder(PremiseDescription,n)) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  
  ggplot(aes(x = PremiseDescription,y = n, fill =CrimeCodeDescription)) +
  geom_bar(stat='identity') +
  labs(x = 'Premise Description and Crime Description', y = 'Count of Incidents', 
       title = 'Count of Incidents and Premise Description and Crime Description') +
  coord_flip() + 
  theme_bw()

# Victim Descent analysis

VictimDescentAbbr = c("A","B","C","D","F","G","H","I","J","K","L","O","P","S","U","V","W","X","Z")
VictimDescentDescription = c("Other Asian","Black",
                             "Chinese","Cambodian","Filipino",
                             "Guamanian","Hispanic/Latin/Mexican",
                             "American Indian/Alaskan Native",
                             "Japanese","Korean","Laotian ",
                             "Other","Pacific Islander",
                             "Samoan","Hawaiian","Vietnamese",
                             "White","Unknown","AsianIndian")

VictimDescentFull = data.frame(VictimDescent = as.character(VictimDescentAbbr),
                               VictimDescentDescription = as.character(VictimDescentDescription))

LACrime$VictimDescent = as.character(LACrime$VictimDescent)

LACrime %>%
  filter(!is.na(VictimDescent)) %>%
  group_by(VictimDescent) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  inner_join(VictimDescentFull) %>%
  head(10) %>%
  mutate(VictimDescentDescription = reorder(VictimDescentDescription,n)) %>%
  
  ggplot(aes(x = VictimDescentDescription,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = VictimDescentDescription, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'VictimDescent', y = 'Count of Incidents', 
       title = 'Count of Incidents and VictimDescent') +
  coord_flip() + 
  theme_bw()

# analysis of weapons used in crime

LACrime %>%
  filter(!is.na(WeaponDescription)) %>%
  group_by(WeaponDescription) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(10) %>%
  mutate(WeaponDescription = reorder(WeaponDescription,n)) %>%
  
  ggplot(aes(x = WeaponDescription,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = WeaponDescription, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Weapon Description', y = 'Count of Incidents', 
       title = 'Count of Incidents and Weapon Description') +
  coord_flip() + 
  theme_bw()

# Map the crime cities

center_lon = median(LACrime$longitude,na.rm = TRUE)
center_lat = median(LACrime$latitude,na.rm = TRUE)

LACrimeSample = LACrime %>% sample_n(50e3)

leaflet(LACrimeSample) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude, 
             color = c("red"))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat, zoom=10)

# Report District incidents

RDGeoLocation = LACrime %>% 
  group_by(ReportingDistrict) %>%
  summarise(RDlat = median(latitude,na.rm = TRUE)
            ,RDlon = median(longitude,na.rm = TRUE)
            ,CountIncidents = n()) %>%
  arrange(desc(CountIncidents)) %>%
  head(50)

head(RDGeoLocation,20) %>%
  mutate(ReportingDistrict = reorder(ReportingDistrict,CountIncidents)) %>%
  ggplot(aes(x = ReportingDistrict,y = CountIncidents)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = ReportingDistrict, y = 1, label = paste0("(",CountIncidents,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'ReportingDistrict', y = 'Count of Incidents', 
       title = 'Count of Incidents in Reporting District') +
  coord_flip() + 
  theme_bw()

# Map the Reporting district incidents

leaflet(RDGeoLocation) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~RDlon, lat = ~RDlat, radius = ~sqrt(CountIncidents)*30,
             color = c("red"))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat, zoom=10)


# Map of the Reporting Districts with their Crimes

RDGeoLocationFull = LACrime %>% 
  group_by(ReportingDistrict) %>%
  summarise(RDlat = median(latitude,na.rm = TRUE)
            ,RDlon = median(longitude,na.rm = TRUE)
            ,CountIncidents = n())

RD = readOGR(dsn = "D:/VIT/VIT2020SEM2/SET-2/Done/LAPD_Reporting_Districts.shp")

####

RD@data$REPDIST = as.numeric(RD@data$REPDIST )
RDGeoLocationFull$ReportingDistrict = as.numeric(RDGeoLocationFull$ReportingDistrict)

RD@data = left_join(RD@data, RDGeoLocationFull, by = c("REPDIST" = "ReportingDistrict"))

bins = c(0,1000,2000, 4000, 5000, 6000, 7000,8000, 9000)

pal = colorBin("YlOrRd", domain = RD@data$CountIncidents, bins = bins)

labels = sprintf(
  "<strong>%s</strong><br/>%g",
  RD@data$REPDIST, RD@data$CountIncidents
) %>% lapply(htmltools::HTML)

center_lon = median(RD@data$RDlon,na.rm = TRUE)
center_lat = median(RD@data$RDlat,na.rm = TRUE)

leaflet(data = RD) %>%  setView(lng=center_lon, lat=center_lat, zoom=12) %>%
  addPolygons(
    fillColor = ~pal(CountIncidents),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~CountIncidents, opacity = 0.7, title = "Reporting Districts and Crimes",
            position = "bottomleft")

# Type of crime analysis.

LACrime %>%
  group_by(CrimeCodeDescription) %>%
  tally() %>%
  arrange(desc(n)) %>%
  head(20) %>%
  mutate(CrimeCodeDescription = reorder(CrimeCodeDescription,n)) %>%
  
  ggplot(aes(x = CrimeCodeDescription,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = CrimeCodeDescription, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'CrimeCodeDescription', y = 'Count of Incidents', 
       title = 'Count of Incidents and CrimeCodeDescription') +
  coord_flip() + 
  theme_bw()


# Type of Crime and Reporting District

LACrime %>%
  group_by(CrimeCodeDescription,ReportingDistrict) %>%
  tally() %>%
  arrange(desc(n)) %>%
  head(20) %>%
  mutate(ReportingDistrict = reorder(ReportingDistrict,n))  %>%
  
  ggplot(aes(x = ReportingDistrict,y = n,fill = CrimeCodeDescription)) +
  geom_bar(stat='identity',colour="white") +
  labs(x = 'ReportingDistrict', y = 'Count of Incidents', 
       title = 'ReportingDistrict and CrimeCodeDescription') +
  coord_flip() + 
  theme_bw() +  theme(legend.position="top")

# Trend of Crimes

LACrimeGroup = LACrime %>%
  mutate(year_month = make_date(year=year(mdy(DateOccurred)),month=month(mdy(DateOccurred))) ) %>% 
  filter(!is.na(year_month)) %>%
  group_by(year_month) %>% summarize(CountCrimes = n()) 

LACrimeGroup %>% filter(year_month != "2017-09-01") %>%
  ggplot(aes(x=year_month,y=CountCrimes)) + 
  geom_line(size=.5, color="red")+geom_point(size=2, color="red")+theme_bw()

# Trend of Battery - simple Assault.

LACrimeGroupBatterySA = LACrime %>% 
  filter(CrimeCode == 624) %>%
  mutate(year_month = make_date(year=year(mdy(DateOccurred)),month=month(mdy(DateOccurred))) ) %>% 
  filter(!is.na(year_month)) %>%
  filter(year_month != "2017-09-01") %>%
  group_by(year_month) %>% 
  summarize(CountCrimes = n())  

LACrimeGroupBatterySA %>% filter(year_month != "2017-09-01") %>%
  ggplot(aes(x=year_month,y=CountCrimes)) + 
  geom_line(size=.5, color="red")+geom_point(size=2, color="red")+theme_bw()

# Predictions using ARIMA for Battery - Simple Assault

CountOfCrimes = ts(LACrimeGroupBatterySA$CountCrimes)

fit = auto.arima(CountOfCrimes,stepwise=FALSE, approximation=FALSE)

predictions = fit %>% forecast(h=4)

predictions %>% autoplot(include=80) +theme_bw()


# Predictions using ETS for Battery - Simple Assault

CountOfCrimes = ts(LACrimeGroupBatterySA$CountCrimes)

fit = ets(CountOfCrimes)

predictions = fit %>% forecast(h=4)

predictions %>% autoplot(include=80) +theme_bw()

#  Predictions using Prophet for Battery - Simple Assault

colnames(LACrimeGroupBatterySA) = c("ds","y")

m <- prophet(LACrimeGroupBatterySA,changepoint.prior.scale = 0.1)

future <- make_future_dataframe(m, periods = 4,freq = "month")

forecast <- predict(m, future)

predictionsValues = tail(forecast$yhat,4)

dataSetCrimes = data.frame(Month = as.character(), CrimeCount = as.numeric())

dataSetMonths = data.frame(c("September 2017",
                             "October 2017",
                             "November 2017",
                             "December 2017"))

dataSetCrimes = cbind(dataSetMonths,round(predictionsValues))

colnames(dataSetCrimes) = c("Months","Predictions")

datatable(dataSetCrimes, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

# Modeling...

