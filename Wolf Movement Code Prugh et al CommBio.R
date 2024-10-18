#Code to analyze wolf movement data from Denali National Park and create Fig.6
#Accompanies Prugh et al. Landscape heterogeneity buffers the impact of extreme winter weather on wildlife
#Last updated 17 Oct 2024

#load packages
library(ggplot2)
library(plyr)
library(tidyverse)

# Establish storm dates
Storm_start <- as.Date("2021-12-19")
Storm_end <- as.Date("2021-12-31")
RoS_Date<-as.Date("2021-12-27")

##Metadata for input data file fields
  #StepID = Identifier for step between 2 locations
  #DOY = day of year
  #Time1 = date/time of first location in the step
  #Time2 = date/time of second location in the step
  #Hours = number of hours between first and second location
  #StepLength = Euclidean distance between the locations (meters)
  #SL_Hour = Movement rate in meters per hour (StepLength/Hours)
  #Pack = name of the wolf pack

##Read in data file and create a Date column
WolfDat<-read.csv("DenaliWolfMovement.csv")%>%
  mutate(Time2 = as.POSIXct(Time2, "%d/%m/%Y %H:%M", tz = "UTC"))
WolfDat$Date2<-date(WolfDat$Time2)
 
##Create columns for Days since storm and Storm phase (before, during, after storm)
WolfDat$DaySinceStorm<-as.numeric(difftime(as.POSIXct(WolfDat$Time2), as.POSIXct(Storm_start),
                                           units = "days"))
WolfDat$Phase<-ifelse(as.numeric(as.POSIXct(WolfDat$Time2)>as.POSIXct(Storm_start) 
                                 & as.POSIXct(WolfDat$Time2)<as.POSIXct(Storm_end)),
                                "During","After")
WolfDat$Phase<-ifelse(WolfDat$DaySinceStorm < 0, "Before",WolfDat$Phase)
                    
##Calculate average movement rate (meters per hour, or mph) per pack per day
mean_step <- WolfDat %>% group_by(Pack, Phase, Date2) %>% 
  summarise(mean_mph = mean(SL_Hour))

##Summary table of movement rates by storm phase
MeanPhase<-ddply(.data=mean_step,c("Phase"), summarise,
                 N=length(mean_mph), 
                 mean=mean(mean_mph),
                 sd=sd(mean_mph), 
                 se=sd/sqrt(N),
                 lcl=mean-1.96*se,
                 ucl=mean+1.96*se)
MeanPhase

##Figure 6
fig6 <- ggplot(mean_step, aes(x=Date2, y=mean_mph)) +
  geom_point(alpha = 0.6, show.legend = FALSE) +
  geom_smooth(method = "loess", formula = y ~ x, span = 0.2, se = T) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d", 
                  limits = c(as.Date("2021-12-12"), as.Date("2022-01-08")))+
  labs(x="Date", y="Wolf movement rate (m/hr)")+
  geom_vline(xintercept = as.Date("2021-12-19"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2021-12-31"), linetype = "dashed")+
  theme_classic() +
  theme(axis.text = element_text(size = 12))+
  theme(text = element_text(size=14))
fig6   





