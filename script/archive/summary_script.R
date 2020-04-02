### Transect data analysis

library("dplyr")
library("ggplot2")
library("lubridate")
library("viridis")

transect<- read.csv("data/transect_data.csv", header= T)

# Checking the columns for unique values
levels(as.factor(transect$locality))

levels(as.factor(transect$station))

#Converting date to `lubridate` for further plotting
#splitting out the year, month, and day that can be read as a date 

transect$date<-mdy(transect$date)
transect$year<- year(transect$date)
transect$month<-month(transect$date)
transect$day<-day(transect$date)

#Not all of the observations have time, so time would not parse all of the values
transect$date<- ymd(with(transect, paste(year,month,day, sep="-")))

# If there are extra localities, or localities with the wrong name, use the code to update the name 
# transect <- transect[which(transect$locality=="incorrect_locality"),]   

# Histograms of the data for live and dead counts per locality
ggplot(transect, aes(count_live))+
  geom_histogram(bins=30)+
  facet_wrap(~locality)

#Tables

#Counting the observations per locality for all time, every year
transect %>%
  group_by(locality) %>%
  summarise(total.count=n()) 

#Counting the observations of each station per date
transect %>%
  group_by(date,station) %>%
  summarise(total.count=n()) 

#counting by locality and transect length
transect %>%
  group_by(year,month,locality, tran_length) %>%
  summarise(total.count=n()) 

# Plots showing the live and dead of each locality with date as the x-axis

ggplot(data=transect, aes(x=date, y = count_live)) +
  geom_point() +
  geom_smooth(method= "lm") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~locality)

ggplot(data=transect, aes(x=date, y = count_dead)) +
  geom_point() +
  geom_smooth(method= "lm") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~locality)

# Plots showing the mean live counts per transect length

ggplot(data=transect, aes(x=tran_length, y = count_live)) +
  stat_summary(fun.y = "mean", geom="point") +
  xlab("Transect Length") +
  ylab ("Mean Live Oyster Counts") +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~locality + year)
  
# Adding the max length of each transect line 
#tran_max<-transect %>% 
  #group_by(date, station) %>%                 
  #mutate(max_length= max(tran_length))

# Calculating the area of each transect length *width
#tran_max$area<- (tran_max$max_length * .15)


# Calculating the densit for the live counts per station per year
transect %>% 
  filter(locality== "LC") %>%
  ggplot(aes(x = year, color=count_live)) +
  labs(x="Year", y= 'Frequency') +
  stat_density(aes(group = count_live), position="stack",geom="line", size= 1.5) +
  scale_color_viridis(option= "heat") +
  facet_wrap(~station)
