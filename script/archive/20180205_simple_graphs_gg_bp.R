#Feb 5 2019
#this is reading in a data file that has been processed
#including calculating density and assigning the strata used in 2018-2019
#will need to go back and create strata for all the old sites too (non 2018-2019)

library("dplyr")
library("ggplot2")
library("lubridate")
library("viridis")

data1<- read.csv("data/archive/2018_2019_winter_only_transect_data.csv", header= T)


########
#simple density things

#mean density by strata
mean_dens<-data1 %>%
  group_by(strata) %>%
  summarise(mean_density=round(mean(density),2))

#variance density by strata
variance_dens<-data1 %>%
  group_by(strata) %>%
  summarise(variance_density=round(var(density),2))



ggplot(data1, aes(density))+
  labs(title = "Frequency histogram of oyster density")+
  geom_histogram(bins=20)+
  facet_wrap(~strata)+
  theme_bw()+
  ylab("Frequency") +
  xlab ("Oyster density per m^2")+
  ylim(c(0,5))


ggplot(data1, aes(density, station, shape=strata, colour=strata))+
  geom_point(size=3)+
  labs(title = "Oyster density by station and strata")+
  ylab("Station") +
  xlab ("Oyster density per m^2")

ggplot(data1, aes(strata, density))+
  geom_boxplot()+ coord_flip()+
  labs(title = "Oyster density by strata")+
  xlab("Strata") +
  ylab ("Oyster density per m^2")

p


######
#thinking about length of transects
######

#let's go back and take a look at lengths of all transects
#this is looking at max so it looks at the max length, and not the segments, s

hist(data1$tran_length)

ggplot(data1, aes(tran_length))+
  labs(title = "Frequency histogram of transect length for collapsed transects")+
  geom_histogram(bins=20)+
  facet_wrap(~locality)+
  theme_bw()+
  ylab("Frequency") +
  xlab ("Length of collapsed transects")+
  ylim(c(0,15))

# #mean length of each collapsed transect in each strata
# aaa<-dtax %>%
#   group_by(station) %>%
#   summarise(mean_tran_length=mean(tran_length))


#now look at the collapsed lengths, which is the sum of lengths of the collapsed transects,

hist(data1$tran_length)

ggplot(data1, aes(tran_length))+
  labs(title = "Frequency histogram of transect length for collapsed transects")+
  geom_histogram(bins=20)+
  facet_wrap(~strata)+
  theme_bw()+
  ylab("Frequency") +
  xlab ("Length of collapsed transects")+
  ylim(c(0,5))

mean(data1$tran_length)

#summing length of transect sampled by strata
z<-data1 %>%
  group_by(strata) %>%
  summarise(total_tran_length=sum(tran_length))
names(z) <- c("strata","total tran length sampled")
write.table(z,file="data_output/sum_strata.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)


#n collapsed transect sampled by strata
zz<-data1 %>%
  group_by(strata) %>%
  summarise(number_collapsed_trans=n())
write.table(zz,file="data_output/n_tran_strata.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)


#mean length of each collapsed transect in each strata
zzz<-data1 %>%
  group_by(strata) %>%
  summarise(mean_tran_length=round(mean(tran_length),2))
write.table(zzz,file="data_output/mean_tran_length.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)


#mean length of each collapsed transect in each strata
zzzz<-data1 %>%
  group_by(strata) %>%
  summarise(var_tran_length=round(var(tran_length),2))
write.table(zzzz,file="data_output/var_tran_length.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)



mean(data1$tran_length)

