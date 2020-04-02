#Feb 5 2019
#this is reading in a data file that has been processed
#including calculating density and assigning the strata used in 2018-2019
#will need to go back and create strata for all the old sites too (non 2018-2019)

library("dplyr")
library("ggplot2")
library("lubridate")
library("viridis")

data1<- read.csv("LC_transect_data_2010_2019.csv", header= T)


########
#simple density things

#mean density by strata
mean_dens<-data1 %>%
  group_by(locality) %>%
  summarise(mean_density=round(mean(density),2))

#variance density by strata
variance_dens<-data1 %>%
  group_by(locality) %>%
  summarise(variance_density=round(var(density),2))



ggplot(data1, aes(density))+
  labs(title = "Frequency histogram of oyster density")+
  geom_histogram(bins=20)+
  facet_wrap(~locality)+
  theme_bw()+
  ylab("Frequency") +
  xlab ("Oyster density per m^2")+
  ylim(c(0,15))

#let's just look at LC for example

datalc<- data1[data1$locality=="LC",]

ggplot(datalc, aes(period, count_live, colour=site))+
  geom_point(size=3)+
  labs(title = "Lone Cabbage oyster count by site and period")+
  ylab("Live oyster count") +
  xlab ("Period")+
  scale_colour_manual(values=c("#0072B2", "#D55E00", "#009E73"))+
  scale_x_continuous(breaks=seq(1,17,1))

ggplot(datalc, aes(period, tran_length, colour=site))+
  geom_point(size=3)+
  labs(title = "Lone Cabbage transect length (m) by site and period")+
  ylab("Transect length (m)") +
  xlab ("Year")+
  scale_colour_manual(values=c("#0072B2", "#D55E00", "#009E73"))+
  scale_x_continuous(breaks=seq(1,17,1))



ggplot(data1, aes(count_live, period))+
  geom_boxplot()+ coord_flip()+
  labs(title = "Oyster density by period")+
  xlab("Period") +
  ylab ("Oyster count")

ggplot(d2, aes(x=period, y=count_live, group=period)) + 
  geom_boxplot()+
  labs(title = "Live oyster count by period") +
  xlab ("Period") +
  ylab ("Live oyster count") +
  theme(text = element_text(size=15))


p


#histogram of oyster counts by density
h <- ggplot(d2, aes(count_live))+
  geom_histogram(aes(x = count_live, y = ..density..),bins=55, fill = 'grey', col = 'black')+
  theme_bw()+
  ylab("Density") +
  xlab ("Oyster Counts")


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
  group_by(period) %>%
  summarise(total_tran_length=sum(tran_length))
names(z) <- c("period","total_tran_length")

ggplot(z, aes(period, total_tran_length))+
  labs(title = "Frequency histogram of transect length for collapsed transects")+
  geom_bar(size=3)+
  theme_bw()+
  ylab("Frequency") +
  xlab ("Length of collapsed transects")+
  ylim(c(0,5))




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

