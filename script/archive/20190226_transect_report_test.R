#Feb 26 2019
#Bill Pine
#summary code for bi-weekly LCR oyster project report

#this is a first attempt at putting the code together in a single
#file that makes the summaries and graphs for the bi-weekly reports
#during field season winter 2018-2019

#could this be part of the code used in package development?


#this code reads in a data file that has been processed and posted to Git
#including calculating density and assigning the strata used in 2018-2019


#packages required (load plyr first)

library("plyr")
library("dplyr")
library("ggplot2")
library("lubridate")
library("viridis")


#data are on Git, but here calling from local Git
data1<- read.csv("data/LC_transect_data_2018_2019.csv", header= T)

#could write data file to table to paste into report
#write.table(data1,file="data_output/data1.txt", sep = ",", quote = FALSE, row.names = T)


#summary statistics function
sumstats = function(x){ 
  NoTran0=length(na.omit(x[x==0]))
  NoTranLive=length(na.omit(x[x>0]))
  NobsTotal=length(na.omit(x))
  y=na.omit(x[x>0])
  Mean=mean(y) 
  Median=median(y)
  Sd=sd(y) 
  Var=var(y)
  CV=sd(y)/mean(y)
  Se=sd(y)/sqrt(length(y))
  L95ci=mean(y)-1.96*(sd(y)/sqrt(length(y)))
  U95ci=mean(y)+1.96*(sd(y)/sqrt(length(y)))
  bstrap <- c()
  for (i in 1:1000){
    bstrap <- c(bstrap, mean(sample(y,(length(y)),replace=T), na.rm = T))}
  Bstrapmean = mean(bstrap)
  L95bstrap = quantile(bstrap,.025)
  U95bstrap = quantile(bstrap,.975)
  return(list(NobsTotal=NobsTotal, Mean = Mean, Median=Median, Sd = Sd, Var = Var, CV = CV, Se = Se, L95ci = L95ci, U95ci = U95ci, Bstrapmean = Bstrapmean, L95bstrap = L95bstrap, U95bstrap = U95bstrap))
}



##############################
## Summary Tables Stats##
##############################

#calculate summary stats for density overall
stat_overall = sumstats(data1$density)
round_stat_overall=lapply(stat_overall,round,2)
#write summary stats
write.table(round_stat_overall,file="data_output/stat_overall.txt", sep = ",", quote = FALSE, row.names = F)


#calculate summary stats for density by year, strata

##only interested in strata for now

# stat_year = list()
# for(i in as.character(min(data1$year):max(data1$year)))
# {
#   if(length(data1$density[data1$year == i]) > 0)
#     stat_year[[i]] = sumstats(data1$density[data1$year == as.numeric(i)])
# }
# 
# stat_season = list()
# for(i in c("Winter", "Summer")){
#   stat_season[[i]] = sumstats(data1$density[data1$Season == i])
# }
# 
# stat_location = list()
# for(i in levels(data1$locality)){
#   stat_location[[i]] = sumstats(data1$density[data1$locality == i])
# }
stat_strata = list()
for(i in c("N_LG", "N_NA","N_SM","Y_NA","Y_SM")){
  stat_strata[[i]] = sumstats(data1$density[data1$strata == i])
}

#ok now let's convert that list (stats_strata) to a dataframe
df.strata.stats<-ldply(stat_strata,data.frame)

#now round the summary stats
round_df.strata.stats=lapply(df.strata.stats[2:13],round,2)
strat.stats<-cbind(df.strata.stats[1],round_df.strata.stats[1:12])
#write summary stats
write.table(strat.stats,file="data_output/round_dfstrata_stats.txt", sep = ",", quote = FALSE, row.names = F)


##############################
## Summary Tables Transects ##
##############################

#table(data1$month,data1$year,data1$locality) 

#summing length of transect sampled by strata
tran_length<-data1 %>%
  group_by(strata) %>%
  summarise(total_tran_length=sum(tran_length))
names(tran_length) <- c("strata","total tran length sampled")
write.table(z,file="data_output/sum_strata.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)


#n collapsed transect sampled by strata
#this will throw an error because of dplyr/plyr conflicts. plyr
#needs to run first
tran_n<-data1 %>%
  group_by(strata) %>%
  dplyr::summarise(n_transects=n())
write.table(tran_n,file="data_output/n_tran_strata.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)

#mean length of each collapsed transect in each strata

tran_mean<-data1 %>%
  group_by(strata) %>%
  summarise(mean_tran_length=round(mean(tran_length),2))
write.table(tran_mean,file="data_output/mean_tran_length.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)

#variance in length of each collapsed transect in each strata
tran_var<-data1 %>%
  group_by(strata) %>%
  summarise(var_tran_length=round(var(tran_length),2))
write.table(tran_var,file="data_output/var_tran_length.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)



###################
## Summary Plots ##
###################

#overall boxplot of density
boxplot(data1$density, ylab = "Density", main = "Oyster Densities Overall")

#boxplot of density by locality
boxplot(data1$density ~ data1$locality, ylab = "Density", xlab = "Localities", main = "Oyster Densities by Localities")

#boxplot of density by season
boxplot(data1$density ~ data1$season, ylab = "Density", xlab = "Season", main = "Oyster Densities by Season")

#boxplot of density by treatment
boxplot(data1$density ~ data1$treatment, ylab = "Density", xlab = "Treatment", main = "Oyster Densities by Treatment")

#boxplot of density by site
boxplot(data1$density ~ data1$site, ylab = "Density", xlab = "Site", main = "Oyster Densities by Site")

#boxplot of density by station
boxplot(data1$density ~ data1$station, ylab = "Density", xlab = "Station", main = "Oyster Densities by Station")

#histogram of collapsed transect lengths

windows()

ggplot(data1, aes(tran_length))+
  labs(title = "Frequency histogram of transect length for collapsed transects")+
  geom_histogram(bins=20)+
  facet_wrap(~strata)+
  theme_bw()+
  ylab("Frequency") +
  xlab ("Length of collapsed transects")+
  ylim(c(0,10))

#histogram of oyster density by strata

ggplot(data1, aes(density))+
  labs(title = "Frequency histogram of oyster density")+
  geom_histogram(bins=20)+
  facet_wrap(~strata)+
  theme_bw()+
  ylab("Frequency") +
  xlab ("Oyster density per m^2")+
  ylim(c(0,5))

#box plot of density on its side

ggplot(data1, aes(strata, density))+
  geom_boxplot()+ coord_flip()+
  labs(title = "Oyster density by strata")+
  xlab("Strata") +
  ylab ("Oyster density per m^2")


#dot plot by strata and station

ggplot(data1, aes(density, station, shape=strata, colour=strata))+
  geom_point(size=3)+
  labs(title = "Oyster density by station and strata")+
  ylab("Station") +
  xlab ("Oyster density per m^2")


