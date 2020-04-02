#Feb 6 2019
#this is reading in a data file that has been processed
#including calculating density and assigning the strata used in 2018-2019

#first attempt at updating summary code for only 2018-2019 samples

library("dplyr")
library("ggplot2")
library("lubridate")
library("viridis")

data1<- read.csv("data/LC_transect_data_2018_2019.csv", header= T)


#round_data_overall=lapply(data1&density,round,2)
write.table(data1,file="data_output/data1.txt", sep = ",", quote = FALSE, row.names = T)


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
  L95se=mean(y)-1.96*(sd(y)/sqrt(length(y)))
  U95se=mean(y)+1.96*(sd(y)/sqrt(length(y)))
  bstrap <- c()
  for (i in 1:1000){
    bstrap <- c(bstrap, mean(sample(y,(length(y)),replace=T), na.rm = T))}
  Bstrapmean = mean(bstrap)
  L95bstrap = quantile(bstrap,.025)
  U95bstrap = quantile(bstrap,.975)
  return(list(NobsTotal=NobsTotal, Mean = Mean, Median=Median, Sd = Sd, Var = Var, CV = CV, Se = Se, L95se = L95se, U95se = U95se, Bstrapmean = Bstrapmean, L95bstrap = L95bstrap, U95bstrap = U95bstrap))
}


#calculate summary stats for density
stat_overall = sumstats(data1$density)
round_stat_overall=lapply(stat_overall,round,2)
#write summary stats
write.table(round_stat_overall,file="data_output/stat_overall.txt", sep = ",", quote = FALSE, row.names = F)

#below are different summarys by year etc

stat_year = list()
for(i in as.character(min(data1$year):max(data1$year)))
{
  if(length(data1$density[data1$year == i]) > 0)
    stat_year[[i]] = sumstats(data1$density[data1$year == as.numeric(i)])
}

stat_season = list()
for(i in c("Winter", "Summer")){
  stat_season[[i]] = sumstats(data1$density[data1$Season == i])
}

stat_location = list()
for(i in levels(data1$locality)){
  stat_location[[i]] = sumstats(data1$density[data1$locality == i])
}
stat_strata = list()
for(i in c("N_LG", "N_NA","N_SM","Y_NA","Y_SM")){
  stat_strata[[i]] = sumstats(data1$density[data1$strata == i])
}

#ok now let's convert that list to a dataframe
library(plyr)
df.strata.stats<-ldply(stat_strata,data.frame)

round_df.strata.stats=lapply(df.strata.stats[2:13],round,2)
z<-cbind(df.strata.stats[1],round_df.strata.stats[1:12])
#write summary stats
write.table(z,file="data_output/round_dfstrata_stats.txt", sep = ",", quote = FALSE, row.names = F)



###################
## Summary Tables ##
###################


##here is a summary table to match the white board totals by strata
dtax=aggregate(count_live~day+month+year+season+treatment+locality+site+bar+station+transect,data=dta0,sum)
st <- read.csv("data/strata.csv", header = T)
for(i in 1:nrow(dtax)){
  station <- as.character(dtax$station[i])
  print(station)
  strata <- as.character(st$strata[st$station == station])
  print(strata)
  ifelse(length(strata) == 0, dtax$strata[i] <- NA, dtax$strata[i] <- strata)
}

whiteboard<-table(dtax$strata)
write.table(whiteboard,file="data_output/whiteboard.txt", sep = ",", quote = FALSE, row.names = F)


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





# #note this depends on whether or not you have year and month from the data compilation survey
# 
# 
# 
# table(data1$month,data1$year,data1$locality) 
# 
# #Counting the observations per locality, year, month
# loc_mnth_yr<-data1 %>%
#   group_by(locality,year,month) %>%
#   summarise(total.count=n())
# names(loc_mnth_yr) <- c("locality", "year", "month","number_transects")
# write.table(loc_mnth_yr,file="data_output/loc_mnth_yr.txt", sep = ",", quote = FALSE, row.names = F)
# 
# #after collapse of transects 
# #(collapse = combining multiple transects on a reef into one by summing length and count)
# loc_mnth_yr<-data1 %>%
#   group_by(locality,year,month) %>%
#   summarise(total.count=n())
# names(loc_mnth_yr) <- c("locality", "year", "month","number_transects")
# write.table(loc_mnth_yr,file="data_output/loc_mnth_yr_collapsed.txt", sep = ",", quote = FALSE, row.names = F)

###########
#below doesn't work if you are using the "data processing code" which already does the collapse
#and density calculations
#note this is a super critical summary however
###########


#Counting the observations of each station per year, 
#these are specific reefs
#note this is dta2 which is the transects before the multiple 
#transects on an individual
#reef are collapsed.  If you want the aggregate of all transect length and #counts, used data1
# station_yr<-dta2 %>%
#   group_by(year,station) %>%
#   summarise(total.count=n()) 
# names(station_yr) <- c("year", "station","number_transects")
# write.table(station_yr,file="data_output/station_yr.txt", sep = ",", quote = FALSE, row.names = F)
# 
# #Counting the observations of each station, these are specific reefs
# station_n<-data1 %>%
#   group_by(station) %>%
#   summarise(total.count=n()) 
# names(station_n) <- c("station","number_transects")
# write.table(station_n,file="data_output/station_n.txt", sep = ",", quote = FALSE, row.names = F)



###################
## Summary Plots ##
###################

#overall boxplot of density
boxplot(data1$density, ylab = "Density", main = "Oyster Densities Overall")

#boxplot of density by locality
boxplot(data1$density ~ data1$locality, ylab = "Density", xlab = "Localities", main = "Oyster Densities by Localities")

#boxplot of density by season
boxplot(data1$density ~ data1$Season, ylab = "Density", xlab = "Season", main = "Oyster Densities by Season")

#boxplot of density by treatment
boxplot(data1$density ~ data1$treatment, ylab = "Density", xlab = "Treatment", main = "Oyster Densities by Treatment")

#boxplot of density by site
boxplot(data1$density ~ data1$site, ylab = "Density", xlab = "Site", main = "Oyster Densities by Site")

#boxplot of density by station
boxplot(data1$density ~ data1$station, ylab = "Density", xlab = "Station", main = "Oyster Densities by Station")

#boxplot of density by month
boxplot(data1$density ~ data1$month, ylab = "Density", xlab = "Month", main = "Oyster Densities by Month")

#boxplot of density by year
boxplot(data1$density ~ data1$year, ylab = "Density", xlab = "Year", main = "Oyster Densities by Year")

#histogram of density by locality
hist(data1$density[data1$locality == "CK"], xlab = "Density", main = "Oyster Density Histogram at Locality CK")
hist(data1$density[data1$locality == "CR"], xlab = "Density", main = "Oyster Density Histogram at Locality CR")
hist(data1$density[data1$locality == "HB"], xlab = "Density", main = "Oyster Density Histogram at Locality HB")
hist(data1$density[data1$locality == "LC"], xlab = "Density", main = "Oyster Density Histogram at Locality LC")
hist(data1$density[data1$locality == "LT"], xlab = "Density", main = "Oyster Density Histogram at Locality LT")
hist(data1$density[data1$locality == "NN"], xlab = "Density", main = "Oyster Density Histogram at Locality NN")


# 
# ## winter 2018-2019 comparison between different sites
# 
# #e3 <- data1[data1$year >= "2018" & data1$month >= "10",]
# e3 <- data1[data1$year >= "2018",]
# 
# 
# e3$date<- ymd(with(e3, paste(year,month,day, sep="-")))
# 
# 
# plot(e3$date[e3$locality == "LC"], e3$density[e3$locality=="LC"], col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density m^2", main = "Oyster Densities")
# points(e3$date[e3$locality=="NN"], e3$density[e3$locality=="NN"], col = "red", pch = 8,cex=1.5)
# points(e3$date[e3$locality=="BT"], e3$density[e3$locality=="BT"], col = "green", pch = 6,cex=1.5)
# points(e3$date[e3$locality=="LT"], e3$density[e3$locality=="LT"], col = "darkblue", pch = 1,cex=1.5)
# 
# 
# legend("topleft",bty="n", pch = c(2,8,6,1), col = c("blue", "red","green","darkblue"), 
#        legend = c("LC", "NN","BT","LT"))
# 
# 
# ###########################
# ## Lone Cabbage Analysis ##
# ###########################
# 
# #Locality == LC == Lone Cabbage
# LC <- data1[data1$locality == "LC",]
# 
# #101 rows of data -> 101 transects
# #data from 2010 - 2017
# 
# #run summary stats
# stat_lc=sumstats(LC$tran_length)
# round_stat_lc=lapply(stat_lc,round,2)
# #write summary stats
# write.table(round_stat_lc,file="data_output/stat_lc.txt", sep = ",", quote = FALSE, row.names = F)
# 
# #LC rock density summary
# lc_rocks<-LC[LC$treatment == "rocks",]
# write.table(lc_rocks,file="data_output/lc_rocks.txt", sep = ",", quote = FALSE, row.names = F)
# 
# stat_lc_rocks<-sumstats(lc_rocks$density)
# round_stat_lc_rocks=lapply(stat_lc_rocks,round,2)
# write.table(round_stat_lc_rocks,file="data_output/stat_lc_rocks.txt", sep = ",", quote = FALSE, row.names = F)
# 
# 
# #LC control density summary
# lc_control<-LC[LC$treatment == "control",]
# stat_lc_control<-sumstats(lc_control$density)
# round_stat_lc_control=lapply(stat_lc_control,round,2)
# 
# write.table(round_stat_lc_control,file="data_output/stat_lc_control.txt", sep = ",", quote = FALSE, row.names = F)
# 
# #number of oysters
# boxplot(LC$count_live, ylab = "Oyster Live Counts", main = "Oyster Live Counts Overall")
# #range 0 - 2488
# 
# #density
# boxplot(LC$density, ylab = "Density", main = "Oyster Densities Overall")
# #range 0 - 737.71
# 
# #by season
# boxplot(LC$density ~ LC$Season, ylab = "Density", xlab = "Season", main = "LC Oyster Densities by Season")
# #slighty higher in winter
# t.test(LC$Season == "Summer", LC$Season == "Winter")
# #not significant
# 
# #by treatment
# boxplot(LC$density ~ LC$treatment, ylab = "Density", xlab = "Treatment", main = "Oyster Densities by Treatment")
# #higher for rocks than control
# t.test(LC$treatment == "control", LC$treatment == "rocks")
# #significant
# 
# 
# 
# #by date
# LC$date<- ymd(with(LC, paste(year,month,day, sep="-")))
# 
# plot(LC$date[LC$treatment == "control"], LC$density[LC$treatment == "control"], col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", main = "Oyster Densities over Time")
# points(LC$date[LC$treatment == "rocks"], LC$density[LC$treatment == "rocks"], col = "red", pch = 8,cex=1.5)
# legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("Control", "Rocks"))
# 
# plot(LC$density[LC$treatment == "rocks"] ~ LC$date[LC$treatment == "rocks"], data=LC,
#      col = "red", cex=1.5, pch = 8, xlab = "Date", ylab = "Density", main = "LC Treatment Oyster Densities over Time")
# m1 <- lm(LC$density[LC$treatment == "rocks"] ~ LC$date[LC$treatment == "rocks"], data=LC)
# abline(m1, col="red")
# 
# plot(LC$density[LC$treatment == "control"] ~ LC$date[LC$treatment == "control"], data=LC,
#      col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", main = "LC Control Site Oyster Densities over Time")
# m2 <- lm(LC$density[LC$treatment == "control"] ~ LC$date[LC$treatment == "control"], data=LC)
# abline(m2, col="blue")
# 
# plot(LC$density ~ LC$date, data=LC,
#      col = "green", pch = 19,cex=1.5, xlab = "Date", ylab = "Density", main = "LC Site Oyster Densities over Time")
# # m3 <- lm(LC$density ~ LC$date, data=LC)
# # abline(m3, col="blue")
# summary(m3)


