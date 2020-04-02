#########################################
## Summary Stats of Transects############
##Jennifer Moore, Mel Moreno, Bill Pine##
#########################################

########################
## Data Organization ###
########################


### Transect data analysis

library("dplyr")
library("ggplot2")
library("lubridate")
library("viridis")

#read in data
transect<- read.csv("data/transect_data.csv", header= T)

#create year, month, day columns
transect$date<-mdy(transect$date)
transect$year<- year(transect$date)
transect$month<-month(transect$date)
transect$day<-day(transect$date)

#create season column
transect$Season<-ifelse(transect$month ==1 | transect$month == 10 | transect$month == 11 | transect$month == 12, "Winter", "Summer")

#Not all of the observations have time, so time would not parse all of the values
transect$date<- ymd(with(transect, paste(year,month,day, sep="-")))

#remove rows with NA for count_live
transect2 <- transect[!is.na(transect$count_live),]

#remove rows with NA for tran_length
transect2 <-transect[!is.na(transect$tran_length),]


########################
## Summary Statistics ##
########################

#aggregate counts data for each bar 
#on a day at a bar, this is all the live oysters counted
dta=aggregate(count_live~day+month+year+Season+treatment+locality+site+bar+station,data=transect2,sum)
#any transects with all 0s - yes, 6 of 256! (2%)

#aggregate data to count live per transect on each bar 

#dta1 has replicate (aka transect), so you will see the individual transects #on a bar
#this is the summary that field crews remember, how many transects that were #done
dta1=aggregate(count_live~day+month+year+Season+treatment+locality+site+transect,data=transect2,sum)

#simple table of number of transects each year, month locality
table(dta1$month,dta1$year,dta1$locality) 

#this is summing the number of oysters at a bar. So if 3 transects done on 1 bar, 
#the number of oysters from those 3 transects is summed
#dta1.1 sums the individual transects on the bar
#dta1.1=aggregate(count_live~day+month+year+Season+treatment+locality+site,data=transect2,sum)

#aggregate data for transect length
#this is summing the length of transects at a bar. 
#So if 3 transects done on 1 bar, 
#the lengths of those 3 transects is summed

#note this is taking the max of the transect lengths, 
#but need to make sure this is doing 
#this on a per transect basis.  
#So if you have 3 replicate transects, tran 1 = 20 total, tran 2=
#18, tran 3= 22 these will eventually need to summed

# max has to be used for each transect because transect length 
#records the "segments" 2.5, 5, up to the max length
# so if you sum this then you end up with the wrong total length of transect 

dta2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+station+transect,data=transect2,max)

#so by including transect in dta2 it determines maximum length of each #transect when multiple transects done on a bar.

#really big decision, note that everything is being summed or counted by day, #month, year, season.  so if you do a station over two days this is summing
#it as what you did on day 1 and then what you did on day 2.  
#really should figure out what we are wanting to show,
#maybe this is just "winter year 1" and need to create dataframe with that #column

#now need to sum those max transects, this gives you the total length of all 
#oyster bar measured on a day when multiple transects were done on a bar
dta2.2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+station,data=dta2,sum)

#really big decision, note that everything is being summed or counted by day, #month, year, season.  so if you do a station over two days this is summing
#it as what you did on day 1 and then what you did on day 2.  
#really should figure out what we are wanting to show,
#maybe this is just "winter year 1" and need to create dataframe with that #column


#merge data frames
#so this merges the summed oyster counts by transect with the summed transect lengths  

#I've checked these by hand (January 2019) in terms of making sure the correct
#total transect length is calculated and it looks good

dta3=merge(dta,dta2.2,by=c("day","month","year","Season","treatment","locality","site","bar","station"))

sort.dta3<-dta3[order(dta3$year, dta3$month, dta3$station),]
#just write this out so people can see it
# write.table(sort.dta3,file="data/archive/merged_data.txt", sep = ",", quote = FALSE, row.names = F)



#calculate density
dta3$area = dta3$tran_length*.154
dta3$density = dta3$count_live/dta3$area

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
stat_overall = sumstats(dta3$density)
round_stat_overall=lapply(stat_overall,round,2)
#write summary stats
# write.table(round_stat_overall,file="data/archive/density_stat_overall.txt", sep = ",", quote = FALSE, row.names = F)

#below are different summarys by year etc

stat_year = list()
for(i in as.character(min(dta3$year):max(dta3$year)))
{
  if(length(dta3$density[dta3$year == i]) > 0)
    stat_year[[i]] = sumstats(dta3$density[dta3$year == as.numeric(i)])
}

stat_season = list()
for(i in c("Winter", "Summer")){
  stat_season[[i]] = sumstats(dta3$density[dta3$Season == i])
}

stat_location = list()
for(i in levels(dta3$locality)){
  stat_location[[i]] = sumstats(dta3$density[dta3$locality == i])
}

###################
## Summary Tables ##
###################


table(transect2$month,transect2$year,transect2$locality) 
table(dta3$month,dta3$year,dta3$locality)

#Counting the observations per locality, year, month
loc_mnth_yr<-dta2 %>%
  group_by(locality,year,month) %>%
  summarise(total.count=n())
names(loc_mnth_yr) <- c("locality", "year", "month","number_transects")
# write.table(loc_mnth_yr,file="data/archive/loc_mnth_yr.txt", sep = ",", quote = FALSE, row.names = F)

#after collapse of transects 
#(collapse = combining multiple transects on a reef into one by summing length and count)
loc_mnth_yr<-dta3 %>%
  group_by(locality,year,month) %>%
  summarise(total.count=n())
names(loc_mnth_yr) <- c("locality", "year", "month","number_transects")
# write.table(loc_mnth_yr,file="data/archive/loc_mnth_yr_collapsed.txt", sep = ",", quote = FALSE, row.names = F)


#Counting the observations of each station per year, 
#these are specific reefs
#note this is dta2 which is the transects before the multiple 
#transects on an individual
#reef are collapsed.  If you want the aggregate of all transect length and #counts, used dta3
station_yr<-dta2 %>%
  group_by(year,station) %>%
  summarise(total.count=n()) 
names(station_yr) <- c("year", "station","number_transects")
# write.table(station_yr,file="data/archive/station_yr.txt", sep = ",", quote = FALSE, row.names = F)

#Counting the observations of each station, these are specific reefs
station_n<-dta3 %>%
  group_by(station) %>%
  summarise(total.count=n()) 
names(station_n) <- c("station","number_transects")
# write.table(station_n,file="data/archive/station_n.txt", sep = ",", quote = FALSE, row.names = F)



###################
## Summary Plots ##
###################

#overall boxplot of density
boxplot(dta3$density, ylab = "Density", main = "Oyster Densities Overall")

#boxplot of density by locality
boxplot(dta3$density ~ dta3$locality, ylab = "Density", xlab = "Localities", main = "Oyster Densities by Localities")

#boxplot of density by season
boxplot(dta3$density ~ dta3$Season, ylab = "Density", xlab = "Season", main = "Oyster Densities by Season")

#boxplot of density by treatment
boxplot(dta3$density ~ dta3$treatment, ylab = "Density", xlab = "Treatment", main = "Oyster Densities by Treatment")

#boxplot of density by site
boxplot(dta3$density ~ dta3$site, ylab = "Density", xlab = "Site", main = "Oyster Densities by Site")
        
#boxplot of density by station
boxplot(dta3$density ~ dta3$station, ylab = "Density", xlab = "Station", main = "Oyster Densities by Station")

#boxplot of density by month
boxplot(dta3$density ~ dta3$month, ylab = "Density", xlab = "Month", main = "Oyster Densities by Month")

#boxplot of density by year
boxplot(dta3$density ~ dta3$year, ylab = "Density", xlab = "Year", main = "Oyster Densities by Year")

#histogram of density by locality
hist(dta3$density[dta3$locality == "CK"], xlab = "Density", main = "Oyster Density Histogram at Locality CK")
hist(dta3$density[dta3$locality == "CR"], xlab = "Density", main = "Oyster Density Histogram at Locality CR")
hist(dta3$density[dta3$locality == "HB"], xlab = "Density", main = "Oyster Density Histogram at Locality HB")
hist(dta3$density[dta3$locality == "LC"], xlab = "Density", main = "Oyster Density Histogram at Locality LC")
hist(dta3$density[dta3$locality == "LT"], xlab = "Density", main = "Oyster Density Histogram at Locality LT")
hist(dta3$density[dta3$locality == "NN"], xlab = "Density", main = "Oyster Density Histogram at Locality NN")



## winter 2018-2019 comparison between different sites

#e3 <- dta3[dta3$year >= "2018" & dta3$month >= "10",]
e3 <- dta3[dta3$year >= "2018",]


e3$date<- ymd(with(e3, paste(year,month,day, sep="-")))


plot(e3$date[e3$locality == "LC"], e3$density[e3$locality=="LC"], col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density m^2", main = "Oyster Densities")
points(e3$date[e3$locality=="NN"], e3$density[e3$locality=="NN"], col = "red", pch = 8,cex=1.5)
points(e3$date[e3$locality=="BT"], e3$density[e3$locality=="BT"], col = "green", pch = 6,cex=1.5)
points(e3$date[e3$locality=="LT"], e3$density[e3$locality=="LT"], col = "darkblue", pch = 1,cex=1.5)


legend("topleft",bty="n", pch = c(2,8,6,1), col = c("blue", "red","green","darkblue"), 
legend = c("LC", "NN","BT","LT"))


###########################
## Lone Cabbage Analysis ##
###########################

#Locality == LC == Lone Cabbage
LC <- dta3[dta3$locality == "LC",]

#101 rows of data -> 101 transects
#data from 2010 - 2017

#run summary stats
stat_lc=sumstats(LC$tran_length)
round_stat_lc=lapply(stat_lc,round,2)
#write summary stats
# write.table(round_stat_lc,file="data/archive/stat_lc.txt", sep = ",", quote = FALSE, row.names = F)

#LC rock density summary
lc_rocks<-LC[LC$treatment == "rocks",]
# write.table(lc_rocks,file="data/archive/lc_rocks.txt", sep = ",", quote = FALSE, row.names = F)

stat_lc_rocks<-sumstats(lc_rocks$density)
round_stat_lc_rocks=lapply(stat_lc_rocks,round,2)
# write.table(round_stat_lc_rocks,file="data/archive/stat_lc_rocks.txt", sep = ",", quote = FALSE, row.names = F)


#LC control density summary
lc_control<-LC[LC$treatment == "control",]
stat_lc_control<-sumstats(lc_control$density)
round_stat_lc_control=lapply(stat_lc_control,round,2)

# write.table(round_stat_lc_control,file="data/archive/stat_lc_control.txt", sep = ",", quote = FALSE, row.names = F)

#number of oysters
boxplot(LC$count_live, ylab = "Oyster Live Counts", main = "Oyster Live Counts Overall")
#range 0 - 2488

#density
boxplot(LC$density, ylab = "Density", main = "Oyster Densities Overall")
#range 0 - 737.71

#by season
boxplot(LC$density ~ LC$Season, ylab = "Density", xlab = "Season", main = "LC Oyster Densities by Season")
#slighty higher in winter
t.test(LC$Season == "Summer", LC$Season == "Winter")
#not significant

#by treatment
boxplot(LC$density ~ LC$treatment, ylab = "Density", xlab = "Treatment", main = "Oyster Densities by Treatment")
#higher for rocks than control
t.test(LC$treatment == "control", LC$treatment == "rocks")
#significant



#by date
LC$date<- ymd(with(LC, paste(year,month,day, sep="-")))

plot(LC$date[LC$treatment == "control"], LC$density[LC$treatment == "control"], col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", main = "Oyster Densities over Time")
points(LC$date[LC$treatment == "rocks"], LC$density[LC$treatment == "rocks"], col = "red", pch = 8,cex=1.5)
legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("Control", "Rocks"))

plot(LC$density[LC$treatment == "rocks"] ~ LC$date[LC$treatment == "rocks"], data=LC,
     col = "red", cex=1.5, pch = 8, xlab = "Date", ylab = "Density", main = "LC Treatment Oyster Densities over Time")
 m1 <- lm(LC$density[LC$treatment == "rocks"] ~ LC$date[LC$treatment == "rocks"], data=LC)
 abline(m1, col="red")

plot(LC$density[LC$treatment == "control"] ~ LC$date[LC$treatment == "control"], data=LC,
     col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", main = "LC Control Site Oyster Densities over Time")
 m2 <- lm(LC$density[LC$treatment == "control"] ~ LC$date[LC$treatment == "control"], data=LC)
 abline(m2, col="blue")

plot(LC$density ~ LC$date, data=LC,
     col = "green", pch = 19,cex=1.5, xlab = "Date", ylab = "Density", main = "LC Site Oyster Densities over Time")
# m3 <- lm(LC$density ~ LC$date, data=LC)
# abline(m3, col="blue")
summary(m3)

m4 <- glm(formula=LC$count_live[LC$treatment == "rocks"] ~ LC$date[LC$treatment == "rocks"], data=LC, family=poisson)
summary(m4)

exp(-0.0004254)


###########################################################

#Oct 4 2018, below needs to be redone to check vs. Peter's calculations

#OK now just checking to see how Peter did some of his calculations
#that were used in the Frederick et al. paper.
#Get the average count per 2.5 m segment as Peter does in his 
#spreadsheet by averaging across segments and replicates
#I don't think this is the way to go, I think need to sum across the
#segments for each transect and divide by length of transect for density
#this is something to think more about

# #Get mean counts per each bar sampled to check Peter's paper
# p_cnt = aggregate(Cnt_Live~ Month + Year + Station + Site + Locality,data=tran,FUN=mean,na.rm=T,na.action=na.pass)         
# p_cnt = merge(cnt,max_tran) 
# p_cnt = cnt %>% arrange(Year, Month, Station)
# p_cnt$Trip = paste0(cnt$Year, "-", cnt$Month)
# 
# #this is just checking to see if this is the same as Peter's spreadsheet
# PF_tab1<-filter(p_cnt, Month=="Apr" & Year=="2013")
# #Yes the mean counts are the same as spreadsheet and same as paper. Good.
#############################################################

