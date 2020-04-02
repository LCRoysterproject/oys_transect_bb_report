#########################################
## Summary Stats of Transects############
##Jennifer Moore, Mel Moreno, Bill Pine##
##########################################

########################
## Data Organization ###
########################

########################
#Note this is the "old code" that has a lot
#of Bill's notes in it related to how the "collapses" work
#This does not have the adjustments in it for the -999 aka flooded segements,
#so that's why the counts are weird for some spots
########################


### Transect data analysis

library("dplyr")
library("ggplot2")
library("lubridate")
library("viridis")

#read in data
#setwd("~/GitHub/transect/data")
transect<- read.csv("data/transect_data_production.csv", header= T)

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

#only winter
transect2 <- transect2[transect2$Season != "Summer",]

#only locality = BT, LC, LT, NN
transect2 <- transect2[transect2$locality != "CK",]
transect2 <- transect2[transect2$locality != "CR",]
transect2 <- transect2[transect2$locality != "HB",]

#only years after 2017
transect2 <- transect2[transect2$year > 2017,]

#get rid of this date as this was an unusual transect on 1 day
transect2 <- transect2[transect2$date != "2018-01-30",]

#####
#for 2018/2019 there are 2 passes (pass 1, pass2)
#average values together for this
#####

dta0=aggregate(count_live~day+month+year+Season+treatment+start_time+end_time+
                 locality+site+bar+station+transect+tran_length,data = transect2, FUN = "mean")


#####
#aggregate counts data for each bar 
#####

#on a day at a bar, this is all the live oysters counted
#note some of the values are negative because of the -999 used for flooded segments
#that is not accounted for in this old code but is in the " data mgmt production" code

dta=aggregate(count_live~day+month+year+Season+treatment+locality+site+bar+
                station,data=dta0,sum)

#can use this to see if there are any zeros

#let's try and match Steve's totals by strata for each 
dtax=aggregate(count_live~day+month+year+Season+treatment+locality+site+bar+
                 station+transect,data=dta0,sum)

sort.dtax<-dtax[order(dtax$locality, dtax$site, dtax$bar, dtax$transect),]
write.table(sort.dtax,file="data_output/dtax.csv", sep = ",", quote = FALSE, 
            col.names = T,row.names=F)



st <- read.csv("strata_working.csv", header = T)
for(i in 1:nrow(dtax)){
  station <- as.character(dtax$station[i])
  print(station)
  strata <- as.character(st$strata[st$station == station])
  print(strata)
  ifelse(length(strata) == 0, dtax$strata[i] <- NA, dtax$strata[i] <- strata)
}

table(dtax$strata)

#yes it matches white board, yeah!

######################################

#aggregate data to count live per transect on each bar 
#dta1 has replicate (aka transect), so you will see the individual transects #on a bar
#this is the summary that field crews remember, how many transects that were #done

#note there is a problem with this in terms of the transects for LCO related to Q1 are
#numbersed 1-18.

dta1=aggregate(count_live~day+month+year+Season+treatment+locality+
                 site+transect,data=transect2,sum)

sort.dta1<-dta1[order(dta1$year, dta1$month, dta1$day),]
write.table(sort.dta1,file="data_output/dta1.csv", sep = ",", quote = FALSE, 
            col.names = T,row.names=F)




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

dta2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+
                 station+transect,data=transect2,max)

#so by including transect in dta2 it determines maximum length of each #transect when multiple transects done on a bar.

sort.dta2<-dta2[order(dta2$year, dta2$month, dta2$station),]
#just write this out so people can see it

write.table(sort.dta2,file="data_output/max_individ_trans_length.txt", sep = ",", quote = FALSE, 
            col.names = T,row.names=F)
#the above is a useful table because you can go to any one transect that has been done and see how
#long that transect was and



#really big decision, note that everything is being summed or counted by day, 
#month, year, season.  so if you do a station over two days this is summing
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
#write.table(sort.dta3,file="data_output/merged_data.txt", sep = ",", quote = FALSE, row.names = F)

#calculate density
dta3$area = dta3$tran_length*.154
dta3$density = dta3$count_live/dta3$area

#####################
## Power Analysis ###
#####################

#interested in only 2018/2019 data
new <- dta3[dta3$year > 2017,]

#combine with the strata data
st <- read.csv("strata_working.csv", header = T)
for(i in 1:nrow(new)){
  station <- as.character(new$station[i])
  print(station)
  strata <- as.character(st$strata[st$station == station])
  print(strata)
  ifelse(length(strata) == 0, new$strata[i] <- NA, new$strata[i] <- strata)
}

#simple table 
table(new$locality, new$site)

#Counting the observations per locality, year, month
loc_mnth_yr<-dta2 %>%
  group_by(locality,year,month) %>%
  summarise(total.count=n())
names(loc_mnth_yr) <- c("locality", "year", "month","number_transects")

sspwrfun <- function(cl, po, v, d)
{
  n = (cl+po)^2 * 2 * v / d^2
  return(n)
}
#cl = confidence level = 1.96 for 95% confidence
#po = power = 0.84 for 80%
#v = population variance
#d = absolute difference you want to detect between population means

cl = 1.96
po = 0.84

#strata N_NA and N_SM
#plot

nna <- subset(new, new$strata == "N_NA")
nsm <- subset(new, new$strata == "N_SM")
plot(nna$year, nna$density, xlim=c(2018,2019),ylim=c(0,max(nsm$density)), 
     col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", 
     main = "Oyster Densities @ Two Strata")
points(nsm$year, nsm$density, col = "red", pch = 8,cex=1.5)
legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("N_NA", "N_SM"))

#calculate # transects
nna <- subset(new, new$strata == "N_NA")
nsm <- subset(new, new$strata == "N_SM")
v1 <- var(nna$density) #10078.64 
v2 <- var(nsm$density) #20199.82
d1 = mean(nsm$density) - mean(nna$density) #129.2683

ss_nna_nsm <- sspwrfun(cl, po, v2, d1) #18.95438


#strata N_NA and N_LG


nna <- subset(new, new$strata == "N_NA")
nlg <- subset(new, new$strata == "N_LG")

plot(nna$year, nna$density, xlim=c(2018,2019),ylim=c(0,max(nlg$density)), 
     col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", 
     main = "Oyster Densities @ Two Strata")
points(nlg$year, nlg$density, col = "red", pch = 8,cex=1.5)
legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("N_NA", "N_LG"))

nna <- subset(new, new$strata == "N_NA")
nlg <- subset(new, new$strata == "N_LG")
v1 <- var(nna$density) #10078.64
v2 <- var(nlg$density) #1103.472
d1 = mean(nna$density) - mean(nlg$density) #141.6282

ss_nna_nlg <- sspwrfun(cl, po, v1, d1) #7.878592

#strata N_SM and N_LG

plot(nsm$year, nsm$density, xlim=c(2018,2019),ylim=c(0,max(nsm$density)), 
     col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", 
     main = "Oyster Densities @ Two Strata")
points(nlg$year, nlg$density, col = "red", pch = 8,cex=1.5)
legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("N_SM", "N_LG"))

v1 <- var(nsm$density) #20199.82
v2 <- var(nlg$density) #1103.472
d1 = mean(nsm$density) - mean(nlg$density) #270.8965

ss_nsm_nlg <- sspwrfun(cl, po, v1, d1) #4.316054

#strata Y_NA and N_NA
yna <- subset(new, new$strata == "Y_NA")

plot(yna$year, yna$density, xlim=c(2018,2019),ylim=c(0,max(nna$density)), 
     col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", 
     main = "Oyster Densities @ Two Strata")
points(nna$year, nna$density, col = "red", pch = 8,cex=1.5)
legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("Y_NA", "N_NA"))

v1 <- var(yna$density) #8609.34 this was the old value, now 37144.41 so # samples increased
v2 <- var(nna$density) #10078.64
d1 = mean(nna$density) - mean(yna$density) #112.2832

ss_yna_nna <- sspwrfun(cl, po, v1, d1) #10.70745 old, now 744


#strata Y_NA and N_SM

plot(yna$year, yna$density, xlim=c(2018,2019),ylim=c(0,max(nsm$density)), 
     col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", 
     main = "Oyster Densities @ Two Strata")
points(nsm$year, nsm$density, col = "red", pch = 8,cex=1.5)
legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("Y_NA", "N_SM"))

v1 <- var(yna$density) #8609.34
v2 <- var(nsm$density) #20199.82
d1 = mean(nsm$density) - mean(yna$density) #241.5515

ss_yna_nsm <- sspwrfun(cl, po, v1, d1) #2.313643 old, new 56.8



#strata Y_NA and N_LG

plot(yna$year, yna$density, xlim=c(2018,2019),ylim=c(0,max(nlg$density)), 
     col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", 
     main = "Oyster Densities @ Two Strata")
points(nlg$year, nlg$density, col = "red", pch = 8,cex=1.5)
legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("Y_NA", "N_SM"))

v1 <- var(yna$density) #8609.34
v2 <- var(nlg$density) #1103.472
d1 = mean(yna$density) - mean(nlg$density) #29.34494

ss_yna_nlg <- sspwrfun(cl, po, v1, d1) #156.7651 old, new 20.2


##need to start working here to add ysm to all comparisons
##but we need to figure out how to handle -999
##in the data file.

#strata Y_NA and Y_SM

ysm <- subset(new, new$strata == "Y_SM")


plot(yna$year, yna$density, xlim=c(2018,2019),ylim=c(0,max(ysm$density)), 
     col = "blue", pch = 2,cex=1.5, xlab = "Date", ylab = "Density", 
     main = "Oyster Densities @ Two Strata")
points(ysm$year, ysm$density, col = "red", pch = 8,cex=1.5)
legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("Y_NA", "Y_SM"))

v1 <- var(ysm$density) #
v2 <- var(yna$density) #
d1 = mean(yna$density) - mean(ysm$density) #

ss_yna_ysm <- sspwrfun(cl, po, v1, d1) #156.7651 old, new 20.2




##make a table
out.samples<-cbind(ss_nna_nsm,ss_nna_nlg,ss_nsm_nlg,ss_yna_nna,ss_yna_nsm,ss_yna_nlg,ss_yna_ysm)
#just write this out so people can see it
out.samples.round=lapply(out.samples[1,],round,0)
write.table(out.samples.round,file="data_output/out_samples.txt", sep = ",", quote = FALSE, 
            col.names = T,row.names=F)






########
######
#thinking about length of transects
######

#let's go back and take a look at lengths of all transects
#this is looking at max so it looks at the max length, and not the segments, so would use dta2

hist(dta2$tran_length)

ggplot(dta2, aes(tran_length))+
  labs(title = "Frequency histogram of transect length for individual transects")+
  geom_histogram(bins=20)+
  facet_wrap(~locality+year)+
  theme_bw()+
  ylab("Frequency") +
  xlab ("Length of individual transects")+
  ylim(c(0,20))

# #mean length of each collapsed transect in each strata
# aaa<-dtax %>%
#   group_by(station) %>%
#   summarise(mean_tran_length=mean(tran_length))


#now look at the collapsed lengths, which is the sum of lengths of the collapsed transects,

hist(new$tran_length)

ggplot(new, aes(tran_length))+
  labs(title = "Frequency histogram of transect length for individual transects")+
  geom_histogram(bins=20)+
  facet_wrap(~strata)+
  theme_bw()+
  ylab("Frequency") +
  xlab ("Length of individual transects")+
  ylim(c(0,5))

mean(new$tran_length)

#summing length of transect sampled by strata
z<-new %>%
  group_by(strata) %>%
  summarise(total_tran_length=sum(tran_length))
names(z) <- c("strata","total tran length sampled")
write.table(z,file="data_output/sum_strata.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)


#n collapsed transect sampled by strata
zz<-new %>%
  group_by(strata) %>%
  summarise(number_collapsed_trans=n())
write.table(zz,file="data_output/n_tran_strata.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)


#mean length of each collapsed transect in each strata
zzz<-new %>%
  group_by(strata) %>%
  summarise(mean_tran_length=round(mean(tran_length),2))
write.table(zzz,file="data_output/mean_tran_length.txt", sep = ",", quote = FALSE, col.names = T,row.names=F)

  
mean(new$tran_length)





