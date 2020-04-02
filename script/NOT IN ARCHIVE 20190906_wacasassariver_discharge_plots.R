################################################################################
#
# Waccasassa River NR Gulf Hammock, Florida  gauge 02313700
# Data downloaded from https://waterdata.usgs.gov/fl/nwis/uv?site_no=02313700
# 72137 Discharge, tide filtered 
# available from 2009-10-02 to 2019-09-04
# data from 2018-10-03 to 2019-09-04 currently provisional
# data saved in file WaccasassaRiverDischarge.csv in data folder
################################################################################

#SETUP
rm(list=ls());gc();

library("zoo")

dis <- read.csv("data/waccasassa_river_discharge.csv", header = T)

#get some date components
dis$Date <- as.Date(dis$Date, "%m/%d/%Y")
dis$year    = as.numeric(strftime(dis$Date,format="%Y"))
dis$month   = as.numeric(strftime(dis$Date,format="%m")) 

#make dataset from epochs, 
disE  = dis[dis$Date>='2009-10-01' & dis$Date<='2019-07-31',]  


#get monthly sum, mean, sd, and var
#discharge
disE.mo  = aggregate(Value~month+year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE.mo  = do.call('data.frame',disE.mo)
names(disE.mo)[3:6] = c('avg','sd','var','sumflow') 
disE.mo$yrmo = disE.mo$year+(disE.mo$month-0.5)/12       


#get yearly mean, sd, and var and annual sum of discharge
#discharge
disE.yr  = aggregate(Value~year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x,na.rm=T)))
disE.yr  = do.call('data.frame',disE.yr)
names(disE.yr)[2:5] = c('avg','sd','var','sum')                      

#overall mean, sd, var
disE.all  = aggregate(Value~Station.ID,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T)))
disE.all  = do.call('data.frame',disE.all)
names(disE.all)[2:4] = c('avg','sd','var')  


#make some time series objects
disE.zoo    = zoo(disE$Value,disE$Date)  

disE.mo.ts  = ts(disE.mo$avg,start=c(2009,10),end=c(2019,7),frequency=12)
disE.mo.sum.ts  = ts(disE.mo$sumflow,start=c(2009,10),end=c(2019,7),frequency=12)
disE.yr.ts  = ts(disE.yr$avg,start=2009,end=2019,frequency=1)



################################################################################ 
#
#                     PLOT DATA
#
################################################################################ 

###########
#now work with water epoch 2
#get monthly sum, mean, sd, and var
#discharge


disE2 = dis[dis$Date>='2010-01-01' & dis$Date<='2019-07-31',]

disE2.mo  = aggregate(Value~month+year,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE2.mo  = do.call('data.frame',disE2.mo)
names(disE2.mo)[3:6] = c('avg','sd','var','sumflow') 
disE2.mo$yrmo = disE2.mo$year+(disE2.mo$month-0.5)/12       


#get yearly mean, sd, and var and annual sum of discharge
#discharge
disE2.yr  = aggregate(Value~year,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x,na.rm=T)))
disE2.yr  = do.call('data.frame',disE2.yr)
names(disE2.yr)[2:5] = c('avg','sd','var','sum')                      

#overall mean, sd, var
disE2.all  = aggregate(Value~Station.ID,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T)))
disE2.all  = do.call('data.frame',disE2.all)
names(disE2.all)[2:4] = c('avg','sd','var')  

disE2.yr$CV  = (disE2.yr$sd/disE2.yr$avg)*100


#make some time series objects
disE2.zoo    = zoo(disE2$Value,disE2$Date)  

disE2.mo.ts  = ts(disE2.mo$avg,start=c(2010,1),end=c(2019,5),frequency=12)
disE2.mo.sum.ts  = ts(disE2.mo$sumflow,start=c(2010,1),end=c(2019,5),frequency=12)
disE2.yr.ts  = ts(disE2.yr$avg,start=2010,end=2019,frequency=1)


par(mfrow=c(2,2))




#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

plot(disE2.yr$year,disE2.yr$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Mean daily discharge by year', ylim=c(0,600), xlim=c(2010,2019))
lines(lowess(disE2.yr$year,disE2.yr$avg),col='red', lwd=4)
abline(h=disE.all[2], col="blue", lwd=4, lty=2) 
#this calls disE because you want the average from full time series

#abline(lm(disE2.yr$avg~seq(1950,2019,1)))
title(stinfo[1,2],outer=T, line = -1.0)

plot(disE2.yr$year,disE2.yr$var,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Daily discharge variance by year', ylim=c(0,200000), xlim=c(2010,2019))
lines(lowess(disE2.yr$year,disE2.yr$var),col='red', lwd=4)
abline(h=disE.all[4], col="blue", lwd=4, lty=2)
#abline(lm(disE2.yr$avg~seq(1950,2019,1)))
title(outer=T)

plot(disE2.yr$year,disE2.yr$CV,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Daily discharge CV by year', ylim=c(0,300), xlim=c(2010,2019))
lines(lowess(disE2.yr$year,disE2.yr$CV),col='red', lwd=4)
abline(h=((disE.all[3]/disE.all[2])*100), col="blue", lwd=4, lty=2)
title(outer=T)

plot(disE2.yr$year,disE2.yr$sum,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Total annual discharge by year', ylim=c(0,5000000), xlim=c(2010,2019))
lines(lowess(disE2.yr$year,disE2.yr$sum),col='red', lwd=4)
abline(h=mean(disE.yr$sum), col="blue", lwd=4, lty=2)

title(outer=T)




###########
# REMAKE PLOTS FOR JUST 2010 - 2018
# take out titles add in letter labels for each panel
#now work with water epoch 2
#get monthly sum, mean, sd, and var
#discharge

disE2 = dis[dis$dates>='2010-01-01' & dis$dates<='2018-12-31',]

disE2.mo  = aggregate(val~month+year,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE2.mo  = do.call('data.frame',disE2.mo)
names(disE2.mo)[3:6] = c('avg','sd','var','sumflow') 
disE2.mo$yrmo = disE2.mo$year+(disE2.mo$month-0.5)/12       

#get yearly mean, sd, and var and annual sum of discharge
#discharge
disE2.yr  = aggregate(val~year,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x,na.rm=T)))
disE2.yr  = do.call('data.frame',disE2.yr)
names(disE2.yr)[2:5] = c('avg','sd','var','sum')                      

#overall mean, sd, var
disE2.all  = aggregate(val~staid,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T)))
disE2.all  = do.call('data.frame',disE2.all)
names(disE2.all)[2:4] = c('avg','sd','var')  

disE2.yr$CV  = (disE2.yr$sd/disE2.yr$avg)*100

#make some time series objects
disE2.zoo    = zoo(disE2$val,disE2$dates)  

disE2.mo.ts  = ts(disE2.mo$avg,start=c(2010,1),end=c(2019,5),frequency=12)
disE2.mo.sum.ts  = ts(disE2.mo$sumflow,start=c(2010,1),end=c(2019,5),frequency=12)
disE2.yr.ts  = ts(disE2.yr$avg,start=2010,end=2019,frequency=1)


par(mfrow=c(2,2))

plot(disE2.yr$year,disE2.yr$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,600), xlim=c(2010,2018))
lines(lowess(disE2.yr$year,disE2.yr$avg),col='red', lwd=4)
abline(h=disE.all[2], col="blue", lwd=4, lty=2) 
#this calls disE because you want the average from full time series
mtext("A", side = 3, line = 1, adj = -0.05, font = 2)

plot(disE2.yr$year,disE2.yr$var,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,200000), xlim=c(2010,2018))
lines(lowess(disE2.yr$year,disE2.yr$var),col='red', lwd=4)
abline(h=disE.all[4], col="blue", lwd=4, lty=2)
mtext("B", side = 3, line = 1, adj = -0.05, font = 2)


plot(disE2.yr$year,disE2.yr$CV,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,300), xlim=c(2010,2018))
lines(lowess(disE2.yr$year,disE2.yr$CV),col='red', lwd=4)
abline(h=((disE.all[3]/disE.all[2])*100), col="blue", lwd=4, lty=2)
mtext("C", side = 3, line = 1, adj = -0.05, font = 2)

plot(disE2.yr$year,disE2.yr$sum,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,5000000), xlim=c(2010,2018))
lines(lowess(disE2.yr$year,disE2.yr$sum),col='red', lwd=4)
abline(h=mean(disE.yr$sum), col="blue", lwd=4, lty=2)
mtext("D", side = 3, line = 1, adj = -0.05, font = 2)
