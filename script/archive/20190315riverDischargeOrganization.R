##########################
## River Discharge Data ##
##########################

library("waterData")
library("reshape")
library("ggplot2")
library("lubridate")

dta3 <- read.csv("data/archive/transect_data_all_epochs.csv", header = T)

#data aggregated for each transect
#created in 20190315_Oyster_transects_DataMgmt_production.R

#station to analyze
station = '02323500'   

#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "2009-01-01") 

dis$year    = as.numeric(strftime(dis$dates,format="%Y"))

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(dis) <- c("StaID", "Discharge", "oldDate", "QualCode", "Year")


#Changing the format of the dates to be able to plot against time
dis$Date <- as.Date(dis$oldDate)

dis$Month <- month(dis$Date, label=TRUE)
dis$Month2 <- month(dis$Date, label=FALSE)

dis_mean_year<-  aggregate( Discharge ~ Year, dis, mean )
dis_mean_month<-  aggregate( Discharge ~ Month2, dis[dis$Year>2009,], mean )
dis_mean_my <- aggregate(Discharge ~ Month2+Year, dis, mean)
dis_mean_date<-  aggregate( Discharge ~ Date, dis, mean )
dis_mean_date$Month<- month(dis_mean_date$Date, label=TRUE)
dis_mean_date$Year <- year(dis_mean_date$Date)

#add annual discharge and monthly discharge to data (dta3)
for(i in 1:nrow(dta3)){
  y <- as.numeric(as.character(dta3$year[i]))
  m <- as.numeric(as.character(dta3$month[i]))
  ad <- dis_mean_year$Discharge[dis_mean_year$Year == y]
  ad_lag <- dis_mean_year$Discharge[dis_mean_year$Year == (y-1)]
  md <- dis_mean_my$Discharge[dis_mean_my$Month2 == m & dis_mean_my$Year == y]
  ifelse(m == 1, md_lag <- dis_mean_my$Discharge[dis_mean_my$Month2 == 12 & dis_mean_my$Year == (y-1)], 
    md_lag <- dis_mean_my$Discharge[dis_mean_my$Month2 == (m-1)& dis_mean_my$Year == y])
  ifelse(m == 2, md_lag2 <- dis_mean_my$Discharge[dis_mean_my$Month2 == 12 & dis_mean_my$Year == (y-1)],
    ifelse(m == 1, md_lag2 <- dis_mean_my$Discharge[dis_mean_my$Month2 == 11 & dis_mean_my$Year == (y-1)], 
      md_lag2 <- dis_mean_my$Discharge[dis_mean_my$Month2 == (m-2)& dis_mean_my$Year == y]))
  ifelse(m == 3, md_lag3 <- dis_mean_my_Discharge[dis_mean_my$Month2 == 12 & dis_mean_my$Year == (y-1)],
    ifelse(m == 2, md_lag3 <- dis_mean_my$Discharge[dis_mean_my$Month2 == 11 & dis_mean_my$Year == (y-1)],
      ifelse(m == 1, md_lag3 <- dis_mean_my$Discharge[dis_mean_my$Month2 == 10 & dis_mean_my$Year == (y-1)], 
        md_lag3 <- dis_mean_my$Discharge[dis_mean_my$Month2 == (m-3)& dis_mean_my$Year == y])))
  
  dta3$ann_dis[i] <- ad
  dta3$mon_dis[i] <- md
  dta3$ann_dis_lag[i] <- ad_lag
  dta3$mon_dis_lag[i] <- md_lag
  dta3$mon_dis_lag2[i] <- md_lag2
  dta3$mon_dis_lag3[i] <- md_lag3
}

dta3$ann_dis_sc <- dta3$ann_dis/max(dta3$ann_dis)
dta3$ann_dis_lag_sc <- dta3$ann_dis_lag/max(dta3$ann_dis_lag)
dta3$mon_dis_sc <- dta3$mon_dis/max(dta3$mon_dis)
dta3$mon_dis_lag_sc <- dta3$mon_dis_lag/max(dta3$mon_dis_lag)
dta3$mon_dis_lag2_sc <- dta3$mon_dis_lag2/max(dta3$mon_dis_lag2)
dta3$mon_dis_lag3_sc <- dta3$mon_dis_lag3/max(dta3$mon_dis_lag3)

write.csv(dta3, "data/finalTransectData_River.csv")
