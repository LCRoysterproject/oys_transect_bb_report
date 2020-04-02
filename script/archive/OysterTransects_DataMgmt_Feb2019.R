###########################################
## Oyster Transects - Data Management    ##
## Jennifer Moore, Bill Pine, Mel Moreno ##
## Updated: February 2019                ##
###########################################

#load packages
library("lubridate") #format dates

#read in data
#transect production file
#could change this to read directly from excel workbook
tr <- read.csv("data/transect_data_production.csv", header = T)

#format date column as a date object
tr$date<-mdy(tr$date)

#create season column
tr$season<-ifelse(tr$month ==1 | tr$month == 10 | tr$month == 11 | tr$month == 12, "Winter", "Summer")

#this removes the odd data from Jan 2018 with the grub box
tr<- tr[tr$date != "2018-01-30",]

#for 2018/2019 there are 2 passes (pass 1, pass 2)
#average values together for this - can change this later if we calculate detection probabilities
dta0=aggregate(count_live~date+day+month+year+season+treatment+start_time+
                 end_time+locality+site+bar+station+transect+tran_length,data = tr, FUN = "mean")

#aggregate live count data for each transect
#first remove all rows with -999 then sum live count for each transect segment
dta0.2 <- dta0[dta0$count_live > -1,]
#oyster live counts by transect
live=aggregate(count_live~day+month+year+season+treatment+locality+site+bar+station,data=dta0.2,sum)

#aggregate transect length for each transect
#for each row with -999 reduce transect length by 2.5

#max length for each transect
dta2=aggregate(tran_length~day+month+year+season+treatment+locality+site+bar+station+transect,data=dta0,max)

#find rows with -999
miss <- which(dta0$count_live < -1)
for(i in 1:length(miss)){
  ind <- miss[i]
  station <- dta0$station[ind]
  transect <- dta0$transect[ind]
  #subtract 2.5 from the transect length for this particular station/transect that has a missing value
  dta2$tran_length[dta2$station == station & dta2$transect == transect] <- dta2$tran_length[dta2$station == station & dta2$transect == transect] - 2.5
}

#sum over all transects
tranlength=aggregate(tran_length~day+month+year+season+treatment+locality+site+bar+station,data=dta2,sum)


#merge live count total data frame with the tran_length total data frame
dta3=merge(live,tranlength,by=c("day","month","year","season","treatment","locality","site","bar","station"))

#calculate density
dta3$area = dta3$tran_length*.1524
dta3$density = dta3$count_live/dta3$area

#read in strata file
st <- read.csv("data/strata.csv", header = T)
#attach strata data to final file
for(i in 1:nrow(dta3)){
  station <- as.character(dta3$station[i])
  strata <- as.character(st$strata[st$station == station])
  ifelse(length(strata) == 0, dta3$strata[i] <- NA, dta3$strata[i] <- strata)
}

#sort data by year, month, and station
sort.dta3<-dta3[order(dta3$year, dta3$month, dta3$station),]

#write cleaned file to .csv
write.csv(sort.dta3,file="transect_data_clean.csv")


