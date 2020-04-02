#######################
## Data Organization ##
#######################

library("lubridate") #for date formatting
library("MASS") #for negative binomial model
library("coefplot")

#read in data
transect<- read.csv("data/transect_data.csv", header= T)

#create year, month, day columns
transect$date<-mdy(transect$date)
transect$year<- year(transect$date)
transect$month<-month(transect$date)
transect$day<-day(transect$date)

#create season column
transect$Season<-ifelse(transect$month ==1 | transect$month == 10 | transect$month == 11 | transect$month == 12, "Winter", "Summer")
#Convert season to factor
transect$Season<-as.factor(transect$Season)


#Not all of the observations have time, so time would not parse all of the values
transect$date<- ymd(with(transect, paste(year,month,day, sep="-")))

#remove rows with NA for count_live
transect2 <- transect[!is.na(transect$count_live),]

#remove rows with NA for tran_length
transect2 <-transect[!is.na(transect$tran_length),]

#aggregate data for each transect 
dta=aggregate(count_live~day+month+year+Season+treatment+locality+site+bar+station,data=transect2,sum)
#any transects with all 0s - yes, 6 of 256! (2%)

#aggregate data for transect length
#note this is taking the max of the transect lengths, but need to make sure this is doing 
#this on a per transect basis.  So if you have 3 replicate transects, tran 1 = 20 total, tran 2=
#18, tran 3= 22 these will eventually need to sum
# max has to be used because transect length records the "segments" 2.5, 5, up to the max length
# so if you sum this then you end up with the wrong total length of transect 

dta2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+station+replicate,data=transect2,max)

#so by including replicate in dta2 it determines maximum length of each transect when multiple 
#transects done on a bar.

#now need to sum those max transects, this gives you the total length of all 
#oyster bar measured on a day when multiple transects were done on a bar
dta2.2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+station,data=dta2,sum)

#merge data frames
dta3=merge(dta,dta2.2,by=c("day","month","year","Season","treatment","locality","site","bar","station"))

#calculate density
dta3$area = dta3$tran_length*.154
dta3$density = dta3$count_live/dta3$area


##################
## GLM Analysis ##
##################

#live counts - response variable

# Count Histogram, for assessing GLM family fit to count data
hist(dta3$count_live,breaks=40,freq=FALSE,col=8)
theta=c(mu=1,k=0.2)
nb_LL=function(theta)
{
  -sum(dnbinom(dta3$count_live,mu=theta[1],size=theta[2],log=TRUE))
}
fit_nb=optim(theta,nb_LL)
lines(seq(0,4000,100),dnbinom(seq(0,4000,100),mu=fit_nb$par[1],size=fit_nb$par[2]),col=2)

#nb looks good


#using total transect length as a covariate
fm <- glm(count_live ~ Season + month + year + treatment + locality + site + bar+ tran_length, family = "poisson", data = dta3)
#overdispersed - try quasi poisson
fmq <- glm(count_live ~ Season + month + year + treatment + locality + site + bar+ tran_length, family = "quasipoisson", data = dta3)
#significant factors - treatment, site, bar
fmq_1 <- glm(count_live ~ treatment + site + + bar + tran_length, family = "quasipoisson", data = dta3)
fmq_2 <- glm(count_live ~ treatment + site + tran_length, family = "quasipoisson", data = dta3)
#fmq_2 best model - treatment, site, and tran_length
#try negative binomial
ffmq_3 <- glm.nb(count_live ~ treatment+site+tran_length, data = dta3)

#using max transect length as an offset
fmo <- glm(count_live ~ Season + month + year + treatment + locality + site + bar, offset = log(tran_length), family = "poisson", data = dta3)
#overdispersed - try quasi poisson
fmoq <- glm(count_live ~ Season + month + year + treatment + locality + site + bar, offset = log(tran_length), family = "quasipoisson", data = dta3)
fmoq_1 <- glm(count_live ~ treatment + site + bar, offset = log(tran_length), family = "quasipoisson", data = dta3)
fmoq_2 <- glm(count_live ~ treatment + site, offset = log(tran_length), family = "quasipoisson", data = dta3)

#try negative binomial
fmoq_3 <- glm.nb(count_live ~ treatment+site+offset(log(tran_length)), data = dta3)

coefplot(fmoq_3)
plot(residuals(fmoq_3))
abline(h=0)

###########################
## Lone Cabbage Analysis ##
###########################

LC <- dta3[dta3$locality == "LC",]

#using max transect length as a covariate
lc_fm <- glm(count_live ~ Season + month + year + treatment + site + bar+ tran_length, family = "poisson", data = LC)
#overdispersed - try quasi poisson
lc_fmq <- glm(count_live ~ Season + month + year + treatment + site + bar+ tran_length, family = "quasipoisson", data = LC)
#significant factors - treatment, site, bar
lc_1 <- glm(count_live ~ treatment + site + + bar + tran_length, family = "quasipoisson", data = LC)
lc_2 <- glm(count_live ~ treatment + site + tran_length, family = "quasipoisson", data = LC)
#fmq_2 best model - treatment, site, and tran_length
#try negative binomial
lc_3 <- glm.nb(count_live ~ treatment+site+tran_length, data = LC)

#using max transect length as an offset
lc_fmo <- glm(count_live ~ Season + month + year + treatment + site + bar, offset = log(tran_length), family = "poisson", data = LC)
#overdispersed - try quasi poisson
lc_fmoq <- glm(count_live ~ Season + month + year + treatment + site + bar, offset = log(tran_length), family = "quasipoisson", data = LC)
lc_o_1 <- glm(count_live ~ treatment + site + bar, offset = log(tran_length), family = "quasipoisson", data = LC)
lc_o_2 <- glm(count_live ~ treatment + site, offset = log(tran_length), family = "quasipoisson", data = LC)
#try negative binomial
lc_o_3 <- glm.nb(count_live ~ treatment+site+offset(log(tran_length)), data = LC)
lc_o_4 <- glm.nb(count_live ~ treatment*site*offset(log(tran_length)), data = LC)

# #########################################################
# # GLM TO MODEL RESTORATION EFFECT OF LIVE OYSTERS.  
# #+0 forces intercept through zero, so check this my doing a confint(mod) to see if
# #it contains zero, of so then can force to zero

# 
# some of these are just to think about rock treatment vs. location - which is bigger effect

full.mod<-glm.nb(count_live~year+Season+locality+site+treatment+offset(log(tran_length))-1,data=dta3)
summary(full.mod)
coefplot(full.mod)
#only locality, site, treatment significant

mod1<-glm.nb(count_live~locality+site+treatment+offset(log(tran_length))-1,data=dta3)
summary(mod1)
coefplot(mod1)
#profile the parameter estimates
confint(mod1)

#now go to LC only

LC.full.mod<-glm.nb(count_live~year+Season+site+treatment+offset(log(tran_length))-1,data=LC)
summary(LC.full.mod)
coefplot(LC.full.mod)
#only site, treatment significant

mod1<-glm.nb(count_live~site+treatment+offset(log(tran_length))-1,data=LC)
summary(mod1)
coefplot(mod1)
#profile the parameter estimates
confint(mod1)

beta_mod1=coef(mod1)

#diff in inshore vs nearshore
exp(beta_mod1[1]-beta_mod1[2])
#12.7 times mean

#diff in inshore vs offshore
exp(beta_mod1[1]-beta_mod1[3])
#5.5 times mean

#diff in nearshore vs offshore
exp(beta_mod1[2]-beta_mod1[3])
#0.43 times mean

#diff in adding rocks
exp(beta_mod1[4])
#2.1 times mean

##thinking about year
mod1.yr<-glm.nb(count_live~year+site+treatment+offset(log(tran_length))-1,data=dta3)
summary(mod1.yr)
coefplot(mod1.yr)
#profile the parameter estimates
confint(mod1)
 
plot(count_live~year, data=dta3)
#now relabel as rock or control follow tim's guide
points(count_live[LC$treatment=="rocks"]~year[LC$treatment=="rocks"],data=dta3, col='red', pch=19)
points(count_live[LC$treatment=="control"]~year[LC$treatment=="control"],data=dta3, col='blue', pch=1)

#build the predicted line
pred_LC=predict(mod1.yr,list(year=2015,site="O",treatment="rocks",tran_length=60),type="response")


### Needs to be worked on

#######################
## Data Organization ##
#######################

library("lubridate") #for date formatting
library("MASS") #for negative binomial model
library("coefplot")

#read in data
transect<- read.csv("data/transect_data.csv", header= T)

#create year, month, day columns
transect$date<-mdy(transect$date)
transect$year<- year(transect$date)
transect$month<-month(transect$date)
transect$day<-day(transect$date)

#create season column
transect$Season<-ifelse(transect$month ==1 | transect$month == 10 | transect$month == 11 | transect$month == 12, "Winter", "Summer")
#Convert season to factor
transect$Season<-as.factor(transect$Season)


#Not all of the observations have time, so time would not parse all of the values
transect$date<- ymd(with(transect, paste(year,month,day, sep="-")))

#remove rows with NA for count_live
transect2 <- transect[!is.na(transect$count_live),]

#remove rows with NA for tran_length
transect2 <-transect[!is.na(transect$tran_length),]

#aggregate data for each transect 
dta=aggregate(count_live~day+month+year+Season+treatment+locality+site+bar+station,data=transect2,sum)
#any transects with all 0s - yes, 6 of 256! (2%)

#aggregate data for transect length
#note this is taking the max of the transect lengths, but need to make sure this is doing 
#this on a per transect basis.  So if you have 3 replicate transects, tran 1 = 20 total, tran 2=
#18, tran 3= 22 these will eventually need to sum
# max has to be used because transect length records the "segments" 2.5, 5, up to the max length
# so if you sum this then you end up with the wrong total length of transect 

dta2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+station+replicate,data=transect2,max)

#so by including replicate in dta2 it determines maximum length of each transect when multiple 
#transects done on a bar.

#now need to sum those max transects, this gives you the total length of all 
#oyster bar measured on a day when multiple transects were done on a bar
dta2.2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+station,data=dta2,sum)

#merge data frames
dta3=merge(dta,dta2.2,by=c("day","month","year","Season","treatment","locality","site","bar","station"))

#calculate density
dta3$area = dta3$tran_length*.154
dta3$density = dta3$count_live/dta3$area


##################
## GLM Analysis ##
##################

#live counts - response variable

# Count Histogram, for assessing GLM family fit to count data
hist(dta3$count_live,breaks=40,freq=FALSE,col=8)
theta=c(mu=1,k=0.2)
nb_LL=function(theta)
{
  -sum(dnbinom(dta3$count_live,mu=theta[1],size=theta[2],log=TRUE))
}
fit_nb=optim(theta,nb_LL)
lines(seq(0,4000,100),dnbinom(seq(0,4000,100),mu=fit_nb$par[1],size=fit_nb$par[2]),col=2)

#nb looks good


#using total transect length as a covariate
fm <- glm(count_live ~ Season + month + year + treatment + locality + site + bar+ tran_length, family = "poisson", data = dta3)
#overdispersed - try quasi poisson
fmq <- glm(count_live ~ Season + month + year + treatment + locality + site + bar+ tran_length, family = "quasipoisson", data = dta3)
#significant factors - treatment, site, bar
fmq_1 <- glm(count_live ~ treatment + site + + bar + tran_length, family = "quasipoisson", data = dta3)
fmq_2 <- glm(count_live ~ treatment + site + tran_length, family = "quasipoisson", data = dta3)
#fmq_2 best model - treatment, site, and tran_length
#try negative binomial
ffmq_3 <- glm.nb(count_live ~ treatment+site+tran_length, data = dta3)

#using max transect length as an offset
fmo <- glm(count_live ~ Season + month + year + treatment + locality + site + bar, offset = log(tran_length), family = "poisson", data = dta3)
#overdispersed - try quasi poisson
fmoq <- glm(count_live ~ Season + month + year + treatment + locality + site + bar, offset = log(tran_length), family = "quasipoisson", data = dta3)
fmoq_1 <- glm(count_live ~ treatment + site + bar, offset = log(tran_length), family = "quasipoisson", data = dta3)
fmoq_2 <- glm(count_live ~ treatment + site, offset = log(tran_length), family = "quasipoisson", data = dta3)

#try negative binomial
fmoq_3 <- glm.nb(count_live ~ treatment+site+offset(log(tran_length)), data = dta3)

coefplot(fmoq_3)
plot(residuals(fmoq_3))
abline(h=0)

###########################
## Lone Cabbage Analysis ##
###########################

LC <- dta3[dta3$locality == "LC",]

#using max transect length as a covariate
lc_fm <- glm(count_live ~ Season + month + year + treatment + site + bar+ tran_length, family = "poisson", data = LC)
#overdispersed - try quasi poisson
lc_fmq <- glm(count_live ~ Season + month + year + treatment + site + bar+ tran_length, family = "quasipoisson", data = LC)
#significant factors - treatment, site, bar
lc_1 <- glm(count_live ~ treatment + site + + bar + tran_length, family = "quasipoisson", data = LC)
lc_2 <- glm(count_live ~ treatment + site + tran_length, family = "quasipoisson", data = LC)
#fmq_2 best model - treatment, site, and tran_length
#try negative binomial
lc_3 <- glm.nb(count_live ~ treatment+site+tran_length, data = LC)

#using max transect length as an offset
lc_fmo <- glm(count_live ~ Season + month + year + treatment + site + bar, offset = log(tran_length), family = "poisson", data = LC)
#overdispersed - try quasi poisson
lc_fmoq <- glm(count_live ~ Season + month + year + treatment + site + bar, offset = log(tran_length), family = "quasipoisson", data = LC)
lc_o_1 <- glm(count_live ~ treatment + site + bar, offset = log(tran_length), family = "quasipoisson", data = LC)
lc_o_2 <- glm(count_live ~ treatment + site, offset = log(tran_length), family = "quasipoisson", data = LC)
#try negative binomial
lc_o_3 <- glm.nb(count_live ~ treatment+site+offset(log(tran_length)), data = LC)
lc_o_4 <- glm.nb(count_live ~ treatment*site*offset(log(tran_length)), data = LC)

# #########################################################
# # GLM TO MODEL RESTORATION EFFECT OF LIVE OYSTERS.  
# #+0 forces intercept through zero, so check this my doing a confint(mod) to see if
# #it contains zero, of so then can force to zero

# 
# some of these are just to think about rock treatment vs. location - which is bigger effect

full.mod<-glm.nb(count_live~year+Season+locality+site+treatment+offset(log(tran_length))-1,data=dta3)
summary(full.mod)
coefplot(full.mod)
#only locality, site, treatment significant

mod1<-glm.nb(count_live~locality+site+treatment+offset(log(tran_length))-1,data=dta3)
summary(mod1)
coefplot(mod1)
#profile the parameter estimates
confint(mod1)

#now go to LC only

LC.full.mod<-glm.nb(count_live~year+Season+site+treatment+offset(log(tran_length))-1,data=LC)
summary(LC.full.mod)
coefplot(LC.full.mod)
#only site, treatment significant

mod1<-glm.nb(count_live~site+treatment+offset(log(tran_length))-1,data=LC)
summary(mod1)
coefplot(mod1)
#profile the parameter estimates
confint(mod1)

beta_mod1=coef(mod1)

#diff in inshore vs nearshore
exp(beta_mod1[1]-beta_mod1[2])
#12.7 times mean

#diff in inshore vs offshore
exp(beta_mod1[1]-beta_mod1[3])
#5.5 times mean

#diff in nearshore vs offshore
exp(beta_mod1[2]-beta_mod1[3])
#0.43 times mean

#diff in adding rocks
exp(beta_mod1[4])
#2.1 times mean

##thinking about year
mod1.yr<-glm.nb(count_live~year+site+treatment+offset(log(tran_length))-1,data=dta3)
summary(mod1.yr)
coefplot(mod1.yr)
#profile the parameter estimates
confint(mod1)

plot(count_live~year, data=dta3)
#now relabel as rock or control follow tim's guide
points(count_live[LC$treatment=="rocks"]~year[LC$treatment=="rocks"],data=dta3, col='red', pch=19)
points(count_live[LC$treatment=="control"]~year[LC$treatment=="control"],data=dta3, col='blue', pch=1)

#build the predicted line
pred_LC=predict(mod1.yr,list(year=2015,site="O",treatment="rocks",tran_length=60),type="response")





