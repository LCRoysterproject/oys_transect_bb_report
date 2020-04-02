rm(list=ls(all=TRUE))
library(rjags)
library(MASS)
library(lme4)

#read in data
d <- read.csv("data/transect_data.csv",header=T)
source("script/oyster_functions.R")
d2 <- organizeData(d)
d3 <- calculateCountsDensity(d,d2)

#data 
LC = round(d3$count_live) #live counts - need to be whole numbers
tran_length = d3$tran_length #transect length - used as offset
site = d3$site #site
locality = d3$locality #locality
period = as.factor(d3$period) #period as a factor
n.sites = length(unique(site)) #number of sites
n.localities = length(unique(locality)) #number of localities
n.periods = length(unique(period)) #number of periods
N = nrow(d3) #sample size
 
########
# Negative binomial model
# Period as categorical covariate, random effect
# Locality and site as categorical covariates, fixed effects

#set up model object
jags=jags.model("script/jags_txt/negbin_periodRE_localitysiteFE.txt",
                data=list('Y'=LC,'site'=site,'locality'=locality,'period'=period,'tran_length'=tran_length,
                          'n.sites'=n.sites,'n.localities'=n.localities,'n.periods'=n.periods,'N'=N),
                n.chains=4,n.adapt=1000)

#burn-in period
update(jags,1000)

#draw 1,000 samples from the sampler
k=coda.samples(jags,c('beta0','sigma','r','alpha','betaL','betaS'),n.iter=1000,thin=5)
summary(k)

# compare to MLE 
#fixed effects
m.FE <- glm.nb(count_live ~ locality + site + as.factor(period)+offset(log(tran_length)), data = d3)
#random effects
m.RE <- glmer.nb(count_live ~ locality + site + (1|as.factor(period))+offset(log(tran_length)), data = d3)



########
# Now - include detection
# N-mixture model
# Binomial detection probability
# Negative binomial abundance
# Period as categorical covariate, random effect
# Locality and site as categorical covariates, fixed effects

#create data for double passes
#pass 1 or pass2
#only subset of data have both
d # data set
d2 # organized data
dp <- doublePasses(d,d2)
#dp2 <- dp[duplicated(dp[,c(1:11)]) | duplicated(dp[,c(1:11)], fromLast = TRUE),]     


#need data in site x pass format
#rows are sites
#columns are passes
#then file for covariates
library(reshape2)
dp2 <- dcast(dp, year + season + period + treatment + locality + site + bar + station + strata + rocks + harvest + tran_length ~ pass, value.var = "count_live")

#data 
M <- nrow(dp2) #number of sites
J <- 2 #number of measurements per site (repeated counts, passes)
C <- as.matrix(cbind(dp2$`1`, dp2$`2`))

#covariates
tran_length = dp2$tran_length #transect length - used as offset
site = dp2$site #site
locality = dp2$locality #locality
period = as.factor(dp2$period) #period as a factor
#number of categories in each covariate
n.sites = length(unique(site)) #number of sites
n.localities = length(unique(locality)) #number of localities
n.periods = length(unique(period)) #number of periods


#set up model object
jags=jags.model("script/jags_txt/nmixmodel_doublepassdet_periodRE_localitysiteFE.txt",
                data=list('C'=C,'J'=J,'M'=M,'site'=site,'locality'=locality,'period'=period,'tran_length'=tran_length,
                          'n.sites'=n.sites,'n.localities'=n.localities,'n.periods'=n.periods),
                n.chains=8,n.adapt=1000)

#burn-in period
update(jags,1000)

#draw 1,000 samples from the sampler
k=coda.samples(jags,c('beta0','sigma','r','alpha','betaL','betaS','totalN','betap'),n.iter=1000,thin=5)
summary(k)
#detection
plogis(3.803)#0.98
#total abundance



#run same model using unmarked to compare estimates
#need to add covariates to these models
library(unmarked)
umf <- unmarkedFramePCount(y = C)
fit <- pcount(~1 ~1, data = umf)
summary(fit)


