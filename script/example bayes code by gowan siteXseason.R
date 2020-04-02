rm(list=ls(all=TRUE))
library('rjags')
library(MASS)

#read in data
d3 <- read.csv('tempData.csv',header=T)

#use data from all periods
LC = round(d3$count_live) #live counts - need to be whole numbers
tran_length = d3$tran_length #transect length - used as offset
site = d3$site
season = d3$season
period = as.factor(d3$period)
N = nrow(d3) #sample size

n.sites = length(unique(site))
n.seasons = length(unique(season))


########
# Site additive across seasons

#set up model object
jags=jags.model('negbin_season+site.txt',
                data=list('Y'=LC,'site'=site,'season'=season,'tran_length'=tran_length,
                          'n.sites'=n.sites,'n.seasons'=n.seasons,'N'=N),
                n.chains=4,n.adapt=1000)

#burn-in period
update(jags,1000)

#draw 1,000 samples from the sampler
k=coda.samples(jags,c('beta0','betaW','betaS','r'),n.iter=1000,thin=5)
summary(k)

# compare to MLE
m3 <- glm.nb(LC ~ site + season + offset(log(tran_length)))
summary(m3)

mle <- m3$coefficients
mle <- c(mle[1], mle[1]+mle[2], mle[1]+mle[3], #summer I, N, O
         mle[1]+mle[4], mle[1]+mle[4]+mle[2], mle[1]+mle[4]+mle[3]) #winter I, N, O

s <- summary(k)
m <- s$statistics[,"Mean"]
bayes <- c(m[1]+m[5]+m[2], m[1]+m[5]+m[3], m[1]+m[5]+m[4], #summer I, N, O
           m[1]+m[6]+m[2], m[1]+m[6]+m[3], m[1]+m[6]+m[4]) #winter I, N, O
cbind(bayes,mle)



########
# Site interactive with season (different site effects in different seasons)

#set up model object
jags=jags.model('negbin_seasonXsite.txt',
                data=list('Y'=LC,'site'=site,'season'=season,'tran_length'=tran_length,
                          'n.sites'=n.sites,'n.seasons'=n.seasons,'N'=N),
                n.chains=4,n.adapt=1000)

#burn-in period
update(jags,1000)

#draw 1,000 samples from the sampler
k=coda.samples(jags,c('beta','r'),n.iter=1000,thin=5)
summary(k)

# compare to additive model
s <- summary(k)
m <- s$statistics[,"Mean"]
int <- c(m[1], m[3], m[5], #summer I, N, O
         m[2], m[4], m[6]) #winter I, N, O
cbind(bayes,mle,int)

