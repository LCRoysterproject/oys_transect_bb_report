rm(list=ls(all=TRUE))
library('rjags')
library(MASS)

#read in data
d3 <- read.csv('tempData.csv',header=T)

#use data from all periods
LC = round(d3$count_live) #live counts - need to be whole numbers
tran_length = d3$tran_length #transect length - used as offset
site = d3$site
period = as.factor(d3$period)
n.periods = length(unique(period))
N = nrow(d3) #sample size


########
# Period as categorical covariate, fixed effect

#set up model object
jags=jags.model('jags_txt/negbin_period.txt',
                data=list('Y'=LC,'period'=period,'tran_length'=tran_length,'n.periods'=n.periods,'N'=N),
                n.chains=4,n.adapt=1000)

#burn-in period
update(jags,1000)

#draw 1,000 samples from the sampler
k=coda.samples(jags,c('beta0','betaS','r'),n.iter=1000,thin=5)
summary(k)

# compare to MLE
m2 <- glm.nb(LC ~ period + offset(log(tran_length)))
summary(m2)

mle <- m2$coefficients

s <- summary(k)
m <- s$statistics[,"Mean"]
bayes <- c(m[1], m[3:(n.periods+1)])
cbind(bayes,mle)


########
# Period as categorical covariate, random effect

#set up model object
jags=jags.model('jags_txt/negbin_periodRE.txt',
                data=list('Y'=LC,'period'=period,'tran_length'=tran_length,'n.periods'=n.periods,'N'=N),
                n.chains=4,n.adapt=1000)

#burn-in period
update(jags,1000)

#draw 1,000 samples from the sampler
k=coda.samples(jags,c('beta0','sigma','r','alpha'),n.iter=1000,thin=5)
summary(k)

# compare to MLE with fixed effects
fe <- m2$coefficients
fe[2:n.periods] <- fe[1] + fe[2:n.periods] #period1 (reference) + period effect

s <- summary(k)
m <- s$statistics[,"Mean"]
re <- m[1:n.periods] + m['beta0']
cbind(re,fe)
