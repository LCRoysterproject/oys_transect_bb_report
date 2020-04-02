
source('script/OysterFunctions.R')

data <- read.csv('data/transect_data_production.csv', header = T)

#organize data 
data2 <- organizeData(data)

#calculate live counts and density
#input data is original data set and organized data set
data3 <- calculateCountsDensity(data, data2)

#summary tables for effort
summaryEffort(data3)

#summary tables for live counts
summaryCounts(data3)

#summary tables for density
summaryDensity(data3)

#boxplots for density
plotsDensity(data3)

#summary tables for recent data (period 19 and 20)
summaryRecent(data3)

#pull out data with 2 passes
#input is organized dataset (i.e., data2) not one that has been through live counts and density
dp <- doublePasses(data2)
