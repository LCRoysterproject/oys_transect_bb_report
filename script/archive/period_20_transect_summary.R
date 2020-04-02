d <- read.csv("data/transect_data.csv", header = T)
d2 <- organizeData(d)
d3 <- calculateCountsDensity(d, d2)
p20 <- subset(d3, d3$period == 20)

p20$rocksBin <- "Y"
p20$rocksBin[p20$rocks == "NA"] <- "N"
p20$rocksBin <- as.factor(p20$rocksBin)
p20$fishrock <- paste(p20$harvest, p20$rocksBin, sep = "_")

nrow(p20[p20$rocksBin == "N",])
sum(p20$tran_length[p20$rocksBin == "N"])
mean(p20$tran_length[p20$rocksBin == "N"])

nrow(p20[p20$rocksBin == "Y",])
sum(p20$tran_length[p20$rocksBin == "Y"])
mean(p20$tran_length[p20$rocksBin == "Y"])

nrow(p20[p20$fishrock == "N_N",])
sum(p20$tran_length[p20$fishrock == "N_N"])
mean(p20$tran_length[p20$fishrock == "N_N"])

nrow(p20[p20$fishrock == "N_Y",])
sum(p20$tran_length[p20$fishrock == "N_Y"])
mean(p20$tran_length[p20$fishrock == "N_Y"])

nrow(p20[p20$fishrock == "Y_N",])
sum(p20$tran_length[p20$fishrock == "Y_N"])
mean(p20$tran_length[p20$fishrock == "Y_N"])

nrow(p20[p20$fishrock == "Y_Y",])
sum(p20$tran_length[p20$fishrock == "Y_Y"])
mean(p20$tran_length[p20$fishrock == "Y_Y"])


mo <- glm.nb(count_live ~ fishrock + offset(log(tran_length)), data = p20)
summary(mo)
p20_2 <- calculateCountsDensity(p20)
