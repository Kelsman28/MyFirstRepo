library(tidyr)
library(stats)
library(reshape2)
library(car)

getwd()
[1] "C:/Users/Julie Kelso/Dropbox/SFS/SFS 2016/SFSAbstract2016"

All = read.csv("C:/Users/Julie Kelso/Dropbox/SFS/SFS 2016/SFSAbstract2016/All.csv")
Ind = read.csv("C:/Users/Julie Kelso/Dropbox/SFS/SFS 2016/SFSAbstract2016/IndexWsites.csv")
colnames(All)

### find a replace "Phospahte" and "Phosphate" with Phosphorus
All$VariableCode[All$VariableCode== "Phospahte"] <- "Phosphorus"
All$VariableCode[All$VariableCode== "Phosphate"] <- "Phosphorus"

w = dcast(All, Day + Month + Year + SiteCode + Watershed  ~ VariableCode, value.var = "DataValue", fun.aggregate = mean)

write.csv(w, "Allwide.csv")

colnames(Ind)[c(6)] <- c("SiteCode")

### merge in an attempt to find as many matches as possible
m = merge(w, Ind, by=c("Month","Year","SiteCode"), all.y = TRUE)

### to only have complete datasets
m = merge(w, Ind, by=c("Day","Month","Year","SiteCode"))

######3 remove duplicates according to multiple columns #####
done = m[!duplicated(m[c("Sample")]),]

### remove soapstone becuase has FI value of 3.5
done = done[c(-10),]
colnames(done)
 #### use complete makes it so ignores NA
cor(done$Ammonia, done$Nitrate, use = "complete")
cor(done$Ammonia, done$FI, use = "complete")
cor(done$Nitrate, done$FI, use = "complete")

plot(done$Ammonia, done$FI)
plot(done$Nitrate, done$FI)
plot(done$Phosphorus, done$FI)
plot(done$NPOC, done$FI)
plot(done$TDN, done$FI)

plot(done$Ammonia, done$BIX)
plot(done$Nitrate, done$BIX)
plot(done$Phosphorus, done$BIX)

plot(done$Ammonia, done$HIX)
plot(done$Nitrate, done$HIX)
plot(done$Phosphorus, done$HIX)

