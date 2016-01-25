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

######3 remove duplicates according to multiple columns #####
done = m[!duplicated(m[c("Sample")]),]

write.csv(done, "done.csv")
### to only have complete datasets
#m = merge(w, Ind, by=c("Day","Month","Year","SiteCode"))

colnames(done)

#calculate SUVA
done$SUVA = (done$abs254/done$NPOC)*100
#calculate DON
done$DON = done$TDN - (done$Ammonia + done$Nitrate)

### remov outliers  ###
done$SUVA[done$SUVA > 9 ] <- "NA"
done$FI[done$FI > 3 ] <- "NA"
done$DON[done$DON < 0 ] <- "NA"
done$TDN[done$TDN > 10 ] <- "NA"

 #### use complete makes it so ignores NA
cor(done$Ammonia, done$Nitrate, use = "complete")
cor(done$Ammonia, done$FI, use = "complete")
cor(done$Nitrate, done$FI, use = "complete")

dev.off()

plot(done$Ammonia, done$NPOC)
plot(done$Nitrate, done$NPOC)

write.csv(done, "done.csv")
done$Nitrate[done$Nitrate > 1.5 ] <- "NA"
done$Phosphorus[done$Phosphorus > 1.5 ] <- "NA"

cor.test(done$TDN, done$NPOC, use = "complete")
cor.test(done$DON, done$NPOC, use = "complete")

plot(done$TN, done$NPOC)
plot(done$DON, done$NPOC)
plot(done$DON, done$Ammonia

plot(done$Ammonia, done$FI)
plot(done$Nitrate, done$FI)
plot(done$Phosphorus, done$FI)
plot(done$NPOC, done$FI)

### remove outlier that is 25 mg/l NPOC
done$NPOC[done$NPOC > 20 ] <- "NA"

plot(done$TDN, done$FI)
plot(done$SUVA, done$FI)
plot(done$DON, done$FI)

plot(done$Ammonia, done$BIX)
plot(done$Nitrate, done$BIX)
plot(done$Phosphorus, done$BIX)
plot(done$NPOC, done$BIX)
plot(done$TDN, done$BIX)
plot(done$SUVA, done$BIX)
plot(done$DON, done$BIX)

plot(done$Ammonia, done$HIX)
plot(done$Nitrate, done$HIX)
plot(done$Phosphorus, done$HIX)
plot(done$NPOC, done$HIX)
plot(done$TDN, done$HIX)
plot(done$SUVA, done$HIX)
plot(done$DON, done$HIX)

plot(done$Ammonia, done$SUVA)
plot(done$Nitrate, done$SUVA)
plot(done$Phosphorus, done$SUVA)
plot(done$NPOC, done$SUVA)
plot(done$TDN, done$SUVA)
plot(done$DON, done$SUVA)

######### Correlations  ####

done <- as.data.frame(sapply(done, as.numeric)) 
write.csv(done, "done.csv")

sink("IndicesCorrelations.csv", append=FALSE, split=F)

cor.test(done$Ammonia, done$FI, use = "complete")
cor.test(done$Nitrate, done$FI,use = "complete")
cor.test(done$Phosphorus, done$FI,use = "complete")
cor.test(done$NPOC, done$FI,use = "complete")
cor.test(done$TDN, done$FI,use = "complete")
cor.test(done$SUVA, done$FI,use = "complete")
cor.test(done$DON, done$FI,use = "complete")

cor.test(done$Ammonia, done$BIX,use = "complete")
cor.test(done$Nitrate, done$BIX,use = "complete")
cor.test(done$Phosphorus, done$BIX,use = "complete")
cor.test(done$NPOC, done$BIX,use = "complete")
cor.test(done$TDN, done$BIX,use = "complete")
cor.test(done$SUVA, done$BIX,use = "complete")
cor.test(done$DON, done$BIX,use = "complete")

cor.test(done$Ammonia, done$HIX,use = "complete")
cor.test(done$Nitrate, done$HIX,use = "complete")
cor.test(done$Phosphorus, done$HIX,use = "complete")
cor.test(done$NPOC, done$HIX,use = "complete")
cor.test(done$TDN, done$HIX,use = "complete")
cor.test(done$SUVA, done$HIX,use = "complete")
cor.test(done$DON, done$HIX,use = "complete")

cor.test(done$Ammonia, done$SUVA)
cor.test(done$Nitrate, done$SUVA)
cor.test(done$Phosphorus, done$SUVA)
cor.test(done$NPOC, done$SUVA)
cor.test(done$TDN, done$SUVA)
cor.test(done$DON, done$SUVA)

sink()
getwd()


### All nutrients plus DOM and TDN
wNut = dcast(All, Day + Month + Year + SiteCode + Watershed  ~ VariableCode, value.var = "DataValue", fun.aggregate = mean)

wNut$DON = wNut$TDN - (wNut$Ammonia + wNut$Nitrate)
write.csv(wNut, "WideNutandDOM.csv")

wNut$DON[wNut$DON < 0 ] <- "NA"

plot(wNut$DON, wNut$NPOC)
