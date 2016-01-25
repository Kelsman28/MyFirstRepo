library(tidyr)
library(stats)
library(reshape2)


getwd()
[1] "C:/Users/Julie Kelso/Dropbox/SFS/SFS 2016/SFSAbstract2016"

All = read.csv("C:/Users/Julie Kelso/Dropbox/SFS/SFS 2016/SFSAbstract2016/All.csv")
Ind = read.csv("C:/Users/Julie Kelso/Dropbox/SFS/SFS 2016/SFSAbstract2016/IndexWsites.csv")
colnames(All)

aql <- melt(airquality, id.vars = c("month", "day"))
aqw <- dcast(aql, month + day ~ variable)

w = dcast(All, Day + Month + Year + SiteCode + Watershed  ~ VariableCode, value.var = "DataValue", fun.aggregate = mean)

write.csv(w, "Allwide.csv")

colnames(Ind)[c(6)] <- c("SiteCode")

m = merge(w, Ind, by=c("Month","Year","SiteCode"), all.y = TRUE)

######3 remove duplicates according to multiple columns #####
done = m[!duplicated(m[c("Sample")]),]
