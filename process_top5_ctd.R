#summarize top 5 m physical data, add lat & lon, export for use in ArcGIS

library(plyr)
library(tidyverse)


d <- read.csv("top5m_physicalData.csv", header = T)
d <- d[,c(3:length(d))]
names(d)<- sapply(X = names(d), FUN = tolower)
d <- d[,c(-7,-10)] #remove duplicate 'depth' and 'id' column
d$fluoro <- NULL #remove to use the fluorescen data from the 'fluoroData' file

f <- read.csv("max5_fluoroData.csv", header = T)
f <- f[,c(2:length(f))]
names(f)<- sapply(X = names(f), FUN = tolower)
f <- f[f$depth <= 5,]

g <- read.csv("ctd_all_odv.csv",header = T)

#get cruise and station metadata
g.data <- g %>% group_by(sta.seamap) %>% summarise(lat = min(sta_lat), lon = min(sta_lon))
d <- merge(x = d, y = g.data, by.x = "seamap", by.y = "sta.seamap", all.x = T)

#find mean for all variables
d.mean <- d %>% group_by(seamap,cruise,storm) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
f.mean <- f %>% group_by(seamap,cruise,storm) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
#rename depth in fluoro before merging
f.mean <- rename(f.mean, c("depth.fluoro" = "depth"))

#merge d.mean and f.mean
df <- merge(x = d.mean, y = f.mean, by = c( "cruise","seamap","storm"), all.x = T)

#re-order variables
df <- df[,c(1:3,10:11,4:9,12:13)]

#split by storm for ease of use in ArcMap
r <- df[df$storm == "Rita",]
write.csv(x = r, file = "top5m_Rita.csv", row.names = F)

i <- df[df$storm == "Ike",]
write.csv(x = i, file = "top5m_Ike.csv", row.names = F)

h <- df[df$storm == "Harvey",]
write.csv(x = h, file = "top5m_Harvey.csv", row.names = F)

n <-  df[df$storm == "None",]
write.csv(x = n, file = "top5m_None.csv", row.names = F)
