#process_ENV_data for ArcGIS to make Figure 1 of manuscript

d <- read.csv("all_EnvTaxaPcDiv.csv",header = T, stringsAsFactors = F)
#remove extra fields
d <- d[,c(3:59)]
d <- d[,c(1:9)]
head(d)

colnames(d) <- c("seamap_stn","seamap_cruise","salinity","water_temp","strat","fluor","lat","lon","depth_m")
d$lon <- d$lon*-1

get_storm <- function(cruise){

  if(cruise == "RP1"){
    storm <- "Harvey"
  } else if(cruise == "SEAMAP"){
    storm <- "Harvey"
  } else if(cruise == "0505"){
    storm <- "Rita"
  } else if(cruise == "0805"){
    storm <- "Ike"
  } else if(cruise == "0284"){
    storm <- "Ike"
  } else {
    storm <- "None"
  }
}

d$storm <- sapply(X = d$seamap_cruise, FUN = get_storm)

write.csv(x = d, file = "all_ENV_historical.csv", row.names = F)

#get 'non-storm' yearly averages
library(tidyverse)
n <- d[d$storm == "None",]
ns <- n %>% group_by(seamap_stn, storm) %>% summarise(salinity_avg = mean(salinity), water_temp_avg = mean(water_temp), lat = mean(lat), lon=mean(lon))

write.csv(x = ns, file = "none_storm_ENV.csv", row.names = F)
