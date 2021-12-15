
#Plot RAPID and historical SEAMAP
library(tidyverse)
library("plyr")
library("grid")
library("gridExtra")
library("scales")
library("oce")
library("reshape2")
library("akima")
library(metR)

#setwd("C:/Users/Kelly Robinson/Dropbox/Louisiana/funding_external/awarded/NSF_RAPID")

# data:
d <- read.csv(file = "ctd_all_odv.csv", header = T)

d$density <- swRho(pressure = d$sta_dpth, salinity = d$salinity, temperature = d$temp, longitude = d$sta_lon, latitude = d$sta_lat)

#Call functions: -----------------

# Compute the distance from Galveston Bay
dist.from.galv.bay <- function(latitude1, longitude1) {
  gb.lat <- round(29.284475866646464,digits = 4)
  gb.lon <- round(-94.78494305562033,digits = 4)
  library("oce")
  geodDist(latitude1 = latitude1, longitude1 = longitude1, latitude2=gb.lat, longitude2=gb.lon)
}

# Spectral colour map from ColorBrewer
  spectral <- function(n=6) {
    library("RColorBrewer")
    rev(brewer.pal(name="Spectral", n=n))
  }

  scale_fill_spectral <- function(...) {
    scale_fill_gradientn(colours=spectral(...))
  }
  scale_colour_spectral <- function(...) {
    scale_colour_gradientn(colours=spectral(...))
  }

  #used to match ArcGIS color guide
  alt.spectral <- (c("steelblue2","aquamarine4","palegreen3","khaki1",
                     "sienna1","tomato1","red3"))

 # Interpolate a slice of data for which the x-axis is a distance in nautical miles
  interp.dist <- function(x, y, z, anisotropy=1000, x.step=500, y.step=2.5, smooth=FALSE, theta=0.2, ...) {
      #
      # Interpolate data over a distance coordinate
      #
      # x   vector of distance *IN KILOMETERS*
      # y   vector of depth in m
      # z   vector of measured variable
      # anisotropy  anisotropy ratio between x and y
      # x/y.step    interpolation grid steps in m
      # smooth      boolean, wether to smooth the first interpolation using fields::image.smooth
      # x/y.step.smooth   interpolation grid step for the smoothing
      # grid.smooth intepolation grid for the smoothing, overrides x/y.step.smooth
      # theta       bandwidth for the kernel smoother in fields::image.smooth

      library("akima")
      library("reshape2")

      # correct x-axis for anisotropy between horizontal and vertical
      x <- x*1852/anisotropy #if x unit is nautical miles

      # interpolate
      i <- interp(x=x, y=y, z=z, xo=seq(0, max(x), by=x.step/anisotropy), yo=seq(0, max(y), by=y.step), ...)

      # smooth
      if ( smooth ) {
        library("fields")
        i <- image.smooth(i, grid=list(x=i$x, y=i$y), theta=theta)
      }

      # extract a data.frame
      out <- melt(i$z, varnames=c("x","y"))
      out$x <- i$x[out$x] * anisotropy/1852 #use /1852 if plotting x-axis as nautical miles
      out$y <- i$y[out$y]

      return(out)
    }

# set variable names----

  vars <- c("salinity")

#assign Hurricane names using cruises

get.storm <- function(cruise){
  if(cruise == "SEAMAP") {
    storm <- "Harvey"
  } else if(cruise == "RP1") {
    storm <- "Harvey"
  } else if (cruise== "0284") {
    storm <- "Ike"
  } else if (cruise== "0804") {
    storm <- "Ike"
  } else if (cruise== "0805") {
    storm <- "Ike"
  } else if (cruise== "0505") {
    storm <- "Rita"
  } else {
    storm <- 'no.storm'
  }
  return(storm)
}

d$storm <- NA

for(i in 1:nrow(d)){

  d[i,]$storm <- get.storm(cruise = d[i,]$cruise)
}


##Section plots ----
#by cruise # ----

plot.theme <- theme(axis.text = element_text(size = 10, colour = "black"),
                      axis.title = element_text(size = 11),
                      strip.text = element_text(size=12, face="bold"),
                      strip.background = element_rect(fill = "white", color = "black"),
                      panel.background = element_rect(fill = "grey60"),
                      plot.title = element_text(lineheight=1.0, face="bold"))

ddply(.data = d, .variables = c("syear","cruise"), function(x){

 # x <- d[d$syear==2017 & d$cruise=="SEAMAP",]

  x$distanceFromStart <- dist.from.galv.bay(latitude1 = x$sta_lat, longitude1 = x$sta_lon)

  dm <- melt(x, id.vars=c("depth", "distanceFromStart"), measure.vars=vars)

  di <- ddply(dm, .variables = c("variable"), function(i) {
    x <- interp.dist(x=i$distanceFromStart, y=i$depth, z=i$value, duplicate="mean", x.step=300, y.step=1, anisotropy=1000)
  }, .progress = "text")

  names(di)[names(di)=="x"] <- "distance"
  names(di)[names(di)=="y"] <- "depth"

  temperature

    min.t <- min(di[di$variable=="temp",]$value, na.rm = T)
    max.t <- max(di[di$variable=="temp",]$value, na.rm = T)

    t <- ggplot(data = di[di$variable=="temp",], aes(x=distance, y=-depth)) +
      geom_tile(aes(fill=value), na.rm=T) +
      stat_contour(aes(z=value), colour="black", alpha=0.7, bins=5, size=0.5, na.rm=TRUE) +
      geom_text_contour(aes(z = round(x = value, digits = 0)) +
      scale_fill_gradientn(colours = spectral(), limits = c(min.t, max.t), na.value = NA, name = "temp (°C)") +
      scale_color_gradient(high = spectral()) +
      coord_cartesian(xlim = c(30,145), ylim = c(-50,-2)) +
      scale_x_continuous(name = "distance (km)", breaks = seq(0,150,10)) +
      scale_y_continuous(name =  "depth (m)", limits = c(-50, 0), breaks = seq(-50,0,5)) + facet_grid(variable~.) +
      plot.theme +
      ggtitle(paste0(unique(x$syear), ": ", unique(x$cruise))) + theme(plot.title = element_text(lineheight=1.0, face="bold"))

  #salinity

    min.s <- min(di[di$variable=="salinity",]$value, na.rm = T)
    max.s <- max(di[di$variable=="salinity",]$value, na.rm = T)


  s <- ggplot(di[di$variable == "salinity",], aes(x=distance, y=-depth)) +
    geom_tile(aes(fill=value), na.rm=T) +
    stat_contour(aes(z=value), colour="black", alpha=0.7, bins=5, size=0.5, na.rm=TRUE) +
    geom_text_contour(aes(z = value)) +
    scale_fill_gradientn(colours = spectral(), limits = c(min.s,max.s), na.value=NA, name = "sal (psu)") +
    scale_color_gradient(high = spectral()) +
    coord_cartesian(xlim = c(30,145), ylim = c(-50,-2)) +
    scale_x_continuous(name = "distance (km)", breaks = seq(0,150,10)) +
    scale_y_continuous(name =  "depth (m)", limits = c(-50, 0), breaks = seq(-50,0,5)) + facet_grid(variable~.) +
    plot.theme

#density

  min.d <- min(di[di$variable=="density",]$value, na.rm = T)
  max.d <- max(di[di$variable=="density",]$value, na.rm = T)


  den <- ggplot(di[di$variable == "density",], aes(x=distance, y=-depth)) +
    geom_tile(aes(fill=value), na.rm=T) +
    stat_contour(aes(z=value), colour="black", alpha=0.7, bins=5, size=0.5, na.rm=TRUE) +
    geom_text_contour(aes(z = value)) +
    scale_fill_gradientn(colours = spectral(), limits = c(min.d,max.d), na.value=NA, name = "sal (psu)") +
    scale_color_gradient(high = spectral()) +
    coord_cartesian(xlim = c(30,145), ylim = c(-50,-2)) +
    scale_x_continuous(name = "distance (km)", breaks = seq(0,150,10)) +
    scale_y_continuous(name =  "depth (m)", limits = c(-50, 0), breaks = seq(-50,0,5)) + facet_grid(variable~.) +
    plot.theme

#fluro
  #
  # min.f <- min(di[di$variable=="fluoro",]$value, na.rm = T)
  # max.f <- max(di[di$variable=="fluoro",]$value, na.rm = T)
  #
  #
  # f <- ggplot(di[di$variable == "fluoro",], aes(x=distance, y=-depth)) +
  #   geom_tile(aes(fill=value), na.rm=T) +
  #   stat_contour(aes(z=value), colour="black", alpha=0.7, bins=5, size=0.5, na.rm=TRUE) +
  #   geom_text_contour(aes(z = value)) +
  #   scale_fill_gradientn(colours = spectral(), limits = c(min.f,max.f), na.value=NA, name = "fluoro") +
  #   scale_color_gradient(high = spectral()) +
  #   coord_cartesian(xlim = c(30,145), ylim = c(-44,-2)) +
  #   scale_x_continuous(name = "distance (km)", breaks = seq(0,150,10)) +
  #   scale_y_continuous(name =  "depth (m)", limits = c(-50, 0), breaks = seq(-50,0,5)) + facet_grid(variable~.) +
  #   plot.theme

  g <- grid.arrange(t, s, den, ncol=1)

  #print image files to a directory
  png(file = paste0(getwd(),"/historical_plots/", unique(x$cruise), ".png"), width = 7, height = 9, units = "in", res = 300)
  plot(g)
  dev.off()

}, .progress="text", .inform = T)

#by storm for SALINITY ----
storm.data <- d[d$storm != "no.storm",]

storm.data$distanceFromStart <- dist.from.galv.bay(latitude1 = storm.data$sta_lat, longitude1 = storm.data$sta_lon)


sal.min <- round(min(storm.data$salinity, na.rm = T),0)
sal.max <- round(max(storm.data$salinity, na.rm = T),0)

  plot.theme.top <- theme(axis.text.y = element_text(size = 11, colour = "black"),
                      axis.text.x = element_blank(),
                      axis.title = element_text(size = 12),
                      strip.text = element_blank(),
                      strip.background = element_blank(),
                      panel.background = element_rect(colour="black", fill = "white"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.ticks.length=unit(.2, "cm"),
                      panel.border = element_rect(fill = NA, colour="black", size=1),
                      plot.margin=unit(c(0.5,0.5,0.1,0.2),"cm"))

  plot.theme.bottom <- theme(axis.text = element_text(size = 11, colour = "black"),
                          axis.title = element_text(size = 12),
                          strip.text = element_blank(),
                          strip.background = element_blank(),
                          panel.background = element_rect(colour="black", fill = "white"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.ticks.length=unit(.2, "cm"),
                          panel.border = element_rect(fill = NA, colour="black", size=1),
                          plot.margin=unit(c(0.5,0.5,0.1,0.2),"cm"))




  #SEAMAP = Harvey
  h <- storm.data[storm.data$cruise=="SEAMAP",]

  #stn.locale
  detach('package:plyr')
  stn.xy <- h %>% group_by(sta.seamap) %>% summarize(distanceFromStart.stn = mean(distanceFromStart))
  stn.xy$y_coord <- 1
  library(plyr)

  dm <- melt(h, id.vars=c("depth", "distanceFromStart"), measure.vars=vars)
  i<-dm
  x <- interp.dist(x=i$distanceFromStart, y=i$depth, z=i$value, duplicate="mean", x.step=1000, y.step=1, anisotropy=1000)
  names(x)[names(x)=="x"] <- "distance"
  names(x)[names(x)=="y"] <- "depth"

  harvey <- ggplot(x, aes(x=distance, y=-depth)) +
            geom_tile(aes(fill=value), na.rm=T) +
            stat_contour(aes(z=value), linetype = "dashed", colour="grey15",
                         alpha=0.7, bins=4, size=0.75, na.rm=TRUE) +
            #geom_text_contour(aes(z=value)) +
            scale_fill_gradient(high = alt.spectral,
                                limits = c(sal.min, sal.max),
                                breaks = seq(sal.min,sal.max,2),
                                labels = seq(sal.min,sal.max,2),
                                na.value=NA, name = "salinity (psu)") +
            scale_color_gradient(high = alt.spectral) +
            geom_point(data = stn.xy, aes(x = distanceFromStart.stn, y = -y_coord),
                       size=1.5, pch=6, colour="black") +
            coord_cartesian(xlim = c(35,145), ylim = c(-50,-2)) +
            scale_x_continuous(name = NULL, breaks = seq(0,150,10)) +
            scale_y_continuous(name =  "depth (m)", limits = c(-50, 0),
                               breaks = seq(-50,0,10), position = "right") +
           #guides(fill=F) +
           guides(fill = guide_colourbar(barwidth = 1.25, barheight = 20,
                                         label.theme = element_text(size = 12),
                                         title.theme = element_text(size = 12),
                                         ticks.colour = "black"),
                                         direction="horizontal") +
           plot.theme.top


     #print image files to a directory
     png(file = paste0(getwd(),"/historical_plots/Fig2_harvey.png"), width = 5, height = 2.5, units = "in", res = 300)
     plot(harvey)
     dev.off()

  #Rita
  r <- storm.data[storm.data$storm=="Rita",]

  dm <- melt(r, id.vars=c("depth", "distanceFromStart"), measure.vars=vars)
  i<-dm
  x <- interp.dist(x=i$distanceFromStart, y=i$depth, z=i$value, duplicate="mean", x.step=1000, y.step=1, anisotropy=1000)
  names(x)[names(x)=="x"] <- "distance"
  names(x)[names(x)=="y"] <- "depth"

  #stn.locale
  detach('package:plyr')
  stn.xy <- r %>% group_by(sta.seamap) %>% summarize(distanceFromStart.stn = mean(distanceFromStart))
  stn.xy$y_coord <- 1
  library(plyr)

  rita <- ggplot(x, aes(x=distance, y=-depth)) +
            geom_tile(aes(fill=value), na.rm=T) +
            stat_contour(aes(z=value), linetype = "dashed", colour="grey15",
                 alpha=0.7, bins=4, size=0.75, na.rm=TRUE) +
            #geom_text_contour(aes(z=value)) +
            scale_fill_gradient(high = alt.spectral,
                                limits = c(sal.min, sal.max),
                                breaks = seq(sal.min,sal.max,2),
                                labels = seq(sal.min,sal.max,2),
                                na.value=NA, name = "salinity (psu)") +
            scale_color_gradient(high = alt.spectral) +
            geom_point(data = stn.xy, aes(x = distanceFromStart.stn, y = -y_coord),
               size=1.5, pch=6,colour="black") +
            coord_cartesian(xlim = c(35,145), ylim = c(-50,-2)) +
            scale_x_continuous(name = NULL, breaks = seq(0,150,10)) +
            scale_y_continuous(name =  "depth (m)", limits = c(-50, 0),
                       breaks = seq(-50,0,10), position = "right") +
            guides(fill=F) +
            plot.theme.top


      #print image files to a directory
      png(file = paste0(getwd(),"/historical_plots/Fig2_rita.png"), width = 5, height = 2.5, units = "in", res = 300)
      plot(rita)
      dev.off()


  #Ike
  ike <- storm.data[storm.data$storm=="Ike",]

  dm <- melt(ike, id.vars=c("depth", "distanceFromStart"), measure.vars=vars)
  i<-dm
  x <- interp.dist(x=i$distanceFromStart, y=i$depth, z=i$value, duplicate="mean", x.step=1000, y.step=1, anisotropy=1000)
  names(x)[names(x)=="x"] <- "distance"
  names(x)[names(x)=="y"] <- "depth"


  #stn.locale
  detach('package:plyr')
  stn.xy <- ike %>% group_by(sta.seamap) %>% summarize(distanceFromStart.stn = mean(distanceFromStart))
  stn.xy$y_coord <- 1
  library(plyr)


  ike.plot <-ggplot(x, aes(x=distance, y=-depth)) +
          geom_tile(aes(fill=value), na.rm=T) +
          stat_contour(aes(z=value), linetype = "dashed", colour="grey15",
                 alpha=0.7, bins=4, size=0.75, na.rm=TRUE) +
         # geom_text_contour(aes(z=value)) +
          scale_fill_gradient(high = alt.spectral,
                              limits = c(sal.min, sal.max),
                              breaks = seq(sal.min,sal.max,2),
                              labels = seq(sal.min,sal.max,2),
                              na.value=NA, name = "salinity (psu)") +
          scale_color_gradient(high = alt.spectral) +
          geom_point(data = stn.xy, aes(x = distanceFromStart.stn, y = -y_coord),
               size=1.5, pch=6,colour="black") +
          coord_cartesian(xlim = c(35,145), ylim = c(-50,-2)) +
          scale_x_continuous(name = NULL, breaks = seq(0,150,10)) +
          scale_y_continuous(name =  "depth (m)", limits = c(-50, 0),
                       breaks = seq(-50,0,10), position = "right") +
          guides(fill=F) +
          #guides(fill = guide_colourbar(barwidth = 1.5, barheight = 10)) +
          plot.theme.top

  #print image files to a directory
  png(file = paste0(getwd(),"/historical_plots/Fig2_ike.png"), width = 5, height = 2.5, units = "in", res = 300)
  plot(ike.plot)
  dev.off()

   g <- grid.arrange(harvey, rita, ike.plot, ncol=1)

  #print image files to a directory
  # png(file = paste0(getwd(),"/historical_plots/Fig2_section.png"), width = 9, height = 7, units = "in", res = 300)
  # plot(g)
  # dev.off()