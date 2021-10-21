library(sf)
library(stringr)
library(RColorBrewer)
library(scales)
library(reldist)

load("generated data\\Barents Sea islands split into 200km coastline polygons 60km inland buffer more than 1sqkm.RData")
coast.id <- data.frame(coastp3)
coast.id <- coast.id[!is.na(coast.id$group),]


# load DEM den habitat and aggregate by polygon id 
load("generated data\\DEM habitat\\DEM den habitat data small polygons without inner buffer.RData")
# quantiles of model values for all known dens in Svalbard
#         5%        10%        25%        33%        50%        66%        75% 
# 0.02097290 0.03974938 0.13320500 0.22224025 0.42777476 0.56898328 0.62407766

agg = aggregate(dat4[,c(12:28)],
                by = list(dat4$new.id),
                FUN = function(x) sum(x,na.rm=T))
names(agg)[1] <- "new.id"
agg2 <- merge(agg, coast.id, by="new.id",all.x=T)
agg2$area <- agg2$b33


# proportion of island is habitat as b33
tapply(agg2$b33, agg2$group, sum)/tapply(agg2$all, agg2$group, sum)
sum(agg2$b33)/sum(agg2$all)

# proportion of island is habitat as b25
tapply(agg2$b25, agg2$group, sum)/tapply(agg2$all, agg2$group, sum)
sum(agg2$b25)/sum(agg2$all)

# proportion of island is habitat as b50
tapply(agg2$b50, agg2$group, sum)/tapply(agg2$all, agg2$group, sum)
sum(agg2$b50)/sum(agg2$all)


load("generated data/first date with ice around islands small polygons 79-20.RData")
hist.data <- merge(dat4,agg2[,c("id3","area","b33","b25","b50","group")],by="id3")
hist.data <- hist.data[!is.na(hist.data$group),]
hist.data$cut <- 1
hist.data$cut[hist.data$doy2 >= 93]  <- 2
hist.data$cut[hist.data$doy2 >= 123]  <- 3


models <- c("BCC-CSM2-MR","NorESM2-LM","EC-Earth3","CanESM5",
            "CNRM-ESM2-1","ACCESS-CM2","NorESM2-MM")   # "BCC-ESMI"


gcm.output.files <- list.files("generated data",pattern="first date with ice around islands small polygons")
gcm.output.files <- gcm.output.files[grepl("2099", gcm.output.files)]
gcm.output.files2 <- gsub('first date with ice around islands small polygons ',"",gcm.output.files)
gcm.output.files2 <- gsub(' 2015-2099.RData',"",gcm.output.files2)
gcm.output.files <- gcm.output.files[gcm.output.files2 %in% models]

for(gf in 1:length(gcm.output.files)){
  load(paste("generated data",gcm.output.files[gf],sep="/"))
  gcm.dat4$model <- gsub(" 2015-2099.RData","",gsub("first date with ice around islands small polygons ","", gcm.output.files[gf]))
  if(gf==1) gcm.data <- gcm.dat4 else gcm.data <-rbind(gcm.data,gcm.dat4)
}
gcm.data <- merge(gcm.data,agg2[,c("id3","area","group")],by="id3")


cols <- brewer.pal(12,"Paired")
cols <- data.frame(col  =c(cols[1], cols[2], grey(0.6), cols[5], cols[6]),
                   group=c("WSVB","ESVB","FJL","SNZ","NNZ"),
                   n=c(31,59,74,58,36))


png(paste0("figures/GCM/all island groupds weighted v3.png"),width=18,height=40,units="cm",res=700)
layout(matrix(c(1:10), ncol=1, byrow = F), heights = rep(c(1,3),5))
par(oma=c(2,0,1.3,1.2))
for(gr in c("WSVB","ESVB","FJL","NNZ","SNZ")) {
  
  d2 <- hist.data[hist.data$group==gr,]
  d2 <- d2[order(d2$year2),]
  
  d3 <- expand.grid(year2 = 1979:2019,
                    id3 = unique(d2$id3))
  d3 <- merge(d3,d2[,c("doy2","year2","id3")],all.x=T)
  d3$ice_free <- 0
  d3$ice_free[is.na(d3$doy2)]     <- 1
  d3$doy2[is.na(d3$doy2)]     <- 213
  d3$doy2[d3$doy2 > 213] <- 213
  d3$doy2[d3$doy2 > 153] <- 153
  d3 <- merge(d3, coast.id[,c("id3","area")],by="id3")
  d3.hist <- d3[order(d3$year2),]
  
  d3.hist.mean <- tapply(d3.hist$doy2,d3.hist$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.5,  weight = d3.hist$area))
  d3.hist.q25  <- tapply(d3.hist$doy2,d3.hist$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.25, weight = d3.hist$area))
  d3.hist.q75  <- tapply(d3.hist$doy2,d3.hist$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.75, weight = d3.hist$area))
  d3.hist.q05  <- tapply(d3.hist$doy2,d3.hist$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.05, weight = d3.hist$area))
  d3.hist.q95  <- tapply(d3.hist$doy2,d3.hist$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.95, weight = d3.hist$area))
  
  d2 <- gcm.data[gcm.data$group==gr,]
  d2 <- d2[order(d2$year2),]
  d2 <- d2[!is.na(d2$year2),]
  
  d3 <- expand.grid(year2 = 2020:2099,
                    id3 = unique(d2$id3),
                    model = models)
  d3 <- merge(d3,d2[,c("doy2","year2","id3","model")],all.x=T)
  d3$ice_free <- 0
  d3$ice_free[is.na(d3$doy2)]     <- 1
  d3$doy2[is.na(d3$doy2)]     <- 213
  d3$doy2[d3$doy2 > 213] <- 213
  d3$doy2[d3$doy2 > 153] <- 153
  d3 <- merge(d3, coast.id[,c("id3","area")],by="id3")
  d3.gcm <- d3[order(d3$year2),]
  
  d3.gcm.mean <- tapply(d3.gcm$doy2,d3.gcm$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.5,  weight = d3.gcm$area))
  d3.gcm.q25  <- tapply(d3.gcm$doy2,d3.gcm$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.25, weight = d3.gcm$area))
  d3.gcm.q75  <- tapply(d3.gcm$doy2,d3.gcm$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.75, weight = d3.gcm$area))
  d3.gcm.q05  <- tapply(d3.gcm$doy2,d3.gcm$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.05, weight = d3.gcm$area))
  d3.gcm.q95  <- tapply(d3.gcm$doy2,d3.gcm$year2,FUN=function(x) reldist::wtd.quantile(x, q= 0.95, weight = d3.gcm$area))
  
  agg.gcm = aggregate(d3.gcm$ice_free,
                      by = list(d3.gcm$model, d3.gcm$year2),
                      FUN = function(x) sum(x,na.rm=T))
  names(agg.gcm) <- c("model", "year2", "ice_free")
  
  
  # par(fig=c(0,1,0,1))
  par(mar=c(0,4,0.25,0))
  # par(mar=c(0,4,2,1),fig=c(0,1,0.8,1))
  plot(unique(hist.data$year2),tapply(hist.data$doy2,hist.data$year2,mean),ylim=c(0,1),xlim=c(1979,2099),
       col="white",yaxt="n",ylab="",xaxt="n",xaxs = 'i')
  abline(v=2019.5, lwd=2, lty=2, col=grey(0.8))
  lines(unique(d3.hist$year2), tapply(d3.hist$ice_free, d3.hist$year2, sum)/length(unique(d3.hist$id3)), col=cols$col[cols$group==gr],lwd=2)
  
  polygon(c(unique(d3.gcm$year2),rev(unique(d3.gcm$year2))), 
          c(tapply(agg.gcm$ice_free, agg.gcm$year2, min), rev(tapply(agg.gcm$ice_free, agg.gcm$year2, max)))/length(unique(d3.gcm$id3)), 
          col=alpha(cols$col[cols$group==gr], alpha=0.15),border="transparent")
  lines(unique(d3.gcm$year2), tapply(agg.gcm$ice_free, agg.gcm$year2, mean)/length(unique(d3.gcm$id3)), col=cols$col[cols$group==gr],lty=3,lwd=2)
  
  axis(2, at=c(0,1), las=1)
  mtext("Proportion",2,line=3.1,cex=0.7)
  mtext("ice-free",  2,line=2.3,cex=0.7)
  mtext("year-round",2,line=1.5,cex=0.7)
  if(gr=="WSVB"){
    axis(3, at=c(2019.5), labels=c(""),tick=T,col=grey(0.8),lwd=2)
    axis(3, at=c(2000, 2060), labels=c("observed", "predicted"),tick=F,line=-0.7)
  }
  
  par(mar=c(0.25,4,0,0))
  # par(mar=c(2,4,0,1),fig=c(0,1,0,0.8),new=T)
  plot(unique(hist.data$year2),tapply(hist.data$doy2,hist.data$year2,mean),ylim=c(1,153),xlim=c(1979,2099),
       col="white",yaxt="n",ylab="",xaxt="n",xaxs = 'i')
  abline(h=c(92,123), lwd=1, col=grey(0.5))
  abline(v=2019.5, lwd=2, lty=2, col=grey(0.8))
  if(gr=="SNZ") axis(1)
  axis(2,at=c(244,274,305,335,365+1,365+32,365+60,365+91)-243, labels=c("1 Sep","1 Oct","1 Nov","1 Dec","1 Jan","1 Feb","1 Mar","1 Apr"),las=2)
  axis(4,at=c(335,365+1)-243, labels=c("in time","too late"),las=3,tick=F,line = -0.8, cex.axis=0.9)
  
  years <- unique(d3.hist$year2)
  polygon(c(years,rev(years)), c(d3.hist.q05, rev(d3.hist.q95)), col=alpha(cols$col[cols$group==gr], alpha=0.15),border="transparent")
  polygon(c(years,rev(years)), c(d3.hist.q25, rev(d3.hist.q75)), col=alpha(cols$col[cols$group==gr], alpha=0.25),border="transparent")
  lines(years, d3.hist.mean, col=cols$col[cols$group==gr], lwd=2)
  
  years <- unique(d3.gcm$year2)
  polygon(c(years,rev(years)), c(d3.gcm.q05, rev(d3.gcm.q95)), col=alpha(cols$col[cols$group==gr], alpha=0.15),border="transparent")
  polygon(c(years,rev(years)), c(d3.gcm.q25, rev(d3.gcm.q75)), col=alpha(cols$col[cols$group==gr], alpha=0.25),border="transparent")
  lines(years, d3.gcm.mean, col=cols$col[cols$group==gr], lwd=2,lty=3)
  
  if(gr!="FJL") y=250-243 else y=380-243
  
  text(1990, y, labels = gr, col=cols$col[cols$group==gr], cex = 2) 
  # par(fig=c(0.2,0.38,0,0.5),mar=c(0,0,0,0),oma=c(0,0,0,0),new=T,bg="transparent")
  # 
  # plot(st_geometry(st_as_sf(coastp30)),col=grey(0.8),border=grey(0.8),lwd=0.8,add=T)
  # plot(coastp30[coastp30$group==gr,],col=grey(0),border=grey(0),lwd=0.8,add=T)
  # 
  # par(opar)
  # 
}
dev.off()


