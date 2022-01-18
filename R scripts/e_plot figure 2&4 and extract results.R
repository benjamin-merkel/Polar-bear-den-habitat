library(sp)
library(sf)
library(Hmisc)
library(RColorBrewer)
library(graticule)
library(raster)
library(spex)

load("generated data\\Barents Sea islands split into 200km coastline polygons 60km inland buffer more than 1sqkm.RData")
coast.id <- data.frame(coastp3)


load("generated data\\DEM habitat\\DEM den habitat data small polygons without inner buffer.RData")
agg = aggregate(dat4[,c(12:28)],
                by = list(dat4$new.id),
                FUN = function(x) sum(x,na.rm=T))
names(agg)[1] <- "new.id"
agg2 <- merge(agg, coast.id, by="new.id",all.x=T)


load("generated data/first date with ice around islands small polygons 79-20.RData")
hist.data    <- merge(dat4,agg2,by="id3",all.x=T)
hist.data$id <- hist.data$new.id
hist.data    <- hist.data[!is.na(hist.data$group),]



d2 <- hist.data#[hist.data$group==gr,]
d2 <- d2[order(d2$year2),]

d3 <- expand.grid(year2 = unique(d2$year2),
                  id3   = unique(d2$id3))
d3 <- merge(d3,d2[,c("doy2","year2","id3","group")],all.x=T)
d3$doy2[is.na(d3$doy2)]     <- 213
d3$doy2[d3$doy2 > 213] <- 213
d3$doy2[d3$doy2 > 153] <- 153
d3 <- merge(d3, agg2[,c("id3","b33","b25",'b50')],by="id3")
d3 <- d3[order(d3$year2),]

hist.data <- d3


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
gcm.data <- merge(gcm.data,coast.id[,c("id3","area","group")],by="id3")


d2 <- gcm.data#[hist.data$group==gr,]
d2 <- d2[order(d2$year2),]

d3 <- expand.grid(year2 = unique(d2$year2),
                  id3   = unique(d2$id3),
                  model = models)
d3 <- merge(d3,d2[,c("doy2","year2","id3","group","model")],all=T)
d3$doy2[is.na(d3$doy2)]     <- 213
d3$doy2[d3$doy2 > 213] <- 213
d3$doy2[d3$doy2 > 153] <- 153
d3 <- merge(d3, agg2[,c("id3","b33","b25",'b50')],by="id3")
d3 <- d3[order(d3$year2),]

gcm.data <- d3


ice_years <- 1979:1988
dat6 <- hist.data[hist.data$year2 %in% ice_years,]
d80s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean))
d80s$id <- (rownames(d80s))

ice_years <- 1989:1998
dat6 <- hist.data[hist.data$year2 %in% ice_years,]
d90s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d90s$id <- (rownames(d90s))

ice_years <- 1999:2008
dat6 <- hist.data[hist.data$year2 %in% ice_years,]
d00s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d00s$id <- (rownames(d00s))

ice_years <- 2009:2018
dat6 <- hist.data[hist.data$year2 %in% ice_years,]
d10s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d10s$id <- (rownames(d10s))

ice_years <- 2020:2029
dat6 <- gcm.data[gcm.data$year2 %in% ice_years,]
d2020s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d2020s$id <- (rownames(d2020s))

ice_years <- 2030:2039
dat6 <- gcm.data[gcm.data$year2 %in% ice_years,]
d2030s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d2030s$id <- (rownames(d2030s))

ice_years <- 2040:2049
dat6 <- gcm.data[gcm.data$year2 %in% ice_years,]
d2040s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d2040s$id <- (rownames(d2040s))

ice_years <- 2050:2059
dat6 <- gcm.data[gcm.data$year2 %in% ice_years,]
d2050s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d2050s$id <- (rownames(d2050s))

ice_years <- 2060:2069
dat6 <- gcm.data[gcm.data$year2 %in% ice_years,]
d2060s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d2060s$id <- (rownames(d2060s))

ice_years <- 2070:2079
dat6 <- gcm.data[gcm.data$year2 %in% ice_years,]
d2070s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d2070s$id <- (rownames(d2070s))

ice_years <- 2080:2089
dat6 <- gcm.data[gcm.data$year2 %in% ice_years,]
d2080s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d2080s$id <- (rownames(d2080s))

ice_years <- 2090:2099
dat6 <- gcm.data[gcm.data$year2 %in% ice_years,]
d2090s  <- data.frame(tapply(dat6$doy2,dat6$id3,mean,na.rm=T))
d2090s$id <- (rownames(d2090s))

dd <- merge(d80s,d90s,by="id",all=T)
dd <- merge(dd,d00s,by="id",all=T)
dd <- merge(dd,d10s,by="id",all=T)
dd <- merge(dd,d2020s,by="id",all=T)
dd <- merge(dd,d2030s,by="id",all=T)
dd <- merge(dd,d2040s,by="id",all=T)
dd <- merge(dd,d2050s,by="id",all=T)
dd <- merge(dd,d2060s,by="id",all=T)
dd <- merge(dd,d2070s,by="id",all=T)
dd <- merge(dd,d2080s,by="id",all=T)
dd <- merge(dd,d2090s,by="id",all=T)
colnames(dd) <- cbind("id","s80","s90","s00","s10",
                      's2020','s2030','s2040','s2050','s2060',
                      's2070','s2080','s2090')
dd$diff <- dd$s10-dd$s80
dd <- merge(dd,agg2,by.x="id",by.y="id3")
dd$group <- factor(dd$group,c("WSVB","ESVB","FJL","NNZ","SNZ"))


dd$cut80 <- 1
dd$cut80[dd$s80>=93]  <- 2
dd$cut80[dd$s80>=123] <- 3
dd$cut90 <- 1
dd$cut90[dd$s90>=93]  <- 2
dd$cut90[dd$s90>=123] <- 3
dd$cut00 <- 1
dd$cut00[dd$s00>=93]  <- 2
dd$cut00[dd$s00>=123] <- 3
dd$cut00[is.na(dd$s00)] <- 3
dd$cut10 <- 1
dd$cut10[dd$s10>=93]  <- 2
dd$cut10[dd$s10>=123] <- 3
dd$cut10[is.na(dd$s10)] <- 3
dd$cut2020 <- 1
dd$cut2020[dd$s2020>=93]  <- 2
dd$cut2020[dd$s2020>=123] <- 3
dd$cut2020[is.na(dd$s2020)] <- 3
dd$cut2030 <- 1
dd$cut2030[dd$s2030>=93]  <- 2
dd$cut2030[dd$s2030>=123] <- 3
dd$cut2030[is.na(dd$s2030)] <- 3
dd$cut2040 <- 1
dd$cut2040[dd$s2040>=93]  <- 2
dd$cut2040[dd$s2040>=123] <- 3
dd$cut2040[is.na(dd$s2040)] <- 3
dd$cut2050 <- 1
dd$cut2050[dd$s2050>=93]  <- 2
dd$cut2050[dd$s2050>=123] <- 3
dd$cut2050[is.na(dd$s2050)] <- 3
dd$cut2060 <- 1
dd$cut2060[dd$s2060>=93]  <- 2
dd$cut2060[dd$s2060>=123] <- 3
dd$cut2060[is.na(dd$s2060)] <- 3
dd$cut2070 <- 1
dd$cut2070[dd$s2070>=93]  <- 2
dd$cut2070[dd$s2070>=123] <- 3
dd$cut2070[is.na(dd$s2070)] <- 3
dd$cut2080 <- 1
dd$cut2080[dd$s2080>=93]  <- 2
dd$cut2080[dd$s2080>=123] <- 3
dd$cut2080[is.na(dd$s2080)] <- 3
dd$cut2090 <- 1
dd$cut2090[dd$s2090>=93]  <- 2
dd$cut2090[dd$s2090>=123] <- 3
dd$cut2090[is.na(dd$s2090)] <- 3



# extract weighted mean dates and change in them
dates      <- as.Date(0:400,"2016-07-01")
days       <- data.frame(dates)
days$doy   <- as.numeric(strftime(days$dates,"%j"))
days$month <- as.numeric(strftime(days$dates,"%m"))
days$doy2  <- days$doy-243
days$doy2[days$month<9] <- days$doy2[days$month<9]+365

groups = (c("WSVB","ESVB","FJL","NNZ","SNZ"))
for(gg in groups){
  dd0 <- dd[dd$group %in% gg,]
  dd0$s00[is.na(dd0$s00)] <- 290
  dd0$s10[is.na(dd0$s10)] <- 290
  dd0$s2020[is.na(dd0$s2020)] <- 290
  dd0$s2030[is.na(dd0$s2030)] <- 290
  dd0$s2040[is.na(dd0$s2040)] <- 290
  dd0$s2050[is.na(dd0$s2050)] <- 290
  dd0$s2060[is.na(dd0$s2060)] <- 290
  dd0$s2070[is.na(dd0$s2070)] <- 290
  dd0$s2080[is.na(dd0$s2080)] <- 290
  dd0$s2090[is.na(dd0$s2090)] <- 290
  ddx <- data.frame(day = c(wtd.mean(dd0$s80, dd0$h20),
                    wtd.mean(dd0$s90, dd0$h20),
                    wtd.mean(dd0$s00, dd0$h20),
                    wtd.mean(dd0$s10, dd0$h20),
                    wtd.mean(dd0$s2020, dd0$h20),
                    wtd.mean(dd0$s2030, dd0$h20),
                    wtd.mean(dd0$s2040, dd0$h20),
                    wtd.mean(dd0$s2050, dd0$h20),
                    wtd.mean(dd0$s2060, dd0$h20),
                    wtd.mean(dd0$s2070, dd0$h20),
                    wtd.mean(dd0$s2080, dd0$h20),
                    wtd.mean(dd0$s2090, dd0$h20)),
                    decade= c(1980,1990,2000,2010,2020,2030,
                              2040,2050,2060,2070,2080,2090),
                    group= gg)
  if(gg == groups[1]) ddx2 <- ddx else ddx2 <- rbind(ddx2,ddx)
}


## time when ice arrives by group 1980
for(gg in unique(ddx2$group)) print(paste(gg, "--",days$dates[days$doy2 %in% round(ddx2$day[ddx2$decade==1980 & ddx2$group==gg], 0)]))
## time when ice arrives by group 2010
for(gg in unique(ddx2$group)) print(paste(gg, "--",days$dates[days$doy2 %in% round(ddx2$day[ddx2$decade==2010 & ddx2$group==gg], 0)]))
## time when ice arrives by group 2090
for(gg in unique(ddx2$group)) print(paste(gg, "--",days$dates[days$doy2 %in% round(ddx2$day[ddx2$decade==2090 & ddx2$group==gg], 0)]))

## shift in days between decades
ddx2$day[ddx2$decade==2010] - ddx2$day[ddx2$decade==1980] 
ddx2$day[ddx2$decade==2090] - ddx2$day[ddx2$decade==1980] 
ddx2$day[ddx2$decade==2090] - ddx2$day[ddx2$decade==2010] 




###################################################################################
## figures


png("figures/decadel change in denning habitat + gcm models V3.png",width=15,height=20,units="cm",res=700)

mat <- matrix(0,nrow=3,ncol=12)
t <- tapply(dd$b33,dd$cut80,sum)
for(yy in as.numeric(names(t))) mat[yy,1] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut90,sum)
for(yy in as.numeric(names(t))) mat[yy,2] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut00,sum)
for(yy in as.numeric(names(t))) mat[yy,3] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut10,sum)
for(yy in as.numeric(names(t))) mat[yy,4] <- t[names(t)==as.character(yy)]

t <- tapply(dd$b33,dd$cut2020,sum)
for(yy in as.numeric(names(t))) mat[yy,5] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut2030,sum)
for(yy in as.numeric(names(t))) mat[yy,6] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut2040,sum)
for(yy in as.numeric(names(t))) mat[yy,7] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut2050,sum)
for(yy in as.numeric(names(t))) mat[yy,8] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut2060,sum)
for(yy in as.numeric(names(t))) mat[yy,9] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut2070,sum)
for(yy in as.numeric(names(t))) mat[yy,10] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut2080,sum)
for(yy in as.numeric(names(t))) mat[yy,11] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut2090,sum)
for(yy in as.numeric(names(t))) mat[yy,12] <- t[names(t)==as.character(yy)]

bp <- barplot(mat/1000,col=c("darkolivegreen","gold",2),ylab="denning habitat [1000 sqkm]",
              border=c("darkolivegreen","gold",2),las=1, density = c(rep(-1,12),rep(20,6)),ylim=c(0,20))
axis(1, at=bp,labels=c("1980's","1990's","2000's","2010's","2020's",
                       "2030's","2040's","2050's","2060's","2070's",
                       "2080's","2090's"),tick=F)

dev.off()











jpeg("figures/figure 2.jpeg",width=15,height=20,units="cm",res=700)

mat <- matrix(0,nrow=3,ncol=7)
t <- tapply(dd$b33,dd$cut80,sum)
for(yy in as.numeric(names(t))) mat[yy,1] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut90,sum)
for(yy in as.numeric(names(t))) mat[yy,2] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut00,sum)
for(yy in as.numeric(names(t))) mat[yy,3] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut10,sum)
for(yy in as.numeric(names(t))) mat[yy,4] <- t[names(t)==as.character(yy)]

t <- tapply(dd$b33,dd$cut2050,sum)
for(yy in as.numeric(names(t))) mat[yy,6] <- t[names(t)==as.character(yy)]
t <- tapply(dd$b33,dd$cut2090,sum)
for(yy in as.numeric(names(t))) mat[yy,7] <- t[names(t)==as.character(yy)]

par(mar=c(2,5,0,0))
bp <- barplot(mat/1000,col=c("darkolivegreen","gold",2),ylab=expression(denning~habitat~"[ 1000"~km^2~"]"),
              border="white",las=1, density = c(rep(-1,12),rep(20,6)),yaxt="n",ylim=c(0,21),lwd=0.01)
axis(2, at=seq(0,20,3),las=1)
axis(1, at=bp,labels=c("1980's","1990's","2000's","2010's","","2050's","2090's"),tick=F)
lines(c(0.2,4.8),c(19.5,19.5))
lines(c(6.2,8.4),c(19.5,19.5))
text(2.5,20,"observed")
text(7.3,20,"predicted")

dev.off()





proj.laea30 <- CRS("+proj=laea +lat_0=90 +lon_0=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
coastp30 <- spTransform(coastp3,proj.laea30)

lons <- seq(10, 70, 10)
lats <- seq(74, 82, 2)
pex <- spex(cp)
g <- st_graticule(st_as_sf(pex), lat = lats, lon = lons)

cp <- coastp30[!is.na(coastp30$group),]
cp <- cp[cp$id3 %in% dd$id,]
cp <- cp[order(cp$new.id),]
ddd <- dd[order(dd$new.id),]
ddd <- ddd[ddd$new.id %in% cp$new.id,]
ddd <- ddd[order(ddd$new.id),]

jpeg("figures/figure 4.jpeg",width=15,height=23,units="cm",res=700)
par(mfrow=c(3,2),oma=c(0,0,2,0))
par(mar=c(0.1,1,2,0.1))
par(xpd = NA)
for(i in 1:6){
  if(i==1) {
    cp$date <- ddd$cut80
    title <- "1980"
  }
  if(i==2) {
    cp$date <- ddd$cut90
    title <- "1990"
  }
  if(i==3) {
    cp$date <- ddd$cut00
    title <- "2000"
  }
  if(i==4) {
    cp$date <- ddd$cut10
    title <- "2010"
  }
  if(i==5) {
    cp$date <- ddd$cut2050
    title <- "2050"
  }
  if(i==6) {
    cp$date <- ddd$cut2090
    title <- "2090"
  }
  
  cp2 <- cp[cp$area > 1e+06,]
  if(i == 3) par(mar=c(2,  1,0.3,0.1))
  if(i == 5) par(mar=c(0.1,1,2,  0.1))
  plot(cp2, lwd=0.5, border=grey(0.4))
  # plot(pex,add=T)
  plot(st_geometry(g), add=T, lwd=0.5, col=grey(0.6))
  invisible(lapply(seq_len(nrow(g)), function(i) {
    if (g$type[i] == "N" && g$x_start[i] - min(g$x_start) < 1000)
      text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
           pos = 2, cex = .7, col = grey(0.6))
    if (g$type[i] == "E" && g$y_end[i] - max(g$y_end) > -1000)
      text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
           pos = 3, cex = .7, col = grey(0.6))
  }))
  plot(cp2, col=c("darkolivegreen","gold",2,2)[cp2$date], lwd=0.1,
       border=c("darkolivegreen","gold",2,1)[cp2$date],add=T)
  # box()
  mtext(title, side = 1 , line = -7, cex = 1.5)
}
mtext("observed", side = 3, line = -1.5,  outer =T,cex=1.6)
mtext("predicted",  side = 3, line = -46, outer =T,cex=1.6)
legend("bottomleft", legend=c("in time (\u22641 Dec)","potentially in time (\u22641 Jan)","not in time (>1 Jan)"),pch=22,
       col=c("darkolivegreen","gold",2),pt.bg=c("darkolivegreen","gold",2),bg="white",box.col="transparent")
dev.off()



tapply(dd$b33, dd$group, sum)/tapply(dd$all, dd$group, sum)
sum(dd$b33)/sum(dd$all)

tapply(dd$b25, dd$group, sum)/tapply(dd$all, dd$group, sum)
sum(dd$b25)/sum(dd$all)

tapply(dd$b50, dd$group, sum)/tapply(dd$all, dd$group, sum)
sum(dd$b50)/sum(dd$all)


x1 <- dd[dd$cut80==1,]
tapply(x1$b33, x1$group, sum)/tapply(dd$b33, dd$group, sum)
sum(x1$b33)/sum(dd$b33)

x2 <- dd[dd$cut10==1,]
tapply(x2$b33, x2$group, sum)/tapply(dd$b33, dd$group, sum)
sum(x2$b33)/sum(dd$b33)

x3 <- dd[dd$cut2090==1,]
tapply(x3$b33, x3$group, sum)/tapply(dd$b33, dd$group, sum)
sum(x3$b33)/sum(dd$b33)


1 - sum(x2$b33)/sum(x1$b33)
1 - sum(x3$b33)/sum(x1$b33)

