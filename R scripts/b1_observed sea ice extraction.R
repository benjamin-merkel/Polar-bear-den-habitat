library(raster)
library(rgdal)
library(rgeos)


r.mask <- raster(nrow=448, ncol=304, xmn=-3850000, xmx=3750000, ymn=-5350000, ymx=5850000)
nsidc <- CRS(' +init=epsg:3411')
projection(r.mask) <- nsidc
directory <- "E:\\environmental data\\sea ice\\sea ice 25km 79-18"

load("generated data\\Barents Sea islands split into 200km coastline polygons 60km inland buffer more than 1sqkm.RData")
coast.id <- subset(data.frame(coastp3),select=c(new.id))

# calc 50km buffer around all islands
load("generated data\\coast complex buffer.RData")


# Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS Passive Microwave Data V001
# extract mean ice conc in 50 km buffer around all islands in Barents sea
for(iyear in 1979:2020){
  files <- list.files(paste(directory,iyear,sep="\\"))
  
  for(c in 1:length(files)){
    date <- as.Date(ISOdate(substr(files[c],4,7),substr(files[c],8,9),substr(files[c],10,11)))
    cat('\r',as.character(date))
    
    year <- strftime(date,"%Y")
    month<- strftime(date,"%m")
    day  <- strftime(date,"%d")
    
    filename  <- files[c]
    con <- file(paste(directory,year,filename,sep="\\"), 'rb')
    x <- readBin(con,"int", size=1, signed=FALSE, 150000)
    close(con)
    
    r <- setValues(r.mask, x[301:length(x)])
    r[r==251]<-250 # circular data hole around north pole, assumed to be 100%
    r[r==253]<-NA # coastline
    r[r==254]<-NA # land mask
    r[r==255]<-NA # missing data
    r <- r/250
    
    r <- crop(r,buff)
    rb <- extract(r,buff)
    rb <- lapply(rb,function(x) mean(x,na.rm=T))
    dat1 <- data.frame(ice=unlist(rb),
                       date=date,
                       id2 = buff$id2,
                       buffer.id = buff$buffer.id)
  
    if(c == 1) dat2 <- dat1 else dat2 <- rbind(dat2,dat1)
  }
  if(iyear == 1979) dat3 <- dat2 else dat3 <- rbind(dat3,dat2)
}
save(dat3,file="generated data\\daily ice around complex coast polygons rerun.RData")
load("generated data\\daily ice around complex coast polygons rerun.RData")


dat3$doy <- as.numeric(strftime(dat3$date,"%j"))
dat3$month <- as.numeric(strftime(dat3$date,"%m"))
dat3$year <- as.numeric(strftime(dat3$date,"%Y"))
dat3$year2 <- dat3$year
dat3$year2[dat3$month<9] <- dat3$year2[dat3$month<9]-1
dat3$doy2 <- dat3$doy-243
dat3$doy2[dat3$month<9] <- dat3$doy2[dat3$month<9]+365
dat3 <- dat3[order(dat3$doy2),]
dat3 <- dat3[order(dat3$year2),]
dat3 <- dat3[dat3$year2!=1978,] #remove 1978 as it does not contain a full year

# look at first date with at least 60% sea ice
dat3$ice.cut <- 0
dat3$ice.cut[dat3$ice>=0.6] <- 1
dat4 <- dat3[dat3$ice.cut==1,]
dat4 <- dat4[!duplicated(paste(dat4$id2,dat4$buffer.id,dat4$year2)),]
dat4 <- dat4[order(dat4$id2,dat4$buffer.id),]
dat4$yc <- "darkolivegreen"
dat4$yc[dat4$doy2>=93]  <- "gold"
dat4$yc[dat4$doy2>=123] <- "red"
dat4$id3 <- paste(dat4$id2,dat4$buffer.id,sep="-")

save(dat4,file="generated data\\first date with ice around islands small polygons 79-20.RData")