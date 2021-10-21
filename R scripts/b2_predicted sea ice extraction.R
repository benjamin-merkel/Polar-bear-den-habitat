library(raster)
library(ncdf4)
library(sf)
library(stringr)

load("generated data\\Barents Sea islands split into 200km coastline polygons 22km inland buffer more than 1sqkm.RData")
coast.id <- data.frame(coastp3)
coast.id <- coast.id[!is.na(coast.id$group),]

# models with best historical freeze up dynamics according to Smith 2020
models <- c("BCC-CSM2-MR","NorESM2-LM","EC-Earth3","CanESM5",
            "CNRM-ESM2-1","ACCESS-CM2","NorESM2-MM")   # "BCC-ESMI"

mmm=5

for(mmm in 1:7){
  model <- models[mmm]
  
  files <- list.files("E:/environmental data/GCM",pattern = ".nc",full.names = T)
  files <- files[grepl(model,files)]
  date.start <- str_split_fixed(str_split_fixed(files, "/",4)[,4], "_",7)[,7]
  date.end   <- strptime(substr(date.start,10,18), "%Y%m%d")
  date.start <- strptime(substr(date.start,1,8), "%Y%m%d")
  
  
  # load and extract from GCM
  for(file.select in 1:length(date.start)){
    
    nc.file <- files[file.select]
    nc <- nc_open(nc.file)
    var_names <- names(nc$var)
    
    lon_name <- var_names[!grepl("nd",var_names) & grepl("on",var_names) & !grepl('siconc',var_names)]
    lat_name <- var_names[!grepl("nd",var_names) & grepl("at",var_names) & !grepl('siconc',var_names)]
    
    time.steps <- as.numeric(difftime(date.end[file.select], date.start[file.select], units ="days"))
    
    if(file.select==1){
      lon <- ncvar_get(nc,varid = lon_name)
      lat <- ncvar_get(nc,varid = lat_name)
    }
    
    sic <- ncvar_get(nc,varid = "siconc", start = c(1, 1, 1), count = c(dim(lon)[1], dim(lon)[2], 1))
    time_bnds <- ncvar_get(nc,varid = var_names[grepl("time_b",var_names)])
    
    date.start2 <- as.Date((1:dim(time_bnds)[2]) - 1, origin = date.start[file.select])
    
    
    if(file.select==1){
      sic.coord <- data.frame(lon=c(lon), lat=c(lat), values=c(sic),id=1:length(c(lon)))
      sic.coord$lon <- ifelse(sic.coord$lon > 180, -360 + sic.coord$lon, sic.coord$lon)
      sic.coord <- sic.coord[!is.na(sic.coord$values),]
      sic.coord <- st_as_sf(sic.coord,coords = c("lon","lat"),crs=4326)
      bs <- st_nearest_feature(buff_cen, sic.coord)
    }
    
    for(yy in 1:dim(time_bnds)[2]){
      cat("\r","file: ",file.select," of ",length(files) ,
          "  time step:",yy," of ", dim(time_bnds)[2], "  ")
      sic <- ncvar_get(nc,varid = "siconc", start = c(1, 1, yy), count = c(dim(lon)[1], dim(lon)[2], 1))
      gcm.dat <- data.frame(iceconc=c(sic[!is.na(sic)][bs]),
                            id3=    paste(buff_cen$id2, buff_cen$buffer.id, sep="-"),
                            date=   date.start2[yy])
      
      if(yy==1) gcm.dat2 <- gcm.dat else gcm.dat2 <- rbind(gcm.dat2, gcm.dat)
    }
    if(file.select==1) gcm.dat3 <- gcm.dat2 else gcm.dat3 <- rbind(gcm.dat3,gcm.dat2)
  }
  
  nc_close(nc)
  save(gcm.dat3,file=paste("generated data\\daily ice around complex coast polygons",model,"2015-2100.RData"))
}


for(mmm in 1:length(models)){
  cat("\r", mmm, " of ", length(models))
  model <- models[mmm]
  
  load(paste("generated data\\daily ice around complex coast polygons",model,"2015-2100.RData"))
  
  gcm.dat3$doy <- as.numeric(strftime(gcm.dat3$date,"%j"))
  gcm.dat3$month <- as.numeric(strftime(gcm.dat3$date,"%m"))
  gcm.dat3$year <- as.numeric(strftime(gcm.dat3$date,"%Y"))
  gcm.dat3$year2 <- gcm.dat3$year
  gcm.dat3$year2[gcm.dat3$month<9] <- gcm.dat3$year2[gcm.dat3$month<9]-1
  gcm.dat3$doy2 <- gcm.dat3$doy-243
  gcm.dat3$doy2[gcm.dat3$month<9] <- gcm.dat3$doy2[gcm.dat3$month<9]+365
  
  # look at first date with at least 60% sea ice
  gcm.dat3$ice.cut <- 0
  gcm.dat3$ice.cut[gcm.dat3$iceconc>= 60] <- 1
  gcm.dat4 <- gcm.dat3[gcm.dat3$ice.cut==1,]
  gcm.dat4 <- gcm.dat4[!duplicated(paste(gcm.dat4$id3,gcm.dat4$year2)),]
  gcm.dat4 <- gcm.dat4[order(gcm.dat4$id3),]
  save(gcm.dat4,file=paste("generated data\\first date with ice around islands small polygons",model,"2015-2099.RData"))
}
