library(rgdal)
library(raster)
library(stringr)
library(rgeos)
library(sf)
library(scales)
library(terra)

load("generated data\\Barents Sea islands split into 200km coastline polygons 60km inland buffer more than 1sqkm.RData")

# 10 meter resolution Arctic DEM
adem.grid <- readOGR('D:\\map data\\Arctic DEM','ArcticDEM_Tile_Index_Rel7')

tile.svb <- c('36_54','39_51','37_52','36_53','34_51','35_52','35_51','35_51','33_52','36_51','36_51','37_51','34_51','34_53','36_52','37_50','35_52','36_50',
              '34_50','33_56','36_52','38_51','37_50','35_54','34_53','33_53','39_51','36_50','35_50','33_51','34_52','35_53','34_51','36_53','37_51','33_51',
              '34_52','35_52','38_51','38_51','33_53','35_50','37_51','35_51','37_52','34_51','34_53','36_50','38_51','34_52','35_50','37_52','37_52','35_51',
              '36_50','35_53','34_52','37_50','35_50','33_56','35_53','36_51','34_50','35_53','37_51','36_51','35_52','34_50','36_52','34_53')
tile.fjl <- c('39_51','44_50','41_51','41_50','42_51','42_50','40_51','42_49','42_50','42_50','44_50','44_49','43_51','43_50','43_51','41_51','42_51','42_51',
              '43_50','42_51','43_50','44_50','43_51','44_50','43_49','43_49','43_51','41_51','43_50','42_50','41_50','41_51','43_49','41_50')
tile.nz  <- c('43_61','43_59','46_55','43_59','44_55','46_55','44_59','44_56','44_58','44_55','45_61','44_61','46_55','44_61','45_56','46_54','44_59',
              '46_53','43_61','44_61','44_57','43_60','44_57','43_60','45_55','44_58','46_55','43_60','45_56','45_55','45_61',
              '44_59','44_57','43_60','46_53','45_61','44_60','44_61','45_56','45_55','45_57','45_54','44_56','44_56','43_61','43_58','44_60','44_58','44_57',
              '44_60','43_57','43_58','46_54','43_59','44_56','43_59','46_54','46_54','43_57','45_56','45_55','45_57','44_59','44_60','44_58','45_61')

adem.svb <- adem.grid[adem.grid$tile %in% tile.svb,]
adem.fjl <- adem.grid[adem.grid$tile %in% tile.fjl,]
adem.nz  <- adem.grid[adem.grid$tile %in% tile.nz,]

tiles <- unique(c(as.character(tile.svb), as.character(tile.fjl), as.character(tile.nz)))


for(i in 1:length(tiles)){
  tile <- tiles[i]
  cat('\r',as.character(tile),"    ")
  r1 <- rast(paste("D:/map data/Arctic DEM/10m/",tile,"/",tile,"_10m_v3.0_reg_dem.tif",sep=""))
  coast <- st_transform(st_as_sf(coastp3),crs(r1))
  coast.tile <- st_crop(coast,raster::raster(r1))
  if(nrow(coast.tile)>0){
    r1 <- crop(r1,vect(coast.tile))
    r1 <- mask(r1,vect(coast.tile))
    s1 <- terrain(r1,v='slope',unit='degrees')
    
    m1 <- -4.906+s1*3.465e-01+(s1^2)*-7.343e-03+r1*2.328e-02+(r1^2)*-6.959e-05
    m2 <- 1/(1+exp(-(m1)))
    
    terra::writeRaster(m2, paste("generated data\\DEM habitat\\data\\DEM den habitat ",tile,".tif",sep=""),overwrite=T)
    
    for(ii in unique(coast.tile$new.id)){
      m4   <- crop(m2,coast.tile[coast.tile$new.id==ii,])
      m4   <- mask(m4,vect(coast.tile[coast.tile$new.id==ii,]))
      val  <- values(m4)
      all  <- length(val[!is.na(val)]) * 100 / 1000000
      h10  <- length(val[val>=0.1 & !is.na(val)]) * 100 / 1000000
      h20  <- length(val[val>=0.2 & !is.na(val)]) * 100 / 1000000
      h30  <- length(val[val>=0.3 & !is.na(val)]) * 100 / 1000000
      h40  <- length(val[val>=0.4 & !is.na(val)]) * 100 / 1000000
      h50  <- length(val[val>=0.5 & !is.na(val)]) * 100 / 1000000
      h60  <- length(val[val>=0.6 & !is.na(val)]) * 100 / 1000000
      h70  <- length(val[val>=0.7 & !is.na(val)]) * 100 / 1000000
      h80  <- length(val[val>=0.8 & !is.na(val)]) * 100 / 1000000
      h90  <- length(val[val>=0.9 & !is.na(val)]) * 100 / 1000000
      
      #  quantiles of DEM habitat for all observed dens
      #         5%        10%        25%        33%        50%        66%        75% 
      # 0.02097290 0.03974938 0.13320500 0.22224025 0.42777476 0.56898328 0.62407766
      
      b10  <- length(val[val>=0.03974938 & !is.na(val)]) * 100 / 1000000
      b25  <- length(val[val>=0.13320500 & !is.na(val)]) * 100 / 1000000
      b33  <- length(val[val>=0.22224025 & !is.na(val)]) * 100 / 1000000
      b50  <- length(val[val>=0.42777476 & !is.na(val)]) * 100 / 1000000
      b66  <- length(val[val>=0.56898328 & !is.na(val)]) * 100 / 1000000
      b75  <- length(val[val>=0.62407766 & !is.na(val)]) * 100 / 1000000
      
      dat <- data.frame(tile,new.id=ii,all,
                        h10,h20,h30,h40,h50,h60,h70,h80,h90,
                        b10,b25,b33,b50,b66,b75)
      if(ii==unique(coast.tile$new.id)[1]) dat2 <- dat else dat2 <- rbind(dat2,dat)
      save(dat2,file=paste("generated data\\DEM habitat\\data\\extracted values DEM den habitat ",tile,".RData",sep=""))
    }
    
    rm(m4,m2,m1,r1,s1,val)
  }
}



load("generated data\\Barents Sea islands split into 200km coastline polygons 60km inland buffer more than 1sqkm.RData")
files <- list.files("generated data/DEM habitat/data", ".RData")
for(i in 1:length(files)){
  load(paste0("generated data/DEM habitat/data/",files[i]))
  if(i==1) dat3 <- dat2 else dat3 <- rbind(dat3,dat2)
}


dat4 <- merge(data.frame(coastp3),dat3,all=T,by="new.id")
save(dat4,file="generated data\\DEM habitat\\DEM den habitat data small polygons without inner buffer.RData")
load("generated data\\DEM habitat\\DEM den habitat data small polygons without inner buffer.RData")

# quantiles of model values for all known dens in Svalbard
#         5%        10%        25%        33%        50%        66%        75% 
# 0.02097290 0.03974938 0.13320500 0.22224025 0.42777476 0.56898328 0.62407766


library(ggplot2)
library(RColorBrewer)

agg3 <- aggregate(dat4[,c(12:28)],
                  by = list(dat4$group),
                  FUN = function(x) sum(x,na.rm=T))
agg3$group <- factor(agg3$Group.1, levels = c("WSVB","ESVB","FJL","NNZ","SNZ"))


cols <- brewer.pal(12,"Paired")
cols <- c(cols[1], cols[2], grey(0.6), cols[5], cols[6])

ggplot(agg3, aes(x=group, y=b33/1000, fill=group)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=cols[c(1:3,5,4)]) +
  theme_minimal() +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=(b25)/1000, ymax=(b50)/1000), width=.2,
                position=position_dodge(.9))


