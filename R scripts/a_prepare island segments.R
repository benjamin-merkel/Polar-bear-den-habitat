library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(RColorBrewer)
source("R scripts/FUNCTION line segmentation.R")

nsidc <- CRS(' +init=epsg:3411 +units=m')
load("generated data\\22km inland buffer.RData")

coast.lines <-st_read("raw data\\map data", "Coastline_GSHHS_NPI")
coast.lines <-st_transform(coast.lines,crs=3411)
ext2        <- extent(800*1000, 2700*1000, -1000*1000, 700*1000)
coast.lines <- st_crop(coast.lines,ext2)
coast.lines$id2 <- 1:788

sf_coast <- st_cast(coast.lines, "LINESTRING") 
sf_coast <- st_polygonize(sf_coast)
sf_coast$area <- st_area(sf_coast)
sf_coast <- sf_coast[as.numeric(sf_coast$area)>0,]
coastp   <- as(sf_coast, "Spatial") # If you want sp

# remove some russian islands south of NZ 
coastp <- coastp[!coastp$id2 %in% c(3,4),]
coastl <- as(coast.lines[coast.lines$id2 %in% coastp$id2,], "Spatial")


# all islands bigger than 9e+08 sqm

# run for islands Spitsbergen: 545, Nordaustlandet:573, Edgeoya: 711, Barentsøya: 692, north and south novaya zemlya: 1,2
# biggest islands in FJL: 5 to 10
for(coli in c(545,711,573,692,1,2,5:10)) {
  
  sl2 <- SegmentSpatialLines(coastl[coastl$id2==coli,], length = 200*1000, merge.last = TRUE)
  proj4string(sl2) <- proj4string(coastl)
  sl2$id <- 1:length(sl2)
  sl2 <- st_as_sf(sl2)
  st_crs(sl2) <- st_crs(sf_coast)
  
  bu2 <- st_buffer(sl2,dist=60*1000)
  bu2$id <- 1:nrow(bu2)
  
  gridall <- st_make_grid(bu2, cellsize = 0.25*1000, what = "centers")
  
  
  #step by step
  bu3 <- list()
  for(id in 1:length(bu2$id)) {
    cat("\r",coli,'-',id," of ",length(bu2$id), "  ")
    
    buffer.step <- bu2[bu2$id == id,]
    buffer.inc  <- st_intersection(buffer.step, bu2)
    coast.step  <- sl2[sl2$id %in% buffer.inc$id.1,]
    
    sf_poly     <- st_intersection(sf_coast[sf_coast$id2==coli,],buffer.inc)
    
    grid <- st_intersection(gridall, st_as_sf(buffer.step)) 
    grid <- st_intersection(grid, sf_poly) 
    
    dis <- list()
    for(ss in 1:length(coast.step$id))  dis[[ss]] <- st_distance(st_as_sf(coast.step[ss,]), grid)
    
    
    df <- data.frame(st_coordinates(grid))
    for(ss in 1:length(coast.step$id)) df[,ss+2] <- as.vector(dis[[ss]])
    colnames(df) <- c("x","y",coast.step$id)
    df$buffer <- apply(df[,3:(length(coast.step$id)+2)],1,function(x) as.numeric(names(x)[which(x==min(x))][1]))
    
    
    ext <- extent(as(grid, "Spatial"))
    r <- raster(resolution = 0.25*1000, ext = ext, crs = nsidc)
    
    #convert the points to a spatial object class sf
    dist_sf <- st_as_sf(df, coords = c("x", "y")) %>%  st_set_crs(3055)
    
    #create the distance raster
    ras_buff <- rasterize(dist_sf, r, "buffer", fun = mean)
    ras_buff[ras_buff!=id] <- NA
    
    polybuff <- rasterToPolygons(ras_buff,dissolve = T)
    st_polybuff <- st_as_sf(polybuff)
    st_crs(st_polybuff) <- st_crs(sf_coast)
    
    bu3[[id]] <- st_intersection(sf_coast[sf_coast$id2 == coli,],st_polybuff)
    bu3[[id]]$buffer.id <- id
  }
  
  save(sl2,file=paste("generated data/",coli," 200 km coastline pieces.RData",sep=""))
  save(bu2,file=paste("generated data/",coli," 200 km coastline pieces 60km inward buffer initial polygons.RData",sep=""))
  save(bu3,file=paste("generated data/",coli," 200 km coastline pieces 60km inward buffer polygons.RData",sep=""))
}



coastp$layer <- NA
coastp$buffer.id <- NA
load("generated data/545 200 km coastline pieces 60km inward buffer polygons.RData")
coastp2 <- rbind(coastp[!coastp$id2 %in% c(573,711,1,2,545,5),], as(bu3[[1]], "Spatial"))
for(coli in 2:length(bu3)) coastp2 <- rbind(coastp2, as(bu3[[coli]], "Spatial"))
load("generated data/573 200 km coastline pieces 60km inward buffer polygons.RData")
for(coli in 1:length(bu3)) coastp2 <- rbind(coastp2, as(bu3[[coli]], "Spatial"))
load("generated data/711 200 km coastline pieces 60km inward buffer polygons.RData")
for(coli in 1:length(bu3)) coastp2 <- rbind(coastp2, as(bu3[[coli]], "Spatial"))
load("generated data/2 200 km coastline pieces 60km inward buffer polygons.RData")
for(coli in 1:length(bu3)) coastp2 <- rbind(coastp2, as(bu3[[coli]], "Spatial"))
load("generated data/1 200 km coastline pieces 60km inward buffer polygons.RData")
for(coli in 1:length(bu3)) coastp2 <- rbind(coastp2, as(bu3[[coli]], "Spatial"))
load("generated data/5 200 km coastline pieces 60km inward buffer polygons.RData")
for(coli in 1:length(bu3)) coastp2 <- rbind(coastp2, as(bu3[[coli]], "Spatial"))



save(coastp2,file="generated data\\Barents Sea islands split into 200km coastline polygons 60km inland buffer.RData")
load("generated data\\Barents Sea islands split into 200km coastline polygons 60km inland buffer.RData")
coastp2$area <- gArea(coastp2,byid=T)
coastp2$centroid_x <- coordinates(gCentroid(coastp2, byid = T))[,1]
coastp2$centroid_y <- coordinates(gCentroid(coastp2, byid = T))[,2]
coastp2 <- coastp2[coastp2$centroid_x < 2150000,]
coastp2 <- coastp2[!coastp2$id2 %in% c(23, 24),]
coastp2 <- coastp2[!(coastp2$centroid_x > 1700000 & coastp2$centroid_y > 470000),]


# remove isladns less than 1 sqkm
coastp3 <- coastp2[coastp2$area > 1000,]

# add groupings
coastp3$group <- "FJL"
coastp3$group[coastp3$centroid_x > 1200000] <- "NZ"
coastp3$group[coastp3$centroid_y < -1e+05] <- "ESVB"
coastp3$group[coastp3$centroid_y < -5.7e+05] <- "WSVB"

coastp3$group[coastp3$id2 %in% c(1,11,37,47,52:54,56,64,69,71,72,74,76,80,83,84,86,88:90,93:96,98,
                                 101:103,109:111,113:115,117,119,125,127,133,134,137:139,142,143,147,
                                 150:153,155:157,162,165,168,180,183,186,189:192)] <- "NNZ"

coastp3$group[coastp3$id2 %in% c(2,11,37,53,54,56,64,74,83,86,90,93:96,98,
                                 101,103,109:111,115,117,119,125,127,133,134,137:139,142,143,
                                 150,151,153,156,157,162,165,180,183,189,191,192)] <- "SNZ"

coastp3$group[coastp3$centroid_x < max(coastp3$centroid_x[coastp3$group=="NNZ"]) & coastp3$group=="NZ"] <- "NNZ"
coastp3$group[coastp3$group=="NZ"] <- "SNZ"

coastp3$group[coastp3$id2 %in% c(5:10,12:22,25:31,33,34,36,38:45,48,49,51,55,57,59,61,63,65:67,73,
                                 75,77,78,82,99,104,106:108,116,124,132,140,145,149,159,160,164,166,
                                 169,172,173,175,176,178,182,184)] <- "FJL"

coastp3$group[coastp3$id2 %in% c(545,556:558,561,573,577,578,592,594,602,624,640,651,653,655,662,
                                 680,692,700,711,724,741,758,760,761:769,771,774:782,785,788)] <- "ESVB"

coastp3$group[coastp3$id2 %in% c(541,542,545,547,548,549,589:591,603,605:608,626,659,686,688,
                                 753,770,772,773,783)] <- "WSVB"

coastp3$group[coastp3$id2 %in% 545 & coastp3$buffer.id %in% c(10:15)] <- "ESVB"

coastp3$id3 <- paste(coastp3$id2,coastp3$buffer.id,sep="-")
coastp3 <- coastp3[order(coastp3$centroid_x),]
coastp3 <- coastp3[order(coastp3$centroid_y),]
coastp3$new.id <- 1:nrow(coastp3)
save(coastp3,file="generated data\\Barents Sea islands split into 200km coastline polygons 60km inland buffer more than 1sqkm.RData")
 
# # calc 50km buffer around all islands
buff <- gBuffer(coastp3,byid=T,width=50000)
save(buff,file="generated data\\coast complex buffer 2.RData")

coastp30 <- spTransform(coastp3,proj.laea30)
coastp30 <- coastp30[!is.na(coastp30$group),]

cols <- brewer.pal(12,"Paired")
cols <- c(cols[1], cols[2], grey(0.6), cols[5], cols[6])
  
coastp30$color <- cols[1]
coastp30$color[coastp30$group %in% "ESVB"]<- cols[2]
coastp30$color[coastp30$group %in% "FJL"] <- cols[3]
coastp30$color[coastp30$group %in% "SNZ"] <- cols[4]
coastp30$color[coastp30$group %in% "NNZ"] <- cols[5]


png("figures\\Polar bear Barents Sea divided in groups smaller polygons without inland buffer.png",width=23,height=20,units="cm",res=700)
plot(coastp30,col=coastp30$color,border=grey(0.2),lwd=0.8)
dev.off()
