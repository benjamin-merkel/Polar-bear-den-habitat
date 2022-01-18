library(tmap)
library(sf)
library(RColorBrewer)
library(spData)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggpubr)

proj.laea30 <- ("+proj=laea +lat_0=90 +lon_0=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")


worldrn <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()
world_crop <- st_transform(worldrn, proj.laea30)
world_crop <- st_union(st_make_valid(world_crop))

lat <- 60
lon <- 40
ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')
world2 <- st_union(st_make_valid(world))
world2 <- st_transform(world2, ortho)


pb_sub <- read_sf("raw data/map data/IUCN Polar Bear Subpopulation Boundaries.shp")
pb_sub <- st_transform(st_make_valid(pb_sub), proj.laea30)
pb_sub <- st_cast(pb_sub,"POLYGON")
pb_sub$id <- 1:nrow(pb_sub)
pb_bs <- pb_sub[29,]

load("generated data\\Barents Sea islands split into 200km coastline polygons 60km inland buffer more than 1sqkm.RData")
coast.id <- data.frame(coastp3)

coast.lines<-st_read("raw data\\map data", "Coastline_GSHHS_NPI")
coast.lines<-st_cast(coast.lines,"POLYGON")
coast.lines<-st_transform((coast.lines), proj.laea30)
coast.lines<-coast.lines[st_area(coast.lines) < max(st_area(coast.lines)),]

# buff30 <- st_transform(st_as_sf(buff),proj.laea30)
coastp30 <- st_transform(st_as_sf(coastp3), proj.laea30)
coastp30 <- coastp30[!is.na(coastp30$group),]
coastp30$group <- factor(coastp30$group, levels = c("WSVB","ESVB","FJL","NNZ","SNZ"))


cols <- brewer.pal(12,"Paired")
cols <- c(cols[1], cols[2], grey(0.6), cols[5], cols[6])



label <- data.frame(x=c( -500,    0, 250,  750, 650)*1000,
                    y=c(-1250,-1300,-900,-1250,-1750)*1000,
                    name= c("WSVB","ESVB","FJL","NNZ","SNZ"))
label <- st_as_sf(label, coords = c('x','y'), crs=st_crs(coast.lines))
label$name <- factor(label$name, levels = c("WSVB","ESVB","FJL","NNZ","SNZ"))

# load DEM den habitat and aggregate by polygon id 
load("generated data\\DEM habitat\\DEM den habitat data small polygons without inner buffer.RData")
# quantiles of model values for all known dens in Svalbard
#         5%        10%        25%        33%        50%        66%        75% 
# 0.02097290 0.03974938 0.13320500 0.22224025 0.42777476 0.56898328 0.62407766

agg3 <- aggregate(dat4[,c(12:28)],
                  by = list(dat4$group),
                  FUN = function(x) sum(x,na.rm=T))
agg3$group <- factor(agg3$Group.1, levels = c("WSVB","ESVB","FJL","NNZ","SNZ"))





bar <- ggplot(agg3, aes(x=group, y=b33/1000, fill=group)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=cols[c(1:3,5,4)]) +
  theme_classic() +
  theme(legend.position="none", axis.line.x = element_line("transparent")) +
  xlab("") +
  ylab(expression(denning~habitat~"[ 1000"~km^2~"]")) +
  scale_y_continuous(expand = c(0, 0)) +
  
  geom_errorbar(aes(ymin=(b25)/1000, ymax=(b50)/1000), width=.2,
                position=position_dodge(.9))


map_frame <- st_union(coastp30, pb_bs)


mainmap <- tm_shape(map_frame) +
  tm_fill(col=grey(1)) +
  
  tm_shape(st_make_valid(coast.lines)) +
  tm_fill(col=grey(0.8)) +

  tm_shape(pb_bs) +
  tm_fill(col=grey(0.96)) +
   
  tm_shape(pb_sub) +
  tm_borders(col=grey(0.2),lwd=1) +
  
  tm_grid(col=grey(0.7), tick=F, x = seq(0, 100, by = 10), y = seq(70,90,4), projection=4326, 
          labels.cardinal = T, labels.format = list(suffix = intToUtf8(176)), labels.rot = c(0,90),lwd=0.6) +
  
  tm_shape(coastp30) +
  tm_fill(col = "group", palette = cols[c(1:3,5,4)], title = "Island group", legend.show = F) +
  tm_borders(col=1) +
  
  tm_shape(label) +
  tm_text('name',col ="name", palette = cols[c(1:3,5,4)], fontface="bold", size = 1.2) +
  tm_scale_bar(position = c("LEFT","BOTTOM")) +
  tm_layout(outer.margins=c(0.02, 0.02, 0.02, 0.26), legend.show = F)

inset <- tm_shape(pb_sub) +
  tm_borders(col=grey(0.8),lwd=1) +
  
  tm_shape(pb_bs) +
  tm_fill(col='beige') +
  
  tm_shape(pb_bs) +
  tm_borders(col="black",lwd=1.5) +
  
  tm_shape(world_crop) +
  tm_fill(col=grey(0.8)) +
  
  tm_grid(col=grey(0.6), tick=F, x = seq(-180, 180, by = 90), y = seq(40,80,20), projection=4326, 
          labels.cardinal = F, labels.show = F, lwd=0.4) +
  
  tm_shape(pb_sub) +
  tm_borders(col=grey(0.4),lwd=1) +
  
  tm_layout(frame = T, bg.color = "white")


png("figures\\Polar bear Barents Sea divided in groups smaller polygons without inland buffer tmap2.png",width=30,height=20,units="cm",res=700)
mainmap
print(inset, vp = grid::viewport(0.1, 0.83, width = 0.4, height = 0.32))
print(bar,   vp = grid::viewport(0.85, 0.5, width = 0.2, height = 1))
dev.off()
  
