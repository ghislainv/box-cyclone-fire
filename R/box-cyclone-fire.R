#!/usr/bin/Rscript

## ==============================================================================
## author          :Ghislain Vieilledent
## email           :ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
## web             :https://ghislainv.github.io
## license         :CC-BY-SA 4.0
## ==============================================================================

##= Libraries
pkg <- c("broom","sp","rgdal","raster","ggplot2","gridExtra",
         "rasterVis","rgeos","dplyr","readr")
## broom: to convert map into data-frame with tiny()
## gridExtra: to combine several ggplots
## rasterVis: for gplot()
## rgeos: for crop()
load.pkg <- function(x) {
  if(!require(x, character.only = T)) {
    install.packages(x)
    require(x, character.only = T)
  }
}
loaded <- lapply(pkg,load.pkg)
## Remove useless objects
rm(pkg,load.pkg,loaded)

## =======================================
## Prepare data
## =======================================

## Set region for Kirindy-Mitea National Park (KMNP)
xmin.KMNP <- 365000; xmax.KMNP <- 430010
ymin.KMNP <- 7637756; ymax.KMNP <- 7730000
Extent.KMNP <- paste(xmin.KMNP, ymin.KMNP, xmax.KMNP, ymax.KMNP)
e.KMNP <- extent(c(xmin.KMNP, xmax.KMNP, ymin.KMNP, ymax.KMNP))
r.KMNP <- raster(ext=e.KMNP, crs="+init=epsg:32738")
r.KMNP.latlong <- projectRaster(r.KMNP, crs="+init=epsg:4326")
e.KMNP.latlong <- extent(r.KMNP.latlong)

## gdalwrap
f <- c("fcc_2000_2017")
system(paste0("gdalwarp -overwrite -dstnodata 0 \\
              -r near -tr 30 30 -te ",Extent.KMNP," -of GTiff \\
              -co 'compress=lzw' -co 'predictor=2' \\
              gisdata/rasters/fcc_2000_2017.tif \\
              rast/fcc_2000_2017_KMNP.tif"))

##=======================
## KMNP
## Import rasters
fcc_2000_2017_KMNP <- raster("rast/fcc_2000_2017_KMNP.tif")
## Deforestation 2009-2017 following cyclone Fanele in January 2009 
defor_KMNP <- fcc_2000_2017_KMNP
defor_KMNP[values(defor_KMNP)<9] <- NA
defor_KMNP[values(defor_KMNP)>=9 & values(defor_KMNP)<=17] <- 2
defor_KMNP[values(defor_KMNP)==118] <- 1
writeRaster(defor_KMNP,filename="rast/defor_KMNP.tif",overwrite=TRUE)

##=================================================================================
## Import maps (shapefiles) and convert to data-frame for ggplot with bloom::tiny()

## SAPM ("système d'aires protégées à Madagascar")
sapm <- readOGR(dsn="gisdata/vectors/sapm",layer="AP-NAP_38s")
sapm_region <- subset(sapm, NOM %in% c("Mikea", "Kirindy Mitea", "Menabe Antimena", "Andranomena", "Ranobe PK 32"))
sapm.df <- tidy(sapm_region)

## Madagascar boundaries
mada.latlong <- readOGR(dsn="gisdata/vectors/mada",layer="MAD_outline")
proj4string(mada.latlong) <- "+init=epsg:4326"
mada <- spTransform(mada.latlong,CRSobj=CRS("+init=epsg:32738"))
mada.df <- tidy(mada)
## Compute land area
# KMNP
land.KMNP <- crop(mada,e.KMNP)
area.land.KMNP <- round(sum(sapply(slot(land.KMNP, "polygons"), slot, "area"))/10000)

## Roads
roads.latlong <- readOGR(dsn="gisdata/vectors/roads",layer="tr_route_polyline")
roads <- spTransform(roads.latlong,CRSobj=CRS("+init=epsg:32738"))
roads.df <- tidy(roads)

## Localities and field observations
Belo <- readOGR(dsn="gisdata/vectors/additional_points",layer="Belo")
Obs <- readOGR(dsn="gisdata/vectors/additional_points",layer="Obs")
## df for localities and field observations
Belo.df <- as.data.frame(Belo); names(Belo.df)[12:13] <- c("x","y")
Obs.df <- as.data.frame(Obs); names(Obs.df)[3:4] <- c("x","y")

## Cyclones
Fanele.latlong <- readOGR(dsn="gisdata/vectors/cyclones",layer="Fanele")
Fanele <- spTransform(Fanele.latlong,CRSobj=CRS("+init=epsg:32738"))
Fanele.df <- tidy(Fanele)

## =======================================
## Plot raster with gplot() from rasterVis
## =======================================

## Setting basic theme options for plot with ggplot2
theme_base <- theme(axis.line=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    legend.position="none",
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.border=element_blank())

##===========
## Madagascar
plot.Mada <- ggplot(data=mada.df,aes(x=long,y=lat,group=id)) +
  geom_polygon(colour=grey(0.4),fill=grey(0.8),size=0.2) +
  geom_rect(aes(xmin=xmin.KMNP,xmax=xmax.KMNP,ymin=ymin.KMNP,ymax=ymax.KMNP),
            fill="transparent",colour="black",size=0.2) +
  theme_bw() + theme_base + theme(plot.margin=unit(c(-0.25,-0.25,-0.5,-0.5),"line"),
                                  panel.background=element_rect(fill="azure")) +
  coord_equal()
## Grob
grob.Mada <- ggplotGrob(plot.Mada)

## Resolution of rasters
high.res <- TRUE
res.rast <- ifelse(high.res, 10e5, 10e3)

## KMNP
# Build deforestation plot
plot.defor.KMNP <- gplot(defor_KMNP,maxpixels=res.rast) +
  annotation_custom(grob=grob.Mada,xmin=xmin.KMNP+8000,
                    xmax=xmin.KMNP+8000+12500,
                    ymin=ymin.KMNP+60000,ymax=ymin.KMNP+60000+36000) +
  geom_polygon(data=mada.df, aes(x=long, y=lat, group=id), colour=grey(0.5), fill="white", size=0.3) +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values=c("forestgreen","red"),na.value="transparent") +
  geom_path(data=roads.df, aes(x=long, y=lat, group=group), colour="black", size=0.2) +
  geom_path(data=Fanele.df, aes(x=long, y=lat, group=group), linetype="dashed", colour="black", size=0.4) +
  geom_polygon(data=sapm.df, aes(x=long, y=lat, group=group), colour="black", fill="transparent", size=0.6) +
  geom_point(data=Belo.df, aes(x=x, y=y), color="black", size=1.5, shape=16) +
  geom_text(data=Belo.df, aes(label=TOPONYME), size=3, vjust=0, nudge_y=1000, hjust=0.5, nudge_x=-3250) +
  geom_point(data=Obs.df, aes(x=x, y=y), color="black", size=2, shape=4, stroke=1) +
  #geom_text(data=Obs.df, aes(label=Obs), size=4, hjust=1, nudge_x=-1000) +
  coord_equal(xlim=c(xmin.KMNP,xmax.KMNP),ylim=c(ymin.KMNP,ymax.KMNP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  #annotate("text",x=xmin.KMNP,y=ymax.KMNP,label="b",hjust=0,vjust=1,size=4,fontface="bold") +
  theme_bw() + theme_base + theme(plot.margin=unit(c(0,0.2,0,0),"cm"), panel.background=element_rect(fill="azure"))
ggsave(filename="figs/fcc_KMNP_orig.png",plot=plot.defor.KMNP,width=7,height=10,unit=c("cm"))

## =====================================
## Area deforested inside protected area
## =====================================

library(sf)
library(stars)
sapm <- st_read(dsn="gisdata/vectors/sapm/AP-NAP_38s.shp") %>%
  dplyr::filter(NOM=="Kirindy Mitea") %>%
  dplyr::mutate(PA=1)

# ## Rasterize the KMNP protected area
# Template <- st_as_stars(defor_KMNP)
# Template[[1]][] <- NA
# sapm_rast <- st_rasterize(sf=sapm[,"PA"], template=Template, burn=1)

## Crop deforestation raster with protected area
defor_PA <- st_crop(st_as_stars(defor_KMNP), sapm)

## Area of forest on 1st January 2009 inside PA
fc2009 <- round(sum(!is.na(defor_PA[[1]][]))*30*30/10000) # 99001 ha
d0917 <- round(sum(defor_PA[[1]][]==2, na.rm=TRUE)*30*30/10000) # 15509 ha

## Percentage of deforestation on 2009--2017 inside PA
p0917 <- round(100*defor2009_2017/fc2009) # 16%

## Save results
df_res <- data.frame(var=c("fc2009", "d0917", "p0917"), val_ha=c(fc2009, d0917, p0917))
write.table(df_res, file="tabs/defor.txt", sep=",", row.names=FALSE)

## EOF