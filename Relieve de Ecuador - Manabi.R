library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
Elev <- raster("Raster/Raster_Ecuador.tif")
Peru        <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Colombia    <- getData('GADM', country='Colombia', level=0) %>% st_as_sf()
Ecuador     <- getData('GADM', country='Ecuador', level=0) %>% st_as_sf()
Ecuado      <- getData('GADM', country='Ecuador', level=1) %>% st_as_sf()
Bolia_xy    <- cbind(Ecuado  , st_coordinates(st_centroid(Ecuado$geometry)))
Boli_alt    <- crop(Elev,Ecuador )                           #   
Boli_alt    <- Boli_alt <- mask(Boli_alt, Ecuador)
slope       = terrain(Boli_alt  , opt = "slope") 
aspect      = terrain(Boli_alt , opt = "aspect")
hill        = hillShade(slope, aspect, angle = 40, direction = 270)
hill.pa        <-  rasterToPoints(hill)
hill.pa_a      <-  data.frame(hill.pa)
dem.p          <-  rasterToPoints(Boli_alt)
df             <-  data.frame(dem.p)

Manabi         <- subset(Ecuado, NAME_1  == "Manabi")
Manabi_box     = st_as_sfc(st_bbox(Manabi ))

Mapa =ggplot()+
  geom_raster(data = hill.pa_a, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  geom_sf(data = Ecuado, fill=NA, color="black", size=0.2)+
  geom_sf(data = Manabi_box, fill=NA, color="#936639", size=1.5)+
  geom_sf(data=Peru, fill=NA, color="black", size=0.5)+
  geom_sf(data=Colombia, fill=NA, color="black", size=0.5)+
  theme_bw()+
  theme(axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        panel.border = element_rect(size = 2))+
  coord_sf(xlim = c(-81.5 ,-75), ylim = c(-7  ,3),expand = FALSE)+
  labs(x = NULL, y = NULL)+
  geom_label(data =  Bolia_xy , aes(x= X, y=Y, label = NAME_1), size = 2.5, color="black", fontface = "bold",fontfamily = "serif", alpha=0.4)+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  # title
  annotate(geom = "text", x = -77, y = 1, hjust = 0, vjust = 1, 
           label = "Ecuador:  Shaded height \n     and relief map",
           size = 5, family="serif", color = "grey20")+
  annotate(geom = "text", x = -77, y = 1.5, hjust = 0, vjust = 1, 
           label = "Colombia",size = 5, family="serif", color = "#606c38",  fontface="italic")+
  annotate(geom = "text", x = -76, y = -4, hjust = 0, vjust = 1, 
           label = "Peru",size = 5, family="serif", color = "#606c38",  fontface="italic")

Ele       <- raster("Raster/Raster_Manabi.tif")
Ecuad     <- getData('GADM', country='Ecuador', level=2) %>% st_as_sf()
Manabii     <- subset(Ecuad , NAME_1 == "Manabi")
Manabii_xy <- cbind(Manabii , st_coordinates(st_centroid(Manabii$geometry)))

slopee    = terrain(Ele  , opt = "slope") 
aspecte    = terrain(Ele , opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
dem          <-  rasterToPoints(Ele)
dfe            <-  data.frame(dem)


MapaA =ggplot()+
  geom_raster(data = hill.pa_a, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  geom_sf(data = Manabii_xy, fill=NA, color="black", size=0.2)+
  coord_sf(xlim = c(-81.08492 ,-79.40501), ylim = c(-1.947404  ,0.3818064),expand = FALSE)+
  theme_bw()+
  theme(axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        panel.border = element_rect(size = 2))+
  labs(x = NULL, y = NULL)+
  geom_label(data =  Manabii_xy , aes(x= X, y=Y, label = NAME_2), size = 2.5, color="black", fontface = "bold",fontfamily = "serif", alpha=0.3)+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -80.4, y =-1.9, hjust = 0, vjust = 0, lineheight = .9,
           label = "Author: Gorky Florez (@gflorezc) Original Idea: Aprende R desde cero, Geometries: RStudio Data: ING-Peru, 2022;",
           size = 3, family = "serif", color = "grey50")+
  # title
  annotate(geom = "text", x = -81.05, y = -0.1, hjust = 0, vjust = 1, 
           label = "Manabi:  Shaded height \n     and relief map",
           size = 5, family="serif", color = "grey20")+
  annotate(geom = "text", x = -80, y = -1.8, hjust = 1, vjust = 0,
           label = "Codigo en Githab", fontface = 2,
           size = 3, family = "serif", color = "grey20")+
  annotate(geom = "text", x = -79.5, y = -1.85, hjust = 1, vjust = 0,
           label = "https://github.com/GorkyFlorez/Mapa_Relieve_Manabi",
           size = 3, family = "serif", color = "#35978f")+
  # date
  annotate(geom = "text", x = -81, y = -1.7, hjust = 0, vjust = 0,
           label = "2022", fontface = 2,
           size = 5, family = "serif", color = "#35978f")

arrowA <- data.frame(x1 = 5.2, x2 = 16, y1 = 15.2, y2 = 20.5)
arrowB <- data.frame(x1 = 5.2, x2 = 16, y1 = 10.2, y2 = 1)

Total= ggplot() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 21), expand = FALSE) +
  annotation_custom(ggplotGrob(Mapa), xmin = 0, xmax = 15, ymin = 0, ymax = 21) +
  annotation_custom(ggplotGrob(MapaA), xmin = 15, xmax = 30, ymin = 0, ymax = 21) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA,  linetype = "dashed", color = "black", size = 1)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB,  linetype = "dashed", color = "black", size = 1)+
  theme_void()

ggsave("Mapa/Manabi_ecuador.png", Total, width = 13, height = 9, 
       dpi = 1200, type = "cairo-png") 


