
# load libraries
library(dplyr)
library(ggplot2)
library(sf)
library(osmdata)
library(tmaptools)
library(ggrepel) # for labeling
# library(directlabels)
# devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(ggspatial)
library(maptools)
library(raster)
library(rnaturalearth)

library(cowplot)
library(ggpubr)
### Load spatial datasets

EZZ.mad <- st_read(dsn = 'input/EEZ_Madeira.shp')
EZZ.mad.box <- st_bbox(EZZ.mad)
EZZ.mad.box
mad <- st_read('input/madeiraP.shp') # madeira island
cont <- st_read('input/md_etopo1_bathymetry_1000.shp') %>% filter(Elevation != 0)
town <- st_read('input/town.shp') %>% filter(name %in% c('Calheta', 'Ponta do Sol', 'Ribeira Brava', 'Câmara de Lobos', 'Funchal'))
city <- st_read('input/city.shp')
meows_ecos <- st_read(dsn="input/meow_ecos.shp") %>% filter(ECOREGION %in% c("Azores Canaries Madeira", "Cape Verde"))
madeira_coastline.sf <- st_read(dsn = 'input/madeira_administrative_boundary.shp')
madeira_bbox <- st_as_sfc(st_bbox(madeira_coastline.sf))
# world_map <- st_read("input/world_map.shp")
meows.cxhul <- (st_convex_hull(st_union(meows_ecos)))
data("wrld_simpl")
wrld_simpl <- st_as_sfc(wrld_simpl)

# reloading from the saved file in the same session with same arguments
spdf_world2 <-    ne_download( scale = "large", type = 'countries' )

spdf_world2 <- st_as_sfc(spdf_world2)
st_write(spdf_world2, "output/world_detailed.shp")

### plot maps

wrld_simpl <- st_read('output/world_detailed.shp')

# Madeira

funchalPort <- st_as_sfc(st_bbox(st_multipoint(matrix(data = c(-16.88905,-16.92571,32.635, 32.65225),nrow= 2))))
st_crs(funchalPort) <- 4326

CaniPort <- data.frame(name=c("Caniçal Port"), longitude=c(-16.736389), latitude=c(32.738889))
CaniPort <- st_as_sf(CaniPort, coords = c("longitude", "latitude"), crs = 4326)

p <- ggplot(data=mad) +
  geom_sf(fill="antiquewhite", colour="steelblue3") +
  geom_sf(data=cont, col=alpha("steelblue3",0.5), size=.1) +
  geom_sf_text(data=cont, aes(label = Elevation), size=2, col="steelblue3") +
  geom_sf(data=funchalPort, col="red", fill=NA) +
  # geom_sf(data = town) + 
  geom_sf(data = city ) + 
  geom_sf(data = CaniPort) + 
  # geom_sf_text_repel(data=town, aes(label=name), size=2) +
  geom_sf_text_repel(data=city, aes(label=name), size=3, nudge_y = 0.03) +
  geom_sf_text_repel(data=CaniPort, aes(label=name), size=3, nudge_y = 0.03) +
  
  annotate(geom = "text", x = -17, y = 32.87, label = "Madeira", 
           color = "black", size = 4.5) +
  annotate(geom = "text", x = -16.3, y = 33.15, label = "Porto Santo", 
           color = "black", size = 4.5) +
  annotate(geom = "text", x = -16.4, y = 32.52, label = "Desertas", 
           color = "black", size = 4.5) +
  coord_sf(crs=4326, xlim=c(-16.25,-17.45), ylim=c(32.35, 33.2)) +
  scale_x_continuous(breaks = c(-16.5,-17)) + 
  scale_y_continuous(breaks = c(32.5, 33 )) +
  annotation_scale(location = "br", width_hint = 0.5, height = unit(x = 0.15,units = "cm")) +
  theme(axis.title=element_blank(), 
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7, angle=90),
        plot.margin = unit(c(0,0,0,0), "cm")) + 
  labs(title="", x="", y="")

p

p +   coord_sf(crs=3035, xlim = c(1789006, 1892024), ylim = c(1475971, 1544676))


# Selvagens 

pp <- ggplot(data=mad) +
  geom_sf(fill="antiquewhite", size=0.1, colour="steelblue3") +
  # geom_sf(data=cont, col="grey", size=.1) + 
  # geom_sf_text(data=cont, aes(label = Elevation), size=2) +
  annotate(geom = "text", x = -15.913, y = 30.15, label = "   Selvagem \n Grande", 
           color = "black", size = 3) +
  annotate(geom = "text", x = -16, y = 30.046, label = "Selvagem Pequena", 
           color = "black", size = 3) +
  annotate(geom = "text", x = -15.94, y = 30.085, label = "Ilhas Selvagens", 
           color = "black", size = 4.5) +
  annotate(geom = "text", x = -15.97, y = 30.105, label = "30.1°N 16°W ", 
           color = "black", size = 2.5) +
  # annotate(geom = "text", x = -16.052, y = 30.102, label = "30.1°N", 
  #          color = "black", size = 2) +
  coord_sf(crs=4326, xlim=c(-15.85,-16.05), ylim=c(30.03, 30.1625)) +
  scale_x_continuous(breaks = c(-16),sec.axis = dup_axis()) + 
  scale_y_continuous(breaks = c(30.1),sec.axis = dup_axis(name = '')) +
  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y =  element_blank(), 
        panel.grid.major = element_line(color = gray(.7), linetype = "dashed", size = 0.05), 
        panel.background = element_rect(fill = "aliceblue")
        ) +
  labs(title="", x="", y="")


pp

# madeira and selvagens

t <- ggdraw() + 
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(pp, 0.05,0.025,0.39,0.39)
t

ggsave(t, filename = "output/mad_sal.tiff",dpi = "retina", width = 10, height = 9.25,  units = "in")


# Funchal port 

ppp <- ggplot(data=mad) +
  geom_sf(fill="antiquewhite", colour="steelblue3") +
  # geom_sf(data=cont, col="grey", size=.1) + 
  # geom_sf_text(data=cont, aes(label = Elevation), size=2) + 
  annotate(geom = "text", x = -16.907, y = 32.65125, label = "Funchal Port", 
           color = "black", size = 3.4) +
  coord_sf(crs=4326, xlim=c(-16.88905,-16.92571), ylim=c(32.635, 32.65225)) +
  # scale_x_continuous(breaks = c(-16.91)) + 
  # scale_y_continuous(breaks = c(32.642)) +
  theme_bw() +
  theme(axis.title=element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        # panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.grid.major = element_line(colour = "transparent"),
        panel.background = element_rect(fill = "aliceblue"),
        panel.spacing = element_blank(),
        panel.border = element_rect(colour = "red", fill=NA),
        plot.margin = unit(c(0,0,-0.5,-0.5), "cm")) +
  labs(x=NULL, y=NULL)


ppp

ggsave(ppp, filename = "output/port.tiff",dpi = "retina", width = 9,height = 5)

# madeira and port

t2 <- ggdraw() + 
  draw_plot(t, 0, 0, 1, 1) +
  draw_plot(ppp, 0.45,0.15,0.2,0.2)
t2


ggsave(t2, filename = "output/madeira_situation_map.tiff",dpi = "retina")

# world

source('utils.R')

# background layer (ocean and graticules)

bb <- make_bbox(c(-180, 180), c(-90, 90), spacing = c(NA, 0.1), proj = projection(mad.sp))

grat <- make_graticules(seq(-150, 180, 30), seq(-90, 90, 30),
                        spacing = c(10, 1), proj = projection(mad.sp))

grat.sf <- st_as_sfc(grat)
bb.sf <- st_as_sfc(bb)


r <- ggplot() +
  # borders()+
  geom_sf(data = bb.sf,
          fill = "light blue", color = NA) +
  # geom_sf(data = grat.sf,
  #         color = NA, size = 0.15) +
  geom_sf(data = wrld_simpl, size = .2, fill = "antiquewhite", col = NA) +
  geom_sf(data=meows.cxhul, fill=alpha("red",0.8), colour=NA) +
  theme_minimal()+
  coord_sf(crs = 54030) +
  labs( x=NULL, y=NULL) +
  # scale_x_continuous(breaks = c(-180,-60, 0, 60, 180)) + 
  # scale_y_continuous(breaks = c(-90, -45, 0, 45, 90)) + 
  theme_bw() + 
  theme(text = element_text(family = "Helvetica"),
        axis.title=element_blank(),
        axis.line = element_blank(),
        axis.text= element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,-0.1,-0.1), "cm"),
        panel.border = element_rect(colour = "black"), 
        panel.background = element_blank())

r

ggsave(filename="output/world.pdf", 
       plot = r, 
       device = cairo_pdf, 
       width = 297, 
       height = 210, 
       units = "mm")

# macaronesia

rr <- ggplot() +
  geom_sf(data=wrld_simpl, fill="antiquewhite") +
  geom_sf(data=madeira_bbox, fill=NA, color="red") +
  geom_sf(data=meows.cxhul, fill="transparent", colour=alpha("red",1), linetype = "dotted") +
  # geom_sf_text(data=madeira_bbox, aes(label = "Madeira"), position=position_stack(vjust = 1.1)) + 
  geom_sf(data=madeira_coastline.sf, fill="antiquewhite") +
  annotate(geom = "text", x = -25, y = 35, label = "Macaronesia", 
           color = "grey", size = 6) +
  annotate(geom = "text", x = -25, y = 25, label = "ATLANTIC \n OCEAN", fontface="italic",
           color = "deepskyblue3", size = 5) +
  annotate(geom = "text", x = -26, y = 40, label = "Azores",
           color = "gray25", size = 4) +
  annotate(geom = "text", x = -17, y = 33.9, label = "Madeira",
           color = "gray25", size = 4) +
  annotate(geom = "text", x = -24.5, y = 18.2, label = "Cape Verde",
           color = "gray25", size = 4) +
  annotate(geom = "text", x = -17, y = 29.5, label = "Canary Islands",
           color = "gray25", size = 4) +
  coord_sf(crs = 4326, datum = 4326, xlim = c(-33,-1.5), ylim = c(12,42.4)) +
  scale_x_continuous(breaks = c(-30,-20,-10, 0)) + 
  scale_y_continuous(breaks = c(10, 20, 30,40)) + 
  annotation_north_arrow(location = "bl", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.5, height = unit(x = 0.15,units = "cm")) + 
  labs(title=NULL, x=NULL, y=NULL) +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        axis.title=element_blank(),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6, angle=90),
        plot.margin = unit(c(0,0,0,0), "cm")) 

rr

rr + coord_sf(crs=54032, xlim = c(-3284541, 0), ylim = c(1330925, 4906430))

label.mac <- st_transform(st_sfc(st_point(c(-25,35)), crs=4326), crs=54032)
label.atoc <- st_transform(st_sfc(st_point(c(-25,25)), crs=4326), crs=54032)
label.az <- st_transform(st_sfc(st_point(c(-26,40)), crs=4326), crs=54032)
label.mad <- st_transform(st_sfc(st_point(c(-17,33.9)), crs=4326), crs=54032)
label.cv <- st_transform(st_sfc(st_point(c(-24.5,18.2)), crs=4326), crs=54032)
label.ci <- st_transform(st_sfc(st_point(c(-17,29.5)), crs=4326), crs=54032)


rr <- rr + coord_sf(crs=54032, xlim = c(-3284541, 0), ylim = c(1330925, 4906430)) + 
  annotate(geom = "text", x = -2600700, y = 3987445, label = "Macaronesia", 
           color = "grey", size = 6) +
  annotate(geom = "text", x = -2600700, y = 2851551, label = "ATLANTIC \n OCEAN", fontface="italic",
           color = "deepskyblue3", size = 5) +
  annotate(geom = "text", x = -2398433, y = 4563697, label = "Azores",
           color = "gray25", size = 4) +
  annotate(geom = "text", x = -1664996, y = 3802994, label = "Madeira",
           color = "gray25", size = 4) +
  annotate(geom = "text", x = -2633298, y = 2074451, label = "Cape Verde",
           color = "gray25", size = 4) +
  annotate(geom = "text", x = -1721228, y = 3309783, label = "Canary Islands",
           color = "gray25", size = 4)


rr


st_transform(st_sfc(st_point(c(-16,12)), crs=4326), crs=54032)
st_transform(st_sfc(st_point(c(-2,20)), crs=4326), crs=54032)


tt <- ggdraw() + 
  draw_plot(rr, 0, 0, 1, 1) +
  draw_plot(r, 0.51,0,0.35,0.35)
tt

ggsave(last_plot(), filename = "output/macaronesia.tiff",dpi = "retina", units = "in")
