## ---------------------------
##
## Script name: maritime flow dataset
##
## Purpose of script:  gather datasets to model maritime flow
##
## Author: Jesús Jiménez López
##
## Date Created: 2019-08-13
##
## Copyright (c) Jesús Jiménez López, 2019
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

## set working directory

# setwd("~")      # (mac, linux)
# setwd("C:/Users/")    # (PC)

## ---------------------------

## load up required packages


library(dplyr)
library(readxl)
library(units)
library(geosphere)
require(rgeos)
library(gdistance)
library(sp)
library(sf)
library(mapdata)
library(maptools)
library(rnaturalearth)
library(nominatim)
library(raster)
library(ggalt)
library(ggmap)
library(purrr)
library(viridis)
library(cowplot)
library(pdftools)
library(smoothr)
library(grid)
library(gridExtra)


## ---------------------------

## load up custom functions

# source("functions/example.R")

## ---------------------------

## read data

data.df <- read_excel(path  = 'input/maritime_flow.xlsx',
                      sheet = 1, skip = 1,
                      col_names = c("Arrival Port/ Marina",	"Arrival_Date", "Departure_Date",
                                    "Duration of stay",	"Ano",	"Factor",	"Realname",	"Code",
                                    "Lat",	"Long",	"Realname_Nextport",	"Code_","Lat_", "Long_",
                                    "Nacionality", "Vlookup type2"	,"Vessel_Length (m)",
                                    "Vessel name"	,"Procv Tonnage","Ton"))


## ---------------------------

## clean data

data.df <- data.df[!is.na(data.df$Lat), ] # remove na values
data.df <- data.df[!is.na(data.df$Long), ]
data.df <- data.df %>% filter(Lat!=0 & Long !=0)
data.df$Lat <- as.numeric(data.df$Lat)
data.df$Long <- as.numeric(data.df$Long)

# arrange dataset to include total number of flights and per period

data.df.gr <- data.df %>% group_by(Realname) %>% summarise(Long=max(Long), Lat=max(Lat), n=n()) # total

data.df.gr.factor <- data.df %>% group_by(Realname, Factor) %>% summarise(Long=max(Long), Lat=max(Lat), n=n()) # per period

# convert to sf and save as shapefile

ports.sf <- st_as_sf(data.df.gr.factor, coords = c("Long", "Lat"))
st_write(ports.sf, "output/ports.shp")

# get Madeira coordinates

if(file.exists('output/centroid.shp')) {
  mad.sf <- st_read('output/centroid.shp')
  mad.sp <- as_Spatial(mad.sf)
} else {
  mad <- osm_geocode("Madeira, Portugal", key = "IYhtcCOF5VHbcpRwv7stb5yhFKgAkwZB") # api key from mapquest
  mad[c("lat", "lon")]
  mad.sf <- st_as_sf(mad,  coords = c("lon", "lat"), crs = 4326)
  mad.sp <- as(mad.sf, "Spatial")
  st_write(mad.sf, 'output/centroid.shp')
}

## bind madeira coordinates to data frame

data.df.gr <- cbind(data.df.gr,
                    data.frame(end_lon=rep(mad.sp@coords[1], dim(data.df.gr)[1]), end_lat=rep(mad.sp@coords[2], dim(data.df.gr)[1])))

data.df.gr.factor <- bind_cols(data.df.gr.factor,
                               data.frame(end_lon=rep(mad.sp@coords[1], dim(data.df.gr.factor)[1]),
                                          end_lat=rep(mad.sp@coords[2], dim(data.df.gr.factor)[1]))) # https://rdrr.io/cran/dplyr/man/bind.html

## ---------------------------

data(wrld_simpl)

shp <- wrld_simpl
r <- raster(res=0.1)
r <-rasterize(shp, r, progress='text')
worldExtentRaster <- r


## Cost surfaces / Resistance surfaces / Conductance


# Simple example # https://www.r-bloggers.com/computing-maritime-routes-in-r/

#make all sea = -999

r[is.na(r)] <- -999

#this turns all landmass to missing
r[r>-999] <- NA

#assign unit cost to all grid cells in water

r[r==-999] <- 1

r <- raster::merge(r,r_rivers50.filtered)

#this turns all landmass to missing

r[is.na(r)] <- 100000


# LatSurface[is.na(LatSurface[])] <- 1

r <- aggregate(r, fact=2)

# crop optional

e1 <- extent(-150, 170,-52,0)
e2 <- extent(-150, 170, 0, 75)
crop1 <- crop(r, e1)
crop2 <- crop(r, e2)
r <- raster::merge(crop1, crop2)

r[r<100000] <- 0.00001

sP <- cbind(c(-16.97253, 55), c(32.75176, -50))

if(file.exists('output/pathShortest_01.rds')) {
  
  pathShortest <- readRDS('output/pathShortest_01.rds')
  
} else {
  
  tr <- transition(1/r, max, directions = 16)
  tr <- geoCorrection(tr, "c")
  
  pathShortest <- apply(data.df.gr[,c("Long", "Lat")], 1, function(x) shortestPath(tr, x, sP[1,], output="SpatialLines") )
  
  saveRDS(pathShortest, 'output/pathShortest_01.rds')
}



# Inspect

# Fix  dateline

merged.lines <- do.call(rbind, pathShortest)
class(merged.lines)
length(merged.lines)
plot(merged.lines, col=1:3)

merged.lines.sf <- st_as_sfc(merged.lines)

merged.lines.sf <- merged.lines.sf %>%
  st_segmentize(units::set_units(100, km))

# merged.lines.sf <- merged.lines.sf %>%
#   mutate(geometry = (geometry + c(180,90)) %% c(360) - c(180,90))
# 
# merged.lines.sf <- merged.lines.sf %>%
#   st_wrap_dateline(options = c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"), quiet = TRUE) %>%
#   sf::st_sf(crs = 4326)

## plot

ggplot() +
  geom_sf(data = merged.lines.sf) +
  theme_minimal()+
  coord_sf(crs = 4326)

ggplot() +
  geom_sf(data = st_as_sfc(wrld_simpl)) +
  geom_sf(data = merged.lines.sf) +
  theme_minimal()+
  coord_sf(crs = 54030)


#######################################################

source('utils.R')
# background layer (ocean and graticules)

bb <- make_bbox(c(-180, 180), c(-90, 90), spacing = c(NA, 0.1), proj = projection(mad.sp))

grat <- make_graticules(seq(-150, 180, 30), seq(-90, 90, 30),
                        spacing = c(10, 1), proj = projection(mad.sp))

grat.sf <- st_as_sfc(grat)
bb.sf <- st_as_sfc(bb)


## Set breaks and build viridis palette

my_breaks  <- scales::pretty_breaks(10)(x = data.df.gr$n)
my_breaks <-c(1, 10, 100, 500, 1000, 5000, 10000)

pal <- (viridis(10))

##

data.df.gr.sf <- (sf::st_bind_cols(data.df.gr,merged.lines.sf))

## Conver ports to sf

data.df.gr.pt <- data.df.gr %>%
  dplyr::select(Long, Lat) %>%
  purrr::transpose() %>%
  purrr::map(~ matrix(flatten_dbl(.), nrow = 1, byrow = TRUE)) %>%
  purrr::map(st_point) %>%
  st_sfc(crs = 4326) %>%
  st_sf(geometry = .)%>%
  bind_cols(data.df.gr) %>%
  dplyr::select(everything(), geometry)

## Group by port

data.df.gr.pt <- data.df.gr.pt %>% group_by(Realname) %>% summarise(Long=max(Long), Lat=max(Lat), ncount=n())


n<-0

for (i in 1:length(data.df.gr.sf$geometry)) {
  
  if(st_length(data.df.gr.sf[i, ]) == units::as_units(0,value = "m")) {
    print(i)
    n <- n + 1
  } else {
    print(i)
    print(st_geometry(data.df.gr.sf[i, ]))
    data.df.gr.sf[i, ]$geometry <- st_geometry(smooth(data.df.gr.sf[i, ], method = "ksmooth", smoothness=10))
    print(st_geometry(data.df.gr.sf[i, ]))    
  }
  
}

## plot

p <- data.df.gr.sf %>%
  arrange((n))  %>%
  ggplot() +
  # borders()+
  geom_sf(data = bb.sf,
          fill = "light blue", color = NA) +
  geom_sf(data = grat.sf,
          color = "grey60", size = 0.25) +
  geom_sf(data = st_as_sfc(wrld_simpl), size = .2, fill = "grey80", col = "gray60") +
  geom_sf(aes(col = n), size=0.5)+
  geom_sf(data=data.df.gr.pt, shape = 19, size = .1) +
  scale_color_gradientn(name = "Number of entries",
                        trans="log10",
                        breaks=my_breaks,
                        labels=my_breaks, colours=pal) +
  # scale_color_gradientn(name = "Ships arriving to Madeira Island (whole period A - E) \n ",
  #                       trans="log10",
  #                       breaks=my_breaks,
  #                       labels=my_breaks, colours=pal) +
  # scale_color_gradientn(name = "Vessel Trips to Madeira Island (whole period A - E) \n ",
  #                       trans="log10",
  #                       breaks=my_breaks,
  #                       labels=my_breaks, colours=pal) +
  # scale_color_viridis(alpha = 0.1, begin = 0, end = 1, direction = 1,
  #                     discrete = FALSE, option = "D",  trans="log10", breaks=my_breaks, labels=my_breaks) +
  guides(color = guide_colorbar(nbin = 256, title.position = "top",
                                title.hjust = 0.5,
                                size=7,
                                barwidth = unit(30, "lines"), barheight = unit(1, "lines"))) +
  theme_minimal()+
  coord_sf(crs = 54030) +
  theme(text = element_text(family = "Helvetica"),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        # position legend within px1lot
        # legend.position = c(0.5, 0.13),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20", linetype = "blank"),
        legend.title = element_text(size = 14, lineheight = 0.1),
        # remove axes
        axis.line = element_blank(),
        legend.margin=margin(t=-0.3, r=0, b=-0.2, l=0, unit="cm"),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
        labs(x=NULL, y=NULL)


#r <- ggdraw(p) + draw_plot_label("A - E", y = .975, x=0.435)



r <- grid.arrange(textGrob("Period A-E: 1936 - 2004",
                           gp = gpar(fontsize = 12, fontface = "bold"),vjust=5),p,heights = c(0.1, 1))

r


ggsave(filename=paste0("output/map_", "total", ".pdf"), 
       plot = r,
       width = 170,
       device = cairo_pdf,
       dpi = 600,
       units = "mm")

ggsave(filename=paste0("output/map_", "total", ".tiff"), 
       plot = r,
       dpi=600,
       width = 170,
       units = "mm")


## zoomed

z <- data.df.gr.sf %>%
  arrange((n))  %>%
  ggplot() +
  # borders()+
  geom_sf(data = bb.sf,
          fill = "light blue", color = NA) +
  geom_sf(data = grat.sf,
          color = "grey60", size = 0.1) +
  geom_sf(data = st_as_sfc(wrld_simpl), size = .2, fill = "grey80", col = "gray60") +
  geom_sf(aes(col = n), size=1)+
  geom_sf(data=data.df.gr.pt, shape = 19, size = .5) +
  scale_color_gradientn(name = "",
                        trans="log10",
                        breaks=my_breaks,
                        labels=my_breaks, colours=pal) +
  # scale_color_gradientn(name = "Ships arriving to Madeira Island (whole period A - E) \n ",
  #                       trans="log10",
  #                       breaks=my_breaks,
  #                       labels=my_breaks, colours=pal) +
  # scale_color_gradientn(name = "Vessel Trips to Madeira Island (whole period A - E) \n ",
  #                       trans="log10",
  #                       breaks=my_breaks,
  #                       labels=my_breaks, colours=pal) +
  # scale_color_viridis(alpha = 0.1, begin = 0, end = 1, direction = 1,
  #                     discrete = FALSE, option = "D",  trans="log10", breaks=my_breaks, labels=my_breaks) +
  guides(color = guide_colorbar(nbin = 256, title.position = "top",
                                title.hjust = 0.5,
                                size=7,
                                barwidth = unit(30, "lines"), barheight = unit(1, "lines"))) +
  theme_minimal()+
  coord_sf(crs = 54030) +
  theme(text = element_text(family = "Helvetica"),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        # position legend within px1lot
        # legend.position = c(0.5, 0.13),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20", linetype = "blank"),
        legend.title = element_text(size = 14, lineheight = 0.1),
        # remove axes
        axis.line = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank())

z <- z +  coord_sf(crs = 54030, xlim = c(-6898736,1155136), ylim = c(1675019, 6976942))
plot(z)
zz <- ggdraw(z) + draw_plot_label(" Period A - E: 1936 - 2004", y = 0.975, x = -0.08, size=12)

ggsave(filename=paste0("output/map_", "total_zoomed", ".pdf"), 
      plot = zz,
      device = cairo_pdf,
      dpi=600,
      units = "mm")

ggsave(filename=paste0("output/map_", "total_zoomed", ".tiff"), 
       plot = zz,
       dpi=600,
       units = "mm")

## Every period

data.df.gr.factor <- data.df %>% group_by(Realname, Factor) %>% summarise(Long=max(Long), Lat=max(Lat), n=n())

## Bind geometry previously calculated

data.df.gr.factor <- data.df.gr.sf %>% dplyr::select(Realname, geometry) %>% full_join(data.df.gr.factor,  by = "Realname") %>% st_sf()


## add labels

periods <- data.frame(Factor=c("A", "B", "C", "D", "E"), 
           period_verbose=c("Period A: 1936 - 1939", "Period B: 1940 - 1973", "Period C: 1974 – 1989",
                            "Period D: 1990 – 1996", "Period E: 1997 – 2004"))

data.df.gr.factor <- left_join(data.df.gr.factor, periods, by = "Factor")

##  plot



# do loop per factor period

my_breaks <-c(1, 10, 100, 500, 1000, 2000, 3000)
plot_period <- function(period) {
  
  pp <- data.df.gr.factor %>% 
    dplyr::filter(Factor==period) %>%
    arrange((n)) %>%
    ggplot() +
    # borders()+
    geom_sf(data = bb.sf,
            fill = "light blue", color = NA) +
    geom_sf(data = grat.sf, 
            color = "grey60", size = 0.1) +
    geom_sf(data = st_as_sfc(wrld_simpl), size = .2, fill = "grey80", col = "gray60") +
    geom_sf(aes(col = n), size=0.5)+
    geom_sf(data=data.df.gr.pt, shape = 19, size = .1) + 
    scale_color_gradientn(name = "Number of entries",
                          trans="log10",
                          breaks=my_breaks,
                          labels=my_breaks, colours=pal) +
    # scale_color_gradientn(name = paste0("Ships arriving to Madeira Island (period ", period, ")"),
    #                       trans="log10",
    #                       breaks=my_breaks,
    #                       labels=my_breaks, colours=pal) +
    # scale_color_gradientn(name = paste0("Vessel Trips to Madeira Island (period ", period, ")"),
    #                       trans="log10",
    #                       breaks=my_breaks,
    #                       labels=my_breaks, colours=pal) +
    # scale_color_viridis(alpha = 0.1, begin = 0, end = 1, direction = 1,
    #                     discrete = FALSE, option = "D",  trans="log10", breaks=my_breaks, labels=my_breaks) +
    guides(color = guide_colorbar(nbin = 256, title.position = "top", 
                                  title.hjust = 0.5,
                                  size=7,
                                  barwidth = unit(30, "lines"), barheight = unit(1, "lines"))) +
    theme_minimal()+
    coord_sf(crs = 54030) +
    theme(text = element_text(family = "Helvetica"),
          plot.margin = unit(c(0, 0, 0, 0), "lines"),
          # position legend within px1lot
          # legend.position = c(0.5, 0.13),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.background = element_rect(color = "grey20", linetype = "blank"),
          legend.title = element_text(size = 14, lineheight = 0.1),
          # remove axes
          axis.line = element_blank(),
          legend.margin=margin(t=-0.3, r=0, b=-0.2, l=0, unit="cm"),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank()) +
    labs(x=NULL, y=NULL)
  
  period_label <- as.character(unique(data.df.gr.factor$period_verbose[data.df.gr.factor$Factor==period])) # get label 
  
  s <- grid.arrange(textGrob(period_label,
                             gp = gpar(fontsize = 12, fontface = "bold"),vjust=5),pp,heights = c(0.1, 1))
  
  s
  
  
  ggsave(filename=paste0("output/map_", period, ".pdf"), 
         plot = s,
         width = 170,
         device = cairo_pdf,
         dpi = 600,
         units = "mm")
  
  ggsave(filename=paste0("output/map_", period, ".tiff"), 
         plot = s,
         dpi=600,
         width = 170,
         units = "mm")
  
  
  # zoomed
  
  pp <- data.df.gr.factor %>% 
    dplyr::filter(Factor==period) %>%
    arrange((n)) %>%
    ggplot() +
    # borders()+
    geom_sf(data = bb.sf,
            fill = "light blue", color = NA) +
    geom_sf(data = grat.sf, 
            color = "grey60", size = 0.1) +
    geom_sf(data = st_as_sfc(wrld_simpl), size = .2, fill = "grey80", col = "gray60") +
    geom_sf(aes(col = n), size=1)+
    geom_sf(data=data.df.gr.pt, shape = 19, size = .5) +
    scale_color_gradientn(name = paste0(""),
                          trans="log10",
                          breaks=my_breaks,
                          labels=my_breaks, colours=pal) +
    guides(color = guide_colorbar(nbin = 256, title.position = "top", 
                                  title.hjust = 0.5,
                                  size=7,
                                  barwidth = unit(30, "lines"), barheight = unit(1, "lines"))) +
    theme_minimal()+
    coord_sf(crs = 54030) +
    theme(text = element_text(family = "Helvetica"),
          plot.margin = unit(c(0, 0, 0, 0), "lines"),
          # position legend within px1lot
          # legend.position = c(0.5, 0.13),
          # legend.position = c(0.5, 0.05),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.background = element_rect(color = "grey20", linetype = "blank"),
          legend.title = element_text(size = 14, lineheight = 0.1),
          # remove axes
          axis.line = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank())

  pp <- pp + coord_sf(crs = 54030, xlim = c(-6898736,1155136), ylim = c(1675019, 6976942))
  
  s <- ggdraw(pp) + draw_plot_label(period_label, y = 0.975, x = -0.08, size=12)
  
  
  #s <- ggdraw(pp) + draw_plot_label(period,  y = 0.975, x = 0)
  
  ggsave(filename=paste0("output/map_", period, "_zoomed", ".pdf"), 
         plot = s,
         device = cairo_pdf,
         dpi = 600,
         units = "mm")
  
  ggsave(filename=paste0("output/map_", period, "_zoomed", ".tiff"), 
         plot = s,
         dpi=600,
         units = "mm")
}


# Plot maps by period

period <- c("A","B", "C", "D", "E")

lapply(period, plot_period)