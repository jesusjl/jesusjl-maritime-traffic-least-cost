# Load libraries

library(readxl)
library(ggalt)
library(sp)
library(sf)
library(ggmap)
library(dplyr)
library(raster)
library(purrr)
library(viridis)
library(units)
library(cowplot)
library(mapdata)

# Read data 

data.df <- read_excel(path  = 'maritime_flow.xlsx', sheet = 1, skip = 1,col_names = c("Arrival Port/ Marina",	"Arrival_Date", "Departure_Date",
"Duration of stay",	"Ano",	"Factor",	"Realname",	"Code",	"Lat",	"Long",	"Realname_Nextport",	"Code_","Lat_", "Long_", "Nacionality",	
"Vlookup type2"	,"Vessel_Length (m)"	,"Vessel name"	,"Procv Tonnage","Ton"))

data.df <- data.df[!is.na(data.df$Lat), ] # remove na values
data.df <- data.df[!is.na(data.df$Long), ]

data.df <- data.df %>% filter(Lat!=0 & Long !=0)

data.df$Lat <- as.numeric(data.df$Lat)
data.df$Long <- as.numeric(data.df$Long)

# Group by Realname and Realname & Factor

data.df.gr <- data.df %>% group_by(Realname) %>% summarise(Long=max(Long), Lat=max(Lat), n=n())

data.df.gr.factor <- data.df %>% group_by(Realname, Factor) %>% summarise(Long=max(Long), Lat=max(Lat), n=n())

# Get Madeira coordinates

mad <- geocode("Madeira", source = "dsk")
mad.sf <- st_as_sf(mad,  coords = c("lon", "lat"), crs = 4326)
mad.sp <- as(mad.sf, "Spatial")

# Load functions # http://strimas.com/spatial/long-flights/

source('utils.R')

# background layer (ocean and graticules)

bb <- make_bbox(c(-180, 180), c(-90, 90), spacing = c(NA, 0.1), proj = projection(mad.sp))

grat <- make_graticules(seq(-150, 180, 30), seq(-90, 90, 30), 
                        spacing = c(10, 1), proj = projection(mad.sp))

grat.sf <- st_as_sfc(grat)
bb.sf <- st_as_sfc(bb)

## Bind madeira coordinates to data frame

data.df.gr <- cbind(data.df.gr, data.frame(end_lon=rep(mad.sp@coords[1], dim(data.df.gr)[1])), end_lat=rep(mad.sp@coords[2], dim(data.df.gr)[1]))

## Convert maritime flow data to sf and plot

data.df.gr <- data.df.gr %>%
  dplyr::select(Long, Lat, end_lon, end_lat) %>% 
  purrr::transpose() %>% 
  purrr::map(~ matrix(flatten_dbl(.), nrow = 2, byrow = TRUE)) %>% 
  purrr::map(st_linestring) %>%
  st_sfc(crs = 4326) %>%
  st_sf(geometry = .)%>%
  bind_cols(data.df.gr) %>%
  dplyr::select(everything(), geometry)

data.df.gr %>% 
  ggplot() +
  borders()+
  geom_sf(col = "blue")+
  borders()+
  theme_minimal()+
  coord_sf(datum = NA)


## Great circles distance accounting for Dateline

data.df.gr <- data.df.gr %>% 
  st_segmentize(units::set_units(100, km)) 

data.df.gr <- data.df.gr %>% 
  mutate(geometry = (geometry + c(180,90)) %% c(360) - c(180,90))

data.df.gr <- data.df.gr %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"), quiet = TRUE) %>% 
  sf::st_sf(crs = 4326)

## Conver ports to sf 

data.df.gr.pt <- data.df %>%
  dplyr::select(Long, Lat) %>% 
  purrr::transpose() %>% 
  purrr::map(~ matrix(flatten_dbl(.), nrow = 1, byrow = TRUE)) %>% 
  purrr::map(st_point) %>%
  st_sfc(crs = 4326) %>%
  st_sf(geometry = .)%>%
  bind_cols(data.df) %>%
  dplyr::select(everything(), geometry)

## Group by port

data.df.gr.pt <- data.df.gr.pt %>% group_by(Realname) %>% summarise(Long=max(Long), Lat=max(Lat), ncount=n())

## Load world map

world_map <- rnaturalearth::ne_countries(scale = 'small', returnclass = c("sf"))

## Set breaks and build viridis palette

my_breaks  <- scales::pretty_breaks(10)(x = data.df.gr$n)
my_breaks <-c(1, 10, 100, 500, 1000, 5000, 10000)

pal <- (viridis(10))

## plot

p <- data.df.gr %>% 
  arrange((n))  %>% 
  ggplot() +
  # borders()+
  geom_sf(data = bb.sf,
          fill = "light blue", color = NA) +
  geom_sf(data = grat.sf, 
          color = "grey60", size = 0.1) +
  geom_sf(data = world_map, size = .2, fill = "grey80", col = "gray60") +
  geom_sf(aes(col = n), size=0.3)+
  geom_sf(data=data.df.gr.pt, shape = 19, size = .1) + 
  scale_color_gradientn(name = "Vessel Trips to Madeira Island (whole period A - E) \n ",
                        trans="log10",
                        breaks=my_breaks,
                        labels=my_breaks, colours=pal) +
  # scale_color_viridis(alpha = 0.1, begin = 0, end = 1, direction = 1,
  #                     discrete = FALSE, option = "D",  trans="log10", breaks=my_breaks, labels=my_breaks) +
  guides(color = guide_colorbar(nbin = 256, title.position = "top", 
                                title.hjust = 0.5,
                                size=7,
                                barwidth = unit(45, "lines"), barheight = unit(1, "lines"))) +
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


r <- ggdraw(p) + draw_plot_label("A - E")



ggsave(filename=paste0("output/map_", "total", ".pdf"), 
       plot = r,
       device = cairo_pdf,
       width = 290,
       height = 210, 
       units = "mm")

## Every period

data.df.gr.factor <- data.df %>% group_by(Realname, Factor) %>% summarise(Long=max(Long), Lat=max(Lat), n=n())

## Bind geometry previously calculated

data.df.gr.factor <- data.df.gr %>% dplyr::select(Realname, geometry) %>% full_join(data.df.gr.factor,  by = "Realname") %>% st_sf()

##  plot

# do loop per factor period

my_breaks <-c(1, 10, 100, 500, 1000, 2000, 3000)
plot_period <- function(period) {
  
p <-  data.df.gr.factor %>% 
    dplyr::filter(Factor==period) %>%
      arrange((n))  %>% 
      ggplot() +
      # borders()+
      geom_sf(data = bb.sf,
              fill = "light blue", color = NA) +
      geom_sf(data = grat.sf, 
              color = "grey60", size = 0.1) +
      geom_sf(data = world_map, size = .2, fill = "grey80", col = "gray60") +
      geom_sf(aes(col = n), size=0.3)+
      geom_sf(data=data.df.gr.pt, shape = 19, size = .1) + 
      scale_color_gradientn(name = paste0("Vessel Trips to Madeira Island (period ", period, ")"),
                            trans="log10",
                            breaks=my_breaks,
                            labels=my_breaks, colours=pal) +
      # scale_color_viridis(alpha = 0.1, begin = 0, end = 1, direction = 1,
      #                     discrete = FALSE, option = "D",  trans="log10", breaks=my_breaks, labels=my_breaks) +
      guides(color = guide_colorbar(nbin = 256, title.position = "top", 
                                    title.hjust = 0.5,
                                    size=7,
                                    barwidth = unit(45, "lines"), barheight = unit(1, "lines"))) +
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
    
    
    r <- ggdraw(p) + draw_plot_label(period)
    
    
    
    ggsave(filename=paste0("output/map_", period, ".pdf"), 
           plot = r,
           device = cairo_pdf,
           width = 290,
           height = 210, 
           units = "mm")
    
    ggsave(filename=paste0("output/map_", period, ".jpg"), 
           plot = r)
}

# Convert to jpg

period <- c("A","B", "C", "D", "E")

lapply(period, plot_period)

library(pdftools)

map.pdf <- paste0('output/',list.files(path = 'output/', pattern = '.pdf'))

map.letters <- c("A", "B", "C", "D", "E", "total")

for (i in 1:length(map.pdf)) {
  bitmap <- pdf_render_page(map.pdf[[i]], page = 1, dpi = 300)
  jpeg::writeJPEG(bitmap, paste0("output/map_", map.letters[i], ".jpg"), quality = 1)
}