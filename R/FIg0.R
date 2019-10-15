
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)

DSM_AU <- raster("./data/BOM/Beck_KG_V1/Beck_KG_V1_present_0p0083.tif")

#DSM_AU

DSM_AU_df <- as.data.frame(DSM_AU, xy = TRUE) %>% dplyr::rename(long = "x", lat = "y")

DSM_AU_df_f <- DSM_AU_df  %>% 
  dplyr::filter(long > 110 & long < 160) %>% 
  dplyr::filter(lat > -45 & lat < -6.8)

ggplot() +
  #lims(x = c(113.7, 160), y = c(-42.5, -6.8))+
  geom_raster(data = DSM_AU_df , aes(x = long, y = lat, fill = Beck_KG_V1_present_0p0083)) +
  scale_fill_viridis_c() +
  coord_quickmap()

#levels(as.factor(DSM_AU_df$Beck_KG_V1_present_0p5))

world <- ggplot2::map_data('world') %>% mutate(long = round(long, 2), lat = round(lat, 2)) %>% 
  left_join(DSM_AU_df)


world <- world %>% 
  dplyr::filter(long > 110 & long <160) %>% 
  dplyr::filter(lat > -45 & lat < -6.8)

KG <- world$Beck_KG_V1_present_0p5

ggplot(world, aes(long, lat)) +
  lims(x = c(113.7, 160), y = c(-42.5, -6.8))+
  geom_map(map=world, aes(map_id=region), fill=KG, color="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  coord_quickmap() + 
  scale_fill_viridis_c( option = "B", na.value = "white") + 
  theme(legend.position='none')+
  xlab("Longitude") + ylab("Latitude") + 
  theme_bw() + 
  theme(legend.position='none',
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

ggplot() +
  geom_tile(data = world , aes(x = lat, y = long)) +
  #scale_fill_viridis_c( option = "B", na.value = "white") +
  coord_quickmap() + 
  theme(legend.position = "none") + 
  theme_bw()


