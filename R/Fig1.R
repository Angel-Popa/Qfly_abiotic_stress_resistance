


suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggrepel))

scale_fill_KG <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("NA","#0000FF", "#0078FF", "#46AAFA", "#FF0000", "#FF9696",
                        "#F5A500", "#FFDC64", "#FFFF00", "#C8C800", "#969600",
                        "#96FF96", "#64C864", "#329632", "#C8FF50", "#64FF50",
                        "#32C800", "#FF00FF", "#C800C8", "#963296", "#966496",
                        "#AAAFFF", "#5A78DC", "#4B50B4", "#320087", "#00FFFF",
                        "#37C8FF", "#007D7D", "#00465F", "#B2B2B2", "#666666"), 
                      c(0, 1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
                        11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
                        21, 22, 23, 24, 25, 26, 27, 28, 29, 30)),
    labels = setNames(c("Ocean water", 
                        "Tropical, rainforest",
                        "Tropical, monsoon",
                        "Tropical, savannah",
                        "Arid, desert, hot",
                        "Arid, desert, cold",
                        "Arid, steppe, hot",
                        "Arid, steppe, cold",
                        "Temperate, dry summer, hot summer",
                        "Temperate, dry summer, warm summer",
                        "Temperate, dry summer, cold summer",
                        "Temperate, dry winter, hot summer",
                        "Temperate, dry winter, warm summer",
                        "Temperate, dry winter, cold summer",
                        "Temperate, no dry season, hot summer",
                        "Temperate, no dry season, warm summer",
                        "Temperate, no dry season, cold summer",
                        "Cold, dry summer, hot summer",
                        "Cold, dry summer, warm summer",
                        "Cold, dry summer, cold summer",
                        "Cold, dry summer, very cold winter",
                        "Cold, dry winter, hot summer",
                        "Cold, dry winter, warm summer",
                        "Cold, dry winter, cold summer",
                        "Cold, dry winter, very cold winter",
                        "Cold, no dry season, hot summer",
                        "Cold, no dry season, warm summer",
                        "Cold, no dry season, cold summer",
                        "Cold, no dry season, very cold winter",
                        "Polar, tundra",
                        "Polar, frost"),
                      c(0, 1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
                        11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
                        21, 22, 23, 24, 25, 26, 27, 28, 29, 30)),
    
    ...
  )
}


DSM_AU <- raster("./data/Beck_KG_V1/Beck_KG_V1_present_0p0083.tif")
pop.lab <- read.csv("./data/Beck_KG_V1/Lat-and-long-pops.csv") %>% 
  mutate(Beck_KG_V1_present_0p0083 = as.character(c("#FF0000", "#64FF50", "#64FF50",
                                                    "#C8FF50", "#64FF50", "#0078FF",
                                                    "#46AAFA", "#FFDC64", "#96FF96", 
                                                    "#F5A500", "#C8FF50", "#0000FF")))
#rownames(pop.lab) <- pop.lab$pop
#xy <- select(pop.lab, "long", "lat") 
#KG <- extract(DSM_AU, xy)
#xy.KG <- cbind(xy, KG)

hatch.area <- readOGR("./data/mygeodata/hatched_area-polygon.shp")
hatched.area <- hatched.SpatialPolygons(hatch.area, fillOddEven = T, density = 4, angle = 45)


DSM_AU_df <- as.data.frame(DSM_AU, xy = TRUE) %>% dplyr::rename(long = "x", lat = "y")

DSM_AU_df_f <- DSM_AU_df %>% 
  dplyr::filter(long > 129 & long <155) %>% 
  dplyr::filter(lat > -39 & lat < -10) %>% 
  dplyr::filter(Beck_KG_V1_present_0p0083 != 0) %>% 
  dplyr::mutate(Beck_KG_V1_present_0p0083 = as.factor(Beck_KG_V1_present_0p0083)) 

fig1 <- ggplot() +
  geom_raster(data = DSM_AU_df_f , aes(x = long, y = lat, fill = Beck_KG_V1_present_0p0083)) +
  scale_fill_KG(name = paste("K","\U00F6","ppen climate classification", sep = "")) +
  coord_quickmap() + 
  theme(legend.position='none')+
  xlab("Longitude") + ylab("Latitude") + 
  geom_polygon(data = hatched.area, aes(x = long,  y = lat, group = group), colour = "gray10", inherit.aes = F) +
  geom_point(aes(x = long, y = lat), 
             alpha = 0.8, 
             size =2, 
             color = "gray20",
             data = pop.lab) +
  ggrepel::geom_label_repel(aes(x = long, y = lat, label = pop, fontface =2), 
                            size = 5,
                            force = 3,
                            data = pop.lab, 
                            fill = "gray99", 
                            alpha = 0.7,
                            segment.color = "black",
                            color = "black",
                            show.legend = FALSE, 
                            #nudge_x = 2,
                            #nudge_y = 2,
                            #direction = "x", 
                            inherit.aes = F) +
  theme_bw() + 
  theme(legend.position='right',
        legend.title = element_text(face = "bold", size = 16),
        legend.key = element_rect(colour = "black"),
        legend.text = element_text(size = 14), 
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

ggsave(fig1, filename = "./fig/fig1.svg", width = 12, height = 12)

#fig1

