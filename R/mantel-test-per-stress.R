

source("./R/desiccation_and_starvation_data_import_and_cleaning.R")
source("./R/wing_data_import.R")

wing.len.median.g2 <- wing.len.median %>% filter(gen == "G2") %>% select("pop", "mwing")

corr.trt.wild <- heat.wild.1.s[[2]] %>% 
  dplyr::select("pop", "pmedian") %>%
  left_join(cold.wild.1.s[[2]], by =c("pop")) %>% 
  dplyr::rename(heat = "pmedian.x") %>%
  dplyr::rename(cold = "pmedian.y") %>%
  left_join(desiccant.wild.1.s[[2]], by = c("pop")) %>% 
  dplyr::rename(des = "pmedian") %>% 
  left_join(starvation.wild.1.s[[2]], by = c("pop")) %>%
  dplyr::rename(star = "pmedian") %>%
  left_join(wing.len.median.g2, by = c("pop")) %>%
  dplyr::rename(wing = "mwing") %>%
  dplyr::select("heat", "cold", "des", "star", "wing")



oldnames <- as.character(seq(1,12,1))
newnames <- heat.wild.1.s[[2]]$pop

geo.dist.raw <-   heat.wild.1.s[[2]] %>% select("lat", "lon") %>% dist( upper = T) 
geo.dist <-   geo.dist.raw %>%
  broom::tidy(geo.dist, diagonal = TRUE, upper = TRUE) %>%
  tidyr::spread(item2, distance)
geo.dist$item1 <- newnames
names(geo.dist) = c(" ", "Alice Springs", "Darwin", "Sydney", "Batemans Bay", "Bega Valley", 
                          "Canberra", "Griffith","Mareeba", "Brisbane", "Cape Tribulation", "Narrabri", "Utchee Creek")


# Calculate mantel test for different stress:


heat.dist <- dist(corr.trt.wild$heat)
des.dist <- dist(corr.trt.wild$des)
star.dist <- dist(corr.trt.wild$star)
wing.dist <- dist(corr.trt.wild$wing)

mantel(geo.dist.raw, heat.dist)
mantel(geo.dist.raw, des.dist)
mantel(geo.dist.raw, star.dist)
mantel(geo.dist.raw, wing.dist)

