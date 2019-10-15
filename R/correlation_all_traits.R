

source("./R/desiccation_and_starvation_data_import_and_cleaning.R")
source("./R/wing_data_import.R")

corr.trt.domes <- heat.old.1.s[[2]] %>% dplyr::select("pop", "pmedian") %>% 
  dplyr::left_join(cold.old.1.s[[2]], by =c("pop")) %>%
  dplyr::rename(heat = "pmedian.x") %>%
  dplyr::rename(cold = "pmedian.y") %>%
  dplyr::left_join(desiccant.domes.1.s[[2]], by = c("pop")) %>% 
  dplyr::rename(des = "pmedian") %>% 
  dplyr::left_join(starvation.domes.1.s[[2]], by = c("pop")) %>%
  dplyr::rename(star = "pmedian") %>% 
  dplyr::select("heat", "cold", "des", "star", "pop")



corr.trt.wild <- heat.wild.1.s[[2]] %>% 
  dplyr::select("pop", "pmedian") %>%
  left_join(cold.wild.1.s[[2]], by =c("pop")) %>% 
  dplyr::rename(heat = "pmedian.x") %>%
  dplyr::rename(cold = "pmedian.y") %>%
  left_join(desiccant.wild.1.s[[2]], by = c("pop")) %>% 
  dplyr::rename(des = "pmedian") %>% 
  left_join(starvation.wild.1.s[[2]], by = c("pop")) %>%
  dplyr::rename(star = "pmedian") %>%
  dplyr::select("heat", "cold", "des", "star", "pop")

corr.trt <- dplyr::bind_rows(corr.trt.wild, corr.trt.domes) %>% 
  left_join(wing.len.median, by = c("pop")) %>%
  dplyr::select("heat", "cold", "des", "star", "mwing")
  

corstars(corr.trt, method = c("spearman"))

correlation_matrix<-rcorr(as.matrix(corr.trt), type="spearman")

wing.weather <- wing.len.median %>% dplyr::filter(gen == "G2") %>% 
  left_join(heat.wild.1.s[[2]], by = c("pop")) %>%
  dplyr::select("pop", "mwing", "lat", "origin") %>%
  dplyr::left_join(weather_variables, by = c("pop")) %>% 
  droplevels() %>% 
  dplyr::select("mwing", "lat", "origin", 
                "mean.max", "mean.min", 
                "mean.rain", "mean.solar", 
                "annual.temp", "max.high.temp", 
                "low.high.temp", "low.min.temp", 
                "high.min.temp", "min.dry.month") %>% 
  distinct()


step.lm.weather.wing <- step(lm(mwing ~ mean.rain + 
                                        mean.solar + 
                                        max.high.temp + 
                                        low.min.temp + 
                                        min.dry.month, 
                                      data = wing.weather), direction = "both")

cline.wing <- lm(mwing ~ lat + origin, data = wing.weather)
summary(cline.wing)

summary(step.lm.weather.wing)
