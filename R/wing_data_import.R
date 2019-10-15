
library("dplyr"); library("ggplot2"); library("emmeans")
source("R/desiccation_and_starvation_data_import_and_cleaning.R") 


#---
# Import data and merge datasets ----
#---

wing.data <- read_csv("./data/wings/wing_data.csv", col_names = TRUE)

labels.wings <- data.frame(pop = c("Alice Springs", "Batemans Bay", "Bega Valley", 
                                   "Brisbane", "Canberra", "Cape Tribulation", 
                                   "Darwin", "Griffith", "Mareeba", 
                                   "Narrabri", "Sydney", "Utchee Creek"), 
                           labs = c(NA, "BB", NA, NA, NA, NA, 
                                    NA, NA, "MA", NA, "SY", NA),
                           lab = c("AS", "BB", "BV", "BR", "CA", "CT", 
                                    "DA", "GR", "MA", "NA", "SY", "UC"),
                           stringsAsFactors = FALSE)


wing.len <- wing.data %>% mutate(wing = round(wing, 2)) %>% 
  mutate(gen = factor(gen, levels = c("G2", "G11"))) %>%
  left_join(labels.wings, by = "pop")

wing.len.g2  <- wing.len %>% filter(gen == "G2")
wing.len.g11 <- wing.len %>% filter(gen == "G11")




wing.len.median <- wing.len %>% 
  dplyr::group_by(pop, gen) %>%
  dplyr::mutate(mwing = round(median(wing),2)) %>% 
  dplyr::ungroup() %>%
  dplyr::select(pop, labs, gen, mwing) %>%
  dplyr::distinct() 


wing.des <- desiccation.1 %>%
  dplyr::filter(gen %in% c("G2", "G10", "G11")) %>%
  dplyr::filter(sex %in% c("Male")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(gen = case_when(gen == "G2" ~ "G2", gen == "G10" ~ "G11", gen == "G11" ~ "G11")) %>%
  dplyr::mutate(gen =  factor(gen, levels = c("G2", "G11"))) %>%
  dplyr::mutate(trt = case_when(trt == "Desiccant" ~ "Desiccation", trt == "Control" ~ "Starvation")) %>%
  dplyr::select(pop, trt, pmedian, gen, lat) %>% 
  dplyr::group_by(pop, trt, gen) %>%
  dplyr::mutate(pmedian = round(mean(pmedian),2)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::left_join(wing.len.median, by = c("pop", "gen")) %>%
  dplyr::select("pop", "labs","trt", "gen", "lat",  "pmedian", "mwing")


wing.des.gen <- wing.des %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(gen) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x))

wing.des.gen.stats <- wing.des %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(gen, trt) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x))


changes.des <- wing.des %>% 
  dplyr::filter(!pop %in% c("Bega Valley", "Canberra", "Cape Tribulation", "Darwin")) %>%
  nest(pmedian, mwing, .key = 'value_col') %>% 
  spread(key = gen, value = value_col) %>% 
  unnest(G2, G11, .sep = '_') %>% 
  dplyr::group_by(pop, trt) %>%
  dplyr::mutate(Wing_change = round(G11_mwing/G2_mwing,2), Des_change = round(G11_pmedian/G2_pmedian, 2)) %>%
  dplyr::select(pop, labs, trt, Des_change, Wing_change)

changes.des.1 <- changes.des %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(trt) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x))  # 1. G11-Control; 2. G2-Desiccant; 3. G11-Control, 4. G11-Desiccant 

#---
# Correlation with heat data ----
#---

heat.domes.2$status <- factor(heat.domes.2$status, levels = c("Wild", "Domesticated"))

wing.heat <- heat.domes.2 %>%
  dplyr::ungroup() %>%
  dplyr::mutate(gen = case_when(status == "Wild" ~ "G2", status == "Domesticated" ~ "G11")) %>%
  dplyr::mutate(gen =  factor(gen, levels = c("G2", "G11"))) %>%
  dplyr::select(pop, gen, lat, pmedian) %>% 
  dplyr::group_by(pop, gen) %>%
  dplyr::mutate(pmedian = round(mean(pmedian),2)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::left_join(wing.len.median, by = c("pop", "gen")) %>%
  dplyr::select("pop", "labs","gen", "lat",  "pmedian", "mwing")


wing.heat.gen <- wing.heat %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(gen) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x))


changes.heat <- wing.heat %>% 
  dplyr::filter(!pop %in% c("Bega Valley", "Canberra", "Cape Tribulation", "Darwin")) %>%
  nest(pmedian, mwing, .key = 'value_col') %>% 
  spread(key = gen, value = value_col) %>% 
  unnest(G2, G11, .sep = '_') %>% 
  dplyr::group_by(pop) %>%
  dplyr::mutate(Wing_change = round(G11_mwing/G2_mwing,2), Heat_change = round(G11_pmedian/G2_pmedian, 2)) %>%
  dplyr::select(pop, labs, Heat_change, Wing_change)

#---
# Wing measurements data ----
#---

wings <- wing.des %>% 
  dplyr::select(pop, labs, gen, lat, mwing) %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(gen) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x))

wings.change <- wing.des %>% 
  dplyr::select(pop, labs, gen, lat, mwing) %>%
  dplyr::distinct() %>%
  dplyr::filter(!pop %in% c("Bega Valley", "Canberra", "Cape Tribulation", "Darwin")) %>%
  spread(key = gen, value = mwing) %>% 
  dplyr::group_by(pop) %>%
  dplyr::mutate(Wing_change = round(G11/G2,2)) %>%
  dplyr::select(pop, labs, lat, Wing_change)


#---
# Statistical analysis ----
#---

wing.lm.1 <- lm(wing ~ pop * gen, data = wing.len)
wing.lm.2 <- lm(wing ~ pop, data = wing.len.g2)
wing.lm.3 <- lm(wing ~ pop, data = wing.len.g11)


anova(wing.lm.1, test = "Chisq")
anova(wing.lm.2, test = "Chisq")
anova(wing.lm.3, test = "Chisq")

emmeans(wing.lm.1, pairwise ~ pop)
emmeans(wing.lm.2, pairwise ~ pop)
emmeans(wing.lm.3, pairwise ~ pop)

#---
# Models for annotation ----
#---

lm.des.1 <- lm(wing.des.gen.stats[[1]]$pmedian ~ wing.des.gen.stats[[1]]$mwing)
lm.des.2 <- lm(wing.des.gen.stats[[2]]$pmedian ~ wing.des.gen.stats[[2]]$mwing)
lm.des.3 <- lm(wing.des.gen.stats[[3]]$pmedian ~ wing.des.gen.stats[[3]]$mwing)
lm.des.4 <- lm(wing.des.gen.stats[[4]]$pmedian ~ wing.des.gen.stats[[4]]$mwing)

lm.des.5 <- lm(changes.des.1[[1]]$Des_change ~ changes.des.1[[1]]$Wing_change)
lm.des.6 <- lm(changes.des.1[[2]]$Des_change ~ changes.des.1[[2]]$Wing_change)

lm.heat.1 <- lm(wing.heat.gen[[1]]$pmedian ~ wing.heat.gen[[1]]$mwing)
lm.heat.2 <- lm(wing.heat.gen[[2]]$pmedian ~ wing.heat.gen[[2]]$mwing)
lm.heat.3 <- lm(changes.heat$Heat_change ~ changes.heat$Wing_change)

lm.wing.1 <- lm(wings[[1]]$mwing ~ wings[[1]]$lat)
lm.wing.2 <- lm(wings[[2]]$mwing ~ wings[[2]]$lat)

lm.wing.3 <- lm(wings.change$Wing_change ~ wings.change$lat)

#---
# Remove list with files and raw input datasets ----
#---

rm(AS_G2, AS_G11, BV_G2, BB_G2, BB_G11, BR_G2, BR_G11, CB_G2, CT_G2, DW_G2, DW_G11, 
   GR_G2, GR_G11, MB_G2, MB_G11, NB_G2, NB_G11, SY_G2, SY_G11, UC_G2, UC_G11, 
   myfiles_1, myfiles_2, myfiles_3, myfiles_4, myfiles_5, myfiles_6, myfiles_7, myfiles_8, myfiles_9,
   myfiles_10, myfiles_11, myfiles_12, myfiles_13, myfiles_14, myfiles_15, myfiles_16, myfiles_17, myfiles_18,
   myfiles_19, myfiles_20, myfiles_21)

