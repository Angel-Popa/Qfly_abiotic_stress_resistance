
#---
# load data for figues
#---

source("R/desiccation_and_starvation_data_import_and_cleaning.R")

#---
# Figure 4: Trends over generations in the desiccation and starvation tolerance data
#---

s06.sd <- desiccation %>% 
  filter(pops == "S06") %>%
  mutate(tmedian = round(median(time), 2)) %>% 
  group_by(.dots=c("sex", "trt")) %>% 
  mutate(nemedian = round(median(time),2)) %>%
  #mutate(nemedian = ifelse(exp == 8, tmedian, round(emedian, 2))) %>% 
  ungroup() %>% 
  mutate(exp = as.factor(exp)) %>% 
  group_by(exp, sex, trt, nemedian) %>%
  summarise(SD = round(sd(time),2)) %>%
  filter(sex != "Female")
  


ggplot(s06.sd, aes(x = exp, y = nemedian, 
                                group =trt, colour = trt)) + 
  geom_point(size = 3) + 
  geom_line() +
  geom_errorbar(aes(ymin=nemedian-SD, ymax=nemedian+SD), width=.2,
                position=position_dodge(0.05)) +
  scale_color_grey() + 
  labs(colour = "Treatment") + 
  ylab("Median survival time (hrs)") +
  xlab(" ") + 
  guides(shape = FALSE) + 
  #facet_grid(. ~ pop) + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray", 
                                    fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 8, 
                                                   unit = "pt")))
title <- ggdraw() + 
  draw_label(
    "Fig 4",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

fig4 <- plot_grid(title, p4, ncol = 1, rel_heights = c(0.05, 0.94))

ggsave("fig/fig4.png", width = 12, height = 4, units = "in", dpi = 300)









