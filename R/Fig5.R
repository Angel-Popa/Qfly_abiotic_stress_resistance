
#---
# load data for figues
#---

source("R/desiccation_and_starvation_data_import_and_cleaning.R")

#---
# Figure 4: Trends over generations in the desiccation and starvation tolerance data
#---

p5 <-  ggplot(des.domes.sd, aes(x = gen, y = pmedian, 
                                group =trt, colour = trt)) + 
  geom_point(size = 3) + 
  geom_line() +
  geom_errorbar(aes(ymin=pmedian-SD, ymax=pmedian+SD), width=.2,
                position=position_dodge(0.05)) +
  scale_color_grey() + 
  labs(colour = "Treatment") + 
  ylab("Median survival time (hrs)") +
  xlab(" ") + 
  guides(shape = FALSE) + 
  facet_grid(. ~ pop) + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray", 
                                    fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 10, unit = "pt"), size =12), 
        legend.title = element_text(size = 12))
title <- ggdraw() + 
  draw_label(
    "Fig 5",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

fig5 <- plot_grid(p5, ncol = 1, rel_heights = c(1))

ggsave(fig5, filename = "fig/fig5.svg", width = 12, height = 4)
