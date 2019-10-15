#---
# load data for figures ----
#---

library("ggrepel")
library("tidyverse")
library("cowplot")

#---
# Plot with regression lines ----
#---

pcor_heat_1_r <- ggplot(wing.heat.gen[[1]], aes(y = pmedian, x = mwing)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") + 
  ylab("Median knockdown time (min)") +
  xlab("Wing length (mm)") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 12, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = wing.heat.gen[[1]]$labs), force = 2) + 
  ggtitle("Wild populations")


pcor_heat_2_r <- ggplot(wing.heat.gen[[2]], aes(y = pmedian, x = mwing)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") +
  ylab("Median knockdown time (min)") +
  xlab("Wing length (mm)") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 12, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = wing.heat.gen[[2]]$labs), force = 2) + 
  ggtitle("Domesticated populations") 

pcor_heat_3_r <- ggplot(changes.heat, aes(y = Heat_change , x = Wing_change)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") +
  ylab("Change in tolerance (Domesticated/Wild)") +
  xlab("Change in wing length (Domesticated/Wild)") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 10), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 12, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = changes.heat$labs), force = 2) + 
  ggtitle("Domesticated populations/Wild populations")

pcor_des_1_r <- ggplot(wing.des.gen.stats[[1]], aes(y = pmedian, x = mwing)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") +
  ylab("Median survival time (hrs)") +
  xlab("Wing length (mm)") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 12, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = wing.des.gen.stats[[1]]$labs), force = 2) + 
  ggtitle("Wild populations") 


pcor_des_2_r <- ggplot(wing.des.gen.stats[[3]], aes(y = pmedian, x = mwing)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") +
  ylab("Median survival time (hrs)") +
  xlab("Wing length (mm)") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 12, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = wing.des.gen.stats[[3]]$labs), force = 2) + 
  ggtitle("Domesticated populations") 

pcor_des_3_r <- ggplot(changes.des.1[[1]], aes(y = Des_change , x = Wing_change)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") +
  ylab("Change in tolerance (Domesticated/Wild)") +
  xlab("Change in wing length (Domesticated/Wild)") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 10), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 12, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = changes.des.1[[1]]$labs), force = 2) + 
  ggtitle("Domesticated populations/Wild populations") 


pcor_stv_1_r <- ggplot(wing.des.gen.stats[[2]], aes(y = pmedian, x = mwing)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") +
  ylab("Median survival time (hrs)") +
  xlab("Wing length (mm)") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 12, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = wing.des.gen.stats[[2]]$labs), force = 2) + 
  ggtitle("Wild populations") 

pcor_stv_2_r <- ggplot(wing.des.gen.stats[[4]], aes(y = pmedian, x = mwing)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") +
  ylab("Median survival time (hrs)") +
  xlab("Wing length (mm)") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 12, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = wing.des.gen.stats[[4]]$labs), force = 2) + 
  ggtitle("Domesticated populations")


pcor_stv_3_r <- ggplot(changes.des.1[[2]], aes(y = Des_change , x = Wing_change)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") +
  ylab("Change in tolerance (Domesticated/Wild)") +
  xlab("Change in wing length (Domesticated/Wild)") + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 10), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 12, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = changes.des.1[[2]]$labs), force = 2) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour = "black", size = 0.5) +
  ggtitle("Domesticated populations/Wild populations")


title.des <- ggdraw() + draw_label("Desiccation tolerance", fontface = 'bold', x = 0.5, hjust = 0.35)
pdes <- plot_grid(title.des, #NULL, 
                  pcor_des_1_r + theme(legend.position = "none"),
                  #NULL,
                  pcor_des_2_r + theme(legend.position = "none"),
                  #NULL,
                  pcor_des_3_r + theme(legend.position = "none"), 
                  ncol = 1, align = "v", axis = "r", 
                  #rel_heights = c(0.1, 1, 0.1, 1, 0.1, 1))
                  rel_heights = c(0.05, 1, 1, 1))


title.stv <- ggdraw() + draw_label("Starvation tolerance", fontface = 'bold', x = 0.5, hjust = 0.35)
pstv <- plot_grid(title.stv, #NULL,
                  pcor_stv_1_r + theme(legend.position = "none"),
                  #NULL,
                  pcor_stv_2_r + theme(legend.position = "none"),
                  #NULL,
                  pcor_stv_3_r + theme(legend.position = "none"), 
                  ncol = 1, align = "v", axis = "r",
                  #rel_heights = c(0.1, 1, 0.1, 1, 0.1, 1))
                  rel_heights = c(0.05, 1, 1, 1))

title.heat <- ggdraw() + draw_label("Heat tolerance", fontface = 'bold', x = 0.5, hjust = 0.35)
pheat <- plot_grid(title.heat, #NULL,
                   pcor_heat_1_r + theme(legend.position = "none"),
                   #NULL,
                   pcor_heat_2_r + theme(legend.position = "none"),
                   #NULL,
                   pcor_heat_3_r + theme(legend.position = "none"),
                   ncol = 1, align = "v", axis = "r",
                   #rel_heights = c(0.1, 1, 0.1, 1, 0.1, 1))
                   rel_heights = c(0.05, 1, 1, 1))



legend_wing <- get_legend(pcor_des_1_r + theme(legend.position="bottom"))


pcomb.wing <- plot_grid(pheat, NULL, pdes, NULL, pstv,
                        ncol = 5, align = "h", axis = "r", 
                        labels = c("", "", ""), #c("Heat tolerance", " ","Desiccation tolerance", " ", "Starvation tolerance"), 
                        label_x = 0.55,
                        hjust = 0.5,
                        rel_widths = c(1, 0.1, 1, 0.1, 1))
title <- ggdraw() + 
  draw_label(
    "Fig 7",
    fontface = 'bold',
    x = 0,
    hjust = 0) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7))


pwing <- plot_grid(pcomb.wing, legend_wing, ncol = 1, rel_heights = c(0.99, .1))

ggsave(plot = pwing, "./fig/Fig8.svg", width = 18, height = 12)

