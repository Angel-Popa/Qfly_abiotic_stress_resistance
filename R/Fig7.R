#---
# load data for figures ----
#---

library("ggrepel")
library("tidyverse")
library("cowplot")

source("./R/wing_data_import.R")

#---
# Plot wing length ----
#---

no.change <- data.frame(sex = c("Male"), change = c(1))

wings.wild <- ggplot(wings[[1]], aes(y = mwing , x = (lat*-1))) + 
  geom_point(aes(colour = pop, shape = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() + 
  labs(colour = "Populations", shape = "Populations") + 
  xlab(" ") +
  ylab("Median wing length (mm)") + 
  scale_x_continuous(breaks = round(seq(12, 37, by = 5),1), limits = c(12,37)) +
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 8, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = wings[[1]]$labs), force = 2) +
  #geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour = "black", size = 0.5) +
  ggtitle("Wild populations")


wings.domes <- ggplot(wings[[2]],aes(y = mwing , x = (lat*-1))) + 
  geom_point(aes(colour = pop, shape = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() + 
  labs(colour = "Populations", shape = "Populations") + 
  xlab(" ") +
  ylab("Median wing length (mm)") + 
  scale_x_continuous(breaks = round(seq(12, 37, by = 5),1), limits = c(12,37)) + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 8, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = wings[[2]]$labs), force = 2) +
  #geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour = "black", size = 0.5) +
  ggtitle("Domesticated populations")


wings.changes <- ggplot(wings.change, aes(y = Wing_change , x = (lat*-1))) + 
  geom_point(aes(colour = pop, shape = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() + 
  labs(colour = "Populations", shape = "Populations") + 
  xlab("Latitude (South)") +
  ylab("Proportion (Domesticated/Wild)") + 
  scale_x_continuous(breaks = round(seq(12, 37, by = 5),1), limits = c(12,37)) +
  theme_bw() +
  ylim(0.8, 1.2) + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray",fill=NA, size=1)) + 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 8, unit = "pt")),
        plot.title = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = wings.change$labs), force = 2) +
  geom_hline(data = no.change, aes(yintercept = change), colour = "black",linetype = "twodash") + 
  ggtitle("Domesticated populations/Wild populations")
plots.wings <- plot_grid(wings.wild + theme(legend.position = "none"),
                         wings.domes + theme(legend.position = "none"),
                         wings.changes + theme(legend.position = "none"), 
                         ncol = 1, align = "v", axis = "l")

legends_wing <- get_legend(wings.wild + theme(legend.position="bottom"))

title <- ggdraw() + 
  draw_label(
    "Fig 6",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )


plots.wings.all <- plot_grid(plots.wings, legends_wing,
                             ncol = 1, rel_heights = c(0.99, .1))



ggsave(plot = plots.wings.all, "./fig/Fig7.svg", width = 8, height = 12)
