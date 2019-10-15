
#---
# load data for figues
#---

source("R/heat_data_clean_import.R")
library("svglite")

#---
# Figure 1: Heat tolerance for all wild and domesticated populations
#---


heat.no.change <- data.frame(sex = c("Male"), change = c(1))

heat.domes.2$status <- factor(heat.domes.2$status, levels = c("Wild", "Domesticated"))

heat.domes.s <- heat.domes.2 %>% 
  ungroup() %>%
  group_by(status) %>% 
  group_split() %>%
  purrr::map(function(x) droplevels(x))


p0 <- ggplot(heat.domes.s[[1]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop, shape = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Populations", shape = "Populations") + 
  ylab("Median \n knockdown time (min)") +
  xlab(" ") + 
  scale_y_continuous(breaks = seq(15, 60, by = 5), limits = c(22, 48)) + 
  scale_x_continuous(breaks = seq(12, 40, by = 5), limits = c(12, 38)) +
  #stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.5) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal", 
        legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10), 
        legend.title.align = 1) + 
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.text=element_text(size=12)) + 
  ggtitle("Wild populations")

p2 <- ggplot(heat.domes.s[[2]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop, shape = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() + 
  labs(colour = "Qfly populations") + 
  ylab("Median \n knockdown time (min)") +
  xlab(" ") + 
  scale_y_continuous(breaks = seq(15, 60, by = 5), limits = c(22, 48)) + 
  scale_x_continuous(breaks = seq(12, 40, by = 5), limits = c(12, 38)) +
  #stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.5) +
  theme_bw(base_size = 12) +
  guides(linetype = FALSE) + 
  theme(axis.title.y = element_text(size=12), 
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ggtitle("Domesticated populations")


wild.p1 <- ggplot(corr.heat.wild.males.prop, aes(x=(lat*-1), y = proportion.wild)) + 
  geom_point(aes(colour = pop, shape = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() + 
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Wild/S06)") +
  xlab(" ") + 
  ylim(0.5, 1.5) +
  scale_x_continuous(breaks = seq(12, 40, by =5), limits = c(12, 38)) +
  geom_hline(data = heat.no.change, aes(yintercept = change), colour = "black", size = 0.5, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  labs(shape = "Domestication status") +
  theme_bw() + 
  theme(axis.title.y = element_text(size=12), 
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ggtitle("Wild populations/S06")

domes.p1 <- ggplot(corr.heat.wild.males.prop, aes(x=(lat*-1), y = proportion.domes)) + 
  geom_point(aes(colour = pop, shape = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() + 
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Domesticated/S06)") +
  xlab(" ") + 
  ylim(0.5, 1.5) +
  theme(axis.text=element_text(size=4)) +
  scale_x_continuous(breaks = seq(12, 40, by =5), limits = c(12, 38)) +
  geom_hline(data = heat.no.change, aes(yintercept = change), colour = "black", size = 0.5, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  labs(shape = "Domestication status") +
  theme_bw() + 
  theme(axis.title.y = element_text(size=12), 
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ggtitle("Domesticated populations/S06")

change.p1 <- ggplot(corr.heat.wild.males.prop, aes(x=(lat*-1), y = change)) + 
  geom_point(aes(colour = pop, shape = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() + 
  labs(colour = "Qfly populations") + 
  ylab("Change in tolerance \n (Domesticated/Wild)") +
  xlab("Latitude (South)") + 
  ylim(0.5, 1.5) +
  theme(axis.text=element_text(size=4)) +
  scale_x_continuous(breaks = seq(12, 40, by =5), limits = c(12, 38)) +
  geom_hline(data = heat.no.change, aes(yintercept = change), colour = "black", size = 0.5, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  labs(shape = "Domestication status") +
  theme_bw() + 
  theme(axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ggtitle("Wild populations/Domesticated populations")




prow <- plot_grid(p0 + theme(legend.position = "none"), 
                  p2 + theme(legend.position = "none"), 
                  wild.p1 + theme(legend.position = "none"), 
                  domes.p1 + theme(legend.position = "none"), 
                  change.p1 + theme(legend.position = "none"),
                  #NULL, 
                  ncol = 1, 
                  align = "v", 
                  axis = "r",
                  rel_heights = c(1, 1, 1, 1, 1, 0.05))

title <- ggdraw() + 
  draw_label(
    "Fig 1",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

legend_a <- get_legend(p0 + theme(legend.position="bottom"))

fig2 <- plot_grid(prow, legend_a, ncol = 1, rel_heights = c(0.93, 0.05))


#ggsave("fig/fig1.png", width = 6, height = 12, units = "in", dpi = 300)
ggsave(fig2, filename = "fig/fig2.svg", width = 6, height = 12)





