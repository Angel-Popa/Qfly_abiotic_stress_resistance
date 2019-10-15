

#---
# load data for figues
#---

source("R/cold_data_import_and_cleaning.R")

#---
# Figure 2: Cold tolerance data for all wild and domesticated populations 
#---

cold.no.change <- data.frame(sex = c("Female", "Male"),
                             change = c(1,1))

cold.p1 <- ggplot(cold.trt.s[[1]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_shape_Qfly() +
  labs(colour = "Populations", 
       shape = "Populations") + 
  ylab("Median \n recovery time (min)") +
  xlab(" ") + 
  theme(axis.text=element_text(size= 12)) +
  scale_y_continuous(breaks = seq(120, 155, by = 5), limits = c(120, 155)) + 
  scale_x_continuous(breaks = seq(12, 40, by = 5), limits = c(12, 38)) +
  #stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.5) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE) + 
  theme(axis.title.y = element_text(size=12), 
        axis.title.x = element_text(size=12),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  theme(legend.position = "bottom", legend.box = "horizontal", 
        legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10), 
        legend.title.align = 1) +
  ggtitle("Wild populations")


cold.p2 <- ggplot(cold.trt.s[[2]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_shape_Qfly() +
  labs(colour = "Populations") + 
  ylab("Median \n recovery time (min)") +
  xlab(" ") + 
  theme(axis.text=element_text(size= 12)) +
  scale_y_continuous(breaks = seq(120, 155, by = 5), limits = c(120, 155)) + 
  scale_x_continuous(breaks = seq(12, 40, by = 5), limits = c(12, 38)) +
  #stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.5) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE) + 
  theme(axis.title.y = element_text(size=12),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  theme(legend.position = "bottom", legend.box = "horizontal", 
        legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        legend.title.align = 1) + 
  ggtitle("Domesticated populations")


cold.wild.p1 <- ggplot(corr.cold.domes.wild.males.prop, aes(x=(lat*-1), y = proportion.wild)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_shape_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Wild/S06)") +
  xlab(" ") + 
  ylim(0.5, 1.5) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  geom_hline(data = cold.no.change, aes(yintercept = change), colour = "black", size = 0.5, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  theme_bw() + 
  theme(axis.title.y = element_text(size=12),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ggtitle("Wild populations/S06")

cold.domes.p1 <- ggplot(corr.cold.domes.wild.males.prop, aes(x=(lat*-1), y = proportion.domes)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_shape_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Domesticated/S06)") +
  xlab(" ") + 
  ylim(0.5, 1.5) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  geom_hline(data = cold.no.change, aes(yintercept = change), colour = "black", size = 0.5, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  theme_bw() +
  theme(axis.title.y = element_text(size=12), 
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ggtitle("Domesticated populations/S06")


cold.change.p1 <- ggplot(corr.cold.domes.wild.males.prop, aes(x=(lat*-1), y = change)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Change in tolerance \n (Domesticated/Wild)") +
  xlab("Latitude (South)") + 
  ylim(0.5, 1.5) +
  scale_x_continuous(breaks = seq(12, 40, by =5), limits = c(12, 38)) +
  geom_hline(data = cold.no.change, aes(yintercept = change), colour = "black", size = 0.5, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  theme_bw() + 
  theme(axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ggtitle("Domesticated populations/Wild populations")




prow <- plot_grid(cold.p1 + theme(legend.position = "none"), 
                  cold.p2 + theme(legend.position = "none"), 
                  cold.wild.p1 + theme(legend.position = "none"), 
                  cold.domes.p1 + theme(legend.position = "none"), 
                  cold.change.p1 + theme(legend.position = "none"),
                  #NULL,
                  ncol = 1, 
                  align = "v", 
                  axis = "r", 
                  rel_heights = c(1,1,1,1,1, 0.05))

title <- ggdraw() + 
  draw_label(
    "Fig 2",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

legend_a <- get_legend(cold.p1 + theme(legend.position="bottom"))

#fig2 <- plot_grid(prow, legend_a, ncol = 1, rel_heights = c(1, 0.05))

fig3 <- plot_grid(prow, legend_a, ncol = 1, rel_heights = c(0.90, 0.06))

ggsave(fig3, filename = "fig/fig3.svg", width = 6, height = 12)

