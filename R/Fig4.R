

#---
# load data for figues
#---

source("R/desiccation_and_starvation_data_import_and_cleaning.R")

#---
# Figure 3: Desiccation and starvation tolerance data
#---

desiccation.no.change <- data.frame(sex = c("Male"), change = c(1,1))


fig.rep.1 <- ggplot(des.wild.repeat.2[[1]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_shape_Qfly() +
  labs(colour = "Populations", 
       shape = "Populations") +
  ylab("Median \n survival time (hrs)") +
  xlab(" ") + 
  ylim(20, 80) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt) + 
  #stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.5) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE) + 
  ggtitle("Wild populations") +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(margin = margin(r = 3, unit = "pt"))) + 
  scale_linetype_manual(name = "", values = c("solid", "twodash"))+ 
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
        axis.title = element_text(size = 12), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))

fig.rep.2 <- ggplot(des.wild.repeat.2[[2]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_shape_Qfly() +
  labs(colour = "Populations", 
       shape = "Populations") + 
  ylab("Median \nsurvival time (hrs)") +
  xlab(" ") + 
  ylim(10, 60) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt) + 
  #stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.5) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE) +
  ggtitle("Domesticated populations") +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(margin = margin(r = 3, unit = "pt"))) + 
  scale_linetype_manual(name = "", values = c("solid", "twodash"))+ 
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))


des.wild.p1 <- ggplot(desiccation.change.domestication, aes(x=(lat*-1), y = proportion.wild)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_shape_Qfly() +
  labs(colour = "Populations") + 
  ylab("Proportion \n(Wild/S06)") +
  xlab(" ") + 
  ylim(0, 2.8) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt.x) +
  geom_hline(data = desiccation.no.change, aes(yintercept = change), colour = "black", size = 0.5, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  ggtitle("Wild populations/S06") +
  theme_bw()+ 
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))

des.domes.p1 <- ggplot(desiccation.change.domestication, aes(x=(lat*-1), y = proportion.domes)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_shape_Qfly() +
  labs(colour = "Populations") +
  ylab("Proportion \n(Domesticated/S06)") +
  xlab(" ") + 
  ylim(0, 2.8) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt.x) +
  geom_hline(data = desiccation.no.change, aes(yintercept = change), colour = "black", size =0.5, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  ggtitle("Domesticated populations/S06") +
  theme_bw()+ 
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))

des.change.p1 <- ggplot(desiccation.change.domestication, aes(x=(lat*-1), y = change)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_shape_Qfly() +
  labs(colour = "Populations") +
  ylab("Proportion \n(Domesticated/Wild)") +
  xlab("Latitude (South)") + 
  ylim(0, 2.8) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt.x) +
  geom_hline(data = desiccation.no.change, aes(yintercept = change), colour = "black", size =0.5, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) +
  ggtitle("Domesticated populations/Wild populations") +
  theme_bw() + 
  guides(colour = guide_legend(order = 1)) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(margin = margin(r = 2, unit = "pt")))+ 
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))


plot.des <- plot_grid(fig.rep.1 + theme(legend.position = "none"), 
                      fig.rep.2 + theme(legend.position = "none"), 
                      des.wild.p1 + theme(legend.position = "none"), 
                      des.domes.p1 + theme(legend.position = "none"),
                      des.change.p1 + theme(legend.position = "none"),
                      #NULL,
                      ncol = 1, 
                      align = "v", 
                      axis = "r", rel_heights = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.05))

legends <- get_legend(fig.rep.1 +theme(legend.position = "bottom"))

title <- ggdraw() + 
  draw_label(
    "Fig 3",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

fig3 <- plot_grid(plot.des, legend_a, ncol = 1, rel_heights = c(0.94, 0.05))
#fig3 <- plot_grid(plot.des, legends, ncol = 1, rel_heights = c(1, 0.05))

ggsave(plot = fig3, filename = "fig/fig4.svg", width = 10, height = 14)
