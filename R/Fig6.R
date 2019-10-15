
#---
# load data for figues
#---

source("R/desiccation_and_starvation_data_import_and_cleaning.R")
library("ggrepel")

#---
# Figure 5: Correlation between G2 desiccation tolerance of the primary 
# and resampled collections from Cape Tribulation, Alice Springs and Sydney
#---

repeated.desiccation.plot <- repeated.desiccation %>% 
  dplyr::select(-c("exp", "gen")) %>% 
  tidyr::spread(sample, pmedian)


p6 <- ggplot(repeated.desiccation.plot, aes(x = first, y = second)) + 
  geom_point(aes(colour = pop, shape = pop), size = 4) + 
  theme_bw() + 
  labs(colour = "Populations", shape = "Populations") +
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  scale_shape_Qfly() + 
  xlab(expression(atop("Median survival time (hrs)", paste(italic("(Primary survey)"))))) +
  ylab(expression(atop('Median survival time (hrs)', paste(italic('(Resampled populations)'))))) +
  ggtitle("Desiccation tolerance in resampled Qfly colonies") + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        axis.title = element_text(size = 12)) +
  geom_smooth(method = lm, se = FALSE, colour = "gray") +
  geom_text_repel(aes(label = repeated.desiccation.plot$pop), force = 5, size = 5) + 
  theme(legend.position = "none")

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

fig6 <- plot_grid(title, p6, ncol = 1, rel_heights = c(1))

ggsave(plot = fig6, filename = "fig/fig6.svg", width = 8, height = 8)