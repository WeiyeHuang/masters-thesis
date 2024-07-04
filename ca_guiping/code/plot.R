.libPaths('~/apps/RLib')
library(tidyverse)
library(ggthemes)
library(cowplot)

setwd("~/experiments/ca_guiping_huangwy88")
load('ca_data.RData')
vtr <- setdiff(ls(), "bio_data_list")
rm(list = vtr)
rm(vtr)

plot_ca <- function(climvar) {
  ggplot(bio_data_list[[climvar]], aes(x=Genera, ymin=Min, ymax=Max, colour = Genera)) +
    geom_errorbar(width = 0.1, linewidth = 1) +
    labs(x="Genera", y=toupper(climvar)) +
    theme_base() +
    scale_color_tableau("Tableau 20") +
    theme(text = element_text(family = "times", size = 12)) +
    theme(legend.position = "none", axis.text.x = element_text(face = "italic", angle = 90, hjust = 1))
}

plots <- lapply(c('bio1', 'bio2', 'bio3', 'bio4', 'bio5',
                  'bio6', 'bio7', 'bio8', 'bio9', 'bio10',
                  'bio11', 'bio12', 'bio13', 'bio14', 'bio15',
                  'bio16', 'bio17', 'bio18', 'bio19'), plot_ca)

svg('ca_plot.svg', 20, 9)
plot_grid(plotlist = plots, ncol = 7, nrow = 3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()
