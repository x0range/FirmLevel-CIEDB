library(pacman)
pacman::p_load(dplyr, tidyr, hexbin, ggplot2, reshape2)

setwd("~/datalake/CIEDB_2009_2013/")
load(file="08_data_complete_panels_annualized.Rda", verbose=T) # df

if (F) {
  ggplot(df, aes(x = Employment, y= def_VA)) +
    stat_binhex() +
    scale_fill_gradient(name = "count", trans = "log", 
                        breaks = 10^(0:6),
    )
}

plot_hist_2d_big <- ggplot(df, aes(x = Employment_diff_ann, y= def_VA_diff_ann)) +
  stat_binhex(bins=100) +
  scale_fill_gradient(name = "Density", trans = "log", 
                      breaks = 10^(0:6),
  ) +
  xlim(-1500000, 10000000) +
  labs(
    x = "Increase in Employment",
    y = "Increase in Value Added")

plot_hist2d_small <- ggplot(df, aes(x = Employment_diff_ann, y= def_VA_diff_ann)) +
  stat_binhex(bins=100) +
  scale_fill_gradient(name = "Density", trans = "log", 
                      breaks = 10^(0:6)) +
  xlim(-15000, 100000) +
  labs(
    x = "Increase in Employment",
    y = "Increase in Value Added")

#print(plot_hist2d_small)

ggsave("hist2d_Delta_Empl_Delta_VA_small.pdf", plot = plot_hist2d_small)

