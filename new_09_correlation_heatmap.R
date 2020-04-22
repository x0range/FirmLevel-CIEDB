library(pacman)
pacman::p_load(dplyr, tidyr, hexbin, ggplot2, reshape2)

setwd("~/datalake/CIEDB_2009_2013/")
load(file="08_data_complete_panels_annualized.Rda", verbose=T) # df

reduced_df <- df %>% 
  ungroup() %>%
  select(Employment_diff_ann, def_VA_diff_ann, def_VA_IO_diff_ann, 
         def_FIAS_diff_ann, def_TOAS_diff_ann, def_LP_diff_ann, 
         def_LP_IO_diff_ann, def_Sales_diff_ann, def_Wages_pc_diff_ann, 
         def_Wages_diff_ann, WS_diff_ann, WS_IO_diff_ann)

reduced_df <- do.call(data.frame,lapply(reduced_df, function(x) replace(x, is.infinite(x),NA)))

colnames(reduced_df) <- c("Employment", "Output (Y)", "VA", "Fixed Cap.", "Total Cap.", "LP (Y)", 
                      "LP (VA)", "Revenue", "Wages pc", "Wage Bill", "Wages/Y", 
                      "Wages/VA")
cormat <- round(cor(reduced_df, use="complete.obs"),3)
upper_tri <- cormat
upper_tri[lower.tri(upper_tri)]<- NA
melted_cormat <- melt(upper_tri, na.rm = TRUE)
cor_heatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

ggsave("correlation_heatmap.pdf", plot = cor_heatmap)


cor_coeff <- cor(df$Employment_diff_ann, df$def_VA_diff_ann, use="complete.obs")
lm <- lm(Employment_diff_ann~def_VA_diff_ann, data=df)
