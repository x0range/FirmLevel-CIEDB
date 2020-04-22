library(pacman)
pacman::p_load(dplyr, tidyr, ggplot2, reshape2)


correlation_heatmap <- function(df) {
  
  cormat <- round(cor(df, use="complete.obs"),4)
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
  
  ggsave("correlation_heatmap_MACRO.pdf", plot = cor_heatmap)
}



# main entry point

setwd("~/datalake/CIEDB_2009_2013/")

# load MACRO sectoral vars
load("08_macro_data.Rda", verbose=T)  # df, sector_description
# load Levy Parameters
load("06_China_fit_results_dataframe.Rda", verbose=T)      #  df3
# load nHHI and Entropy and FirmAge and SHares from Firm level
load("09_ISICR4_sectoral_monopolization.Rda", verbose=T) #nHHI_TOAS_wide, Entropy_TOAS_wide, nHHI_TOAS_long, Entropy_TOAS_long
load("09_ISICR4_sectoral_shares_from_balanced.Rda", verbose=T) # df_E_s, df_VA_s


# Data preparation

# Macro data
df <- df %>%
  select(Year, code, EMP, VA, LP, K, GO, COMP, CAP, RoC, WS, Wages, VA_PI, VA_QI, Employment_g, VA_g, LP_diff)

colnames(df) <- c("Year", "Sector", "Employment", "VA", "LP", "Capital", "Gross output", "Wage bill", "Returns", 
                  "Return on Capital",  "Wage share",  "Average wage", "Output price level", "Output quantity",
                  "Employment growth WIOD", "VA growth WIOD", "LP Change WIOD")

# Levy fits # pt 1

df3 <- df3[df3$Levy_Soofi_ID>=90,]
df3_alphas <- df3 %>%
  filter(Separator_Variable=="Sector.ISICR4" & Fit_Variable!="Labor productivity (imputed)" & Fit_Variable!="Labor productivity (imputed) difference") %>%
  select(Fit_Variable, Year, Class, Levy_alpha)
df3_alphas <- dcast(df3_alphas, Year+Class ~ Fit_Variable)

df3_alphas <- df3_alphas %>% 
  select(Year, Class, `Investment rate`, `Labor productivity growth`, `Return on Capital`, TFP)

colnames(df3_alphas) <- c("Year", "Sector", "Investment rate Levy alpha", "LP growth Levy alpha", "Return on capital Levy alpha", "TFP Levy alpha")

df_combined <- merge(df, df3_alphas, by=c("Year", "Sector"), all=T)

# Levy fits # pt 2

df3_Levy_LP <- df3 %>%
  filter(Separator_Variable=="Sector.ISICR4" & Fit_Variable=="Labor productivity (imputed)") %>%
  select(Year, Class, Levy_alpha, Levy_beta, Levy_gamma, Levy_delta)

colnames(df3_Levy_LP) <- c("Year", "Sector", "LP Levy alpha", "LP Levy beta", "LP Levy gamma", "LP Levy delta")

df_combined <- merge(df_combined, df3_Levy_LP, by=c("Year", "Sector"), all=T)

# Levy fits # pt 3

df3_Levy_LP_diff <- df3 %>%
  filter(Separator_Variable=="Sector.ISICR4" & Fit_Variable=="Labor productivity (imputed) difference") %>%
  select(Year, Class, Levy_alpha, Levy_beta, Levy_gamma, Levy_delta)

colnames(df3_Levy_LP_diff) <- c("Year", "Sector", "LP Change Levy alpha", "LP Change Levy beta", "LP Change Levy gamma", "LP Change Levy delta")

df_combined <- merge(df_combined, df3_Levy_LP_diff, by=c("Year", "Sector"), all=T)

# nHHI, Entropy
colnames(nHHI_TOAS_long) <- c("Year", "Sector", "nHHI")
colnames(Entropy_TOAS_long) <- c("Year", "Sector", "Entropy")

# Firm level imputed sectoral growth rates for consistency check

df_E_s <- df_E_s[,colnames(df_E_s)!=""]
df_E_s <- melt(df_E_s)
colnames(df_E_s) <- c("Year", "Sector", "Employment growth CIEDB")
df_VA_s <- df_VA_s[,colnames(df_VA_s)!=""]
df_VA_s <- melt(df_VA_s)
colnames(df_VA_s) <- c("Year", "Sector", "VA growth CIEDB")




reduced_df <- df %>% 
  ungroup() %>%
  select(Employment_diff_ann, def_VA_diff_ann, def_VA_IO_diff_ann, 
         def_FIAS_diff_ann, def_TOAS_diff_ann, def_LP_diff_ann, 
         def_LP_IO_diff_ann, def_Sales_diff_ann, def_Wages_pc_diff_ann, 
         def_Wages_diff_ann, WS_diff_ann, WS_IO_diff_ann)

colnames(reduced_df) <- c("Employment", "Output (Y)", "VA", "Fixed Cap.", "Total Cap.", "LP (Y)", 
                          "LP (VA)", "Revenue", "Wages pc", "Wage Bill", "Wages/Y", 
                          "Wages/VA")



#cor_coeff <- cor(df$Employment_diff_ann, df$def_VA_diff_ann, use="complete.obs")
#lm <- lm(Employment_diff_ann~def_VA_diff_ann, data=df)
