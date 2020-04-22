library(pacman)
pacman::p_load(dplyr, tidyr, ggplot2, reshape2)


correlation_heatmap <- function(df, heatmap_filename, textsize=4, digits=4) {
  
  cormat <- round(cor(df, use="complete.obs"), digits)
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
    geom_text(aes(Var2, Var1, label = value), color = "black", size = textsize) +
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
  
  print(cor_heatmap)
  ggsave(heatmap_filename, plot = cor_heatmap)
}



# main entry point

setwd("~/datalake/CIEDB_2009_2013/")

# load MACRO sectoral vars
load("08_macro_data.Rda", verbose=T)  # df, sector_description
# load Levy Parameters
load("06_China_fit_results_dataframe.Rda", verbose=T)      #  df3
# load nHHI and Entropy and FirmAge and SHares from Firm level
load("09_ISICR4_sectoral_monopolization.Rda", verbose=T) #nHHI_TOAS_wide, Entropy_TOAS_wide, nHHI_TOAS_long, Entropy_TOAS_long
#load("09_ISICR4_sectoral_change.Rda", verbose=T) # df_E_g, df_VA_g, df_LP_diff, df_LP_IO_diff, df_FA_g, df_E_s, df_VA_s,
load("09_ISICR4_sectoral_shares_from_balanced.Rda", verbose=T) # df_E_s, df_VA_s
load("09_ISICR4_sectoral_accounts.Rda", verbose=T) #sectoral

# Data preparation

# Macro data
df <- df %>%
  select(Year, code, EMP, VA, LP, K, GO, COMP, CAP, RoC, WS, Wages, VA_PI, VA_QI, Employment_g, VA_g, LP_diff)

df$Year <- as.numeric(as.character(df$Year))
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

#df3_Levy_LP <- df3 %>%
#  filter(Separator_Variable=="Sector.ISICR4" & Fit_Variable=="Labor productivity (imputed)")

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

df_combined <- merge(df_combined, nHHI_TOAS_long, by=c("Year", "Sector"), all=T)

colnames(Entropy_TOAS_long) <- c("Year", "Sector", "Entropy")

df_combined <- merge(df_combined, Entropy_TOAS_long, by=c("Year", "Sector"), all=T)

# Firm Age
df_FA <- sectoral %>% select(Year, Sector.Short, Avg.Firm_Age)
colnames(df_FA) <- c("Year", "Sector", "Average Firm Age")

df_combined <- merge(df_combined, df_FA, by=c("Year", "Sector"), all=T)

# Firm level imputed sectoral growth rates for consistency check

df_E_s <- df_E_s[,colnames(df_E_s)!=""]
df_E_s <- melt(df_E_s)
colnames(df_E_s) <- c("Year", "Sector", "Employment growth CIEDB")

df_combined <- merge(df_combined, df_E_s, by=c("Year", "Sector"), all=T)

df_VA_s <- df_VA_s[,colnames(df_VA_s)!=""]
df_VA_s <- melt(df_VA_s)
colnames(df_VA_s) <- c("Year", "Sector", "VA growth CIEDB")

df_combined <- merge(df_combined, df_VA_s, by=c("Year", "Sector"), all=T)

df_combined2 <- df_combined %>% 
  select(-c("Year", "Sector"))

df_combined_fdiff <- df_combined %>%
  arrange(Sector, Year) %>%
  group_by(Sector) %>%
  mutate_at(vars(-Sector, -Year), funs(. - dplyr::lag(., 1))) %>%
  ungroup()

df_combined_fdiff2 <- df_combined_fdiff %>% 
  select(-c("Year", "Sector"))

correlation_heatmap(df_combined2, heatmap_filename="correlation_heatmap_MACRO.pdf", digits=2, textsize=2)
correlation_heatmap(df_combined_fdiff2, heatmap_filename="correlation_heatmap_MACRO_fdiff.pdf", digits=2, textsize=2)

df_combined_reduced <- df_combined2 %>%
  select(Employment, VA, LP, Capital, `Return on Capital`, `Wage share`, `Average wage`, `Employment growth WIOD`, 
         `VA growth WIOD`, `LP Change WIOD`, `Investment rate Levy alpha`, `Return on capital Levy alpha`, `LP Levy alpha`, 
         `LP Levy delta`, `LP Change Levy alpha`, `LP Change Levy beta`, `LP Change Levy gamma`, nHHI, Entropy, 
         `Average Firm Age`)

df_combined_fdiff_reduced <- df_combined_fdiff2 %>%
  select(Employment, VA, LP, Capital, `Return on Capital`, `Wage share`, `Average wage`, `Employment growth WIOD`, 
         `VA growth WIOD`, `LP Change WIOD`, `Investment rate Levy alpha`, `Return on capital Levy alpha`, `LP Levy alpha`, 
         `LP Levy delta`, `LP Change Levy alpha`, `LP Change Levy beta`, `LP Change Levy gamma`, nHHI, Entropy, 
         `Average Firm Age`)

correlation_heatmap(df_combined_reduced, heatmap_filename="correlation_heatmap_MACRO_reduced.pdf", digits=2, textsize=3)
correlation_heatmap(df_combined_fdiff_reduced, heatmap_filename="correlation_heatmap_MACRO_fdiff_reduced.pdf", digits=2, textsize=3)

df_combined_FL_only <- df_combined2 %>%
  select(Employment, VA, LP, Capital, `Gross output`, `Wage bill`, `Returns`, `Return on Capital`, `Wage share`, `Average wage`,              
         `Output price level`, `Output quantity`, `Employment growth WIOD`, `VA growth WIOD`, `LP Change WIOD`)

df_combined_fdiff_FL_only <- df_combined_fdiff2 %>%
  select(Employment, VA, LP, Capital, `Gross output`, `Wage bill`, `Returns`, `Return on Capital`, `Wage share`, `Average wage`,              
         `Output price level`, `Output quantity`, `Employment growth WIOD`, `VA growth WIOD`, `LP Change WIOD`)

colnames(df_combined_FL_only) <- c("Employment", "Value added", "Labor productivity", "Capital", "Gross output", "Wage bill", "Returns", "Return on Capital", "Wage share", "Average wage",              
                                   "Output price level", "Output quantity", "Employment growth", "VA growth", "LP Change")
colnames(df_combined_fdiff_FL_only) <- c("Employment", "Value added", "Labor productivity", "Capital", "Gross output", "Wage bill", "Returns", "Return on Capital", "Wage share", "Average wage",              
                                   "Output price level", "Output quantity", "Employment growth", "VA growth", "LP Change")

correlation_heatmap(df_combined_FL_only, heatmap_filename="correlation_heatmap_MACRO_FL_only.pdf", digits=2, textsize=4)
correlation_heatmap(df_combined_fdiff_FL_only, heatmap_filename="correlation_heatmap_MACRO_fdiff_FL_only.pdf", digits=2, textsize=4)

df_combined_reduced_macro_levy <- df_combined %>%
  select(Employment, VA, LP, `Employment growth WIOD`, `VA growth WIOD`, `LP Change WIOD`, `LP Levy alpha`, 
         `LP Levy delta`, `LP Change Levy alpha`, `LP Change Levy beta`, `LP Change Levy gamma`, nHHI,  
         `Average Firm Age`)

df_combined_fdiff_reduced_macro_levy <- df_combined_fdiff2 %>%
  select(Employment, VA, LP, `Employment growth WIOD`, `VA growth WIOD`, `LP Change WIOD`, `LP Levy alpha`, 
         `LP Levy delta`, `LP Change Levy alpha`, `LP Change Levy beta`, `LP Change Levy gamma`, nHHI,  
         `Average Firm Age`)

colnames(df_combined_fdiff_reduced_macro_levy) <- c("Employment", "Value added", "Labor productivity", "Employment growth", "VA growth", "LP change", "LP Levy alpha", 
                                                    "LP Levy delta", "LP change Levy alpha", "LP change Levy beta", "LP change Levy gamma", "normalized HHI",  
                                                    "Average firm age")
colnames(df_combined_reduced_macro_levy) <- c("Employment", "Value added", "Labor productivity", "Employment growth", "VA growth", "LP change", "LP Levy alpha", 
                                                    "LP Levy delta", "LP change Levy alpha", "LP change Levy beta", "LP change Levy gamma", "normalized HHI",  
                                                    "Average firm age")

correlation_heatmap(df_combined_reduced_macro_levy, heatmap_filename="correlation_heatmap_MACRO_Levy.pdf", digits=2, textsize=4)
correlation_heatmap(df_combined_fdiff_reduced_macro_levy, heatmap_filename="correlation_heatmap_MACRO_fdiff_Levy.pdf", digits=2, textsize=4)







