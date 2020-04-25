
library(pacman)
pacman::p_load(openxlsx, reshape2, ggplot2, dplyr)


#load("08_GB2002_ISICR4_table.Rda", verbose=T)  # GB2002_ISICR4
#colnames(GB2002_ISICR4) <- c("code", "Sector.Short")



MACRO_data <- read.xlsx("WIOD_SEA_Nov16.xlsx", sheet=2)
MACRO_data_explanation <- read.xlsx("WIOD_SEA_Nov16.xlsx", sheet=1)
#print(MACRO_data_explanation[,c("X3","X4")])
# (GO    - Gross output)
# (II    - Intermediate inputs)
# VA     - Value added
# EMP    - Number of persons engaged
# (EMPE  - Employees)        (all NA)
# (H_EMPE- Work hours tota)  (all NA)l
# COMP   - Compensation of employees
# (LAB   - labour compensation)  
# CAP    - Capital compensation
# K      - Nominal capital
# GO_PI
# II_PI
# VA_PI  - Value added price level
# GO_QI
# II_QI
# VA_QI  - Gross value added volume (2010)

MACRO_China <- MACRO_data[MACRO_data$country=="CHN", ]
MACRO_China$country <- NULL

# get description
sector_description <- unique(MACRO_China[, c("description", "code")])
#MACRO_China$description <- NULL

# long format
# join all vars
MACRO_China <- melt(MACRO_China, index.vars=c("code", "description"), variable.name="Year", value.name="value")
MACRO_China <- dcast(MACRO_China, Year+description+code ~ variable)

df <- MACRO_China %>%
  group_by(Year) %>%
  mutate(
    Employment_share = EMP / sum(EMP),
    VA_share = VA / sum(VA),
    K_share = K / sum(K),
    LP = VA / EMP,
    WS = COMP / VA,
    RoC = CAP / K,
    Wages = COMP / EMP,
  ) %>%
  ungroup() %>%
  arrange(code, Year) %>%
  group_by(code) %>%
  mutate(
    K_diff = K - dplyr::lag(K, 1),
    Employment_diff = EMP - dplyr::lag(EMP, 1),
    VA_diff = VA - dplyr::lag(VA, 1),
    LP_diff = LP - dplyr::lag(LP, 1),
    Employment_share_diff = Employment_share - dplyr::lag(Employment_share, 1),
    VA_share_diff = VA_share - dplyr::lag(VA_share, 1),
    
    IR = K_diff / dplyr::lag(K, 1),
    Employment_g = Employment_diff / dplyr::lag(EMP, 1),
    VA_g = VA_diff / dplyr::lag(VA, 1),
    Employment_share_g = Employment_share_diff / dplyr::lag(Employment_share, 1),
    VA_share_g = VA_share_diff / dplyr::lag(VA_share, 1),
    
    Wage_per_output_quantity = Wages / VA_QI,
    Capital_intensity = K / EMP,
    VA_per_output_quantity = VA / VA_QI,
  ) %>%
  select(-c(K_diff))


sector_dispersion <- df %>%
  group_by(Year) %>%
  summarise(
    N = n(),
    HHI_E = sum(Employment_share**2, na.rm=T),
    Entropy_E = -1 * sum(Employment_share * log(Employment_share), na.rm=T),
    HHI_K = sum(K_share**2, na.rm=T),
    Entropy_K = -1 * sum(K_share * log(K_share), na.rm=T),
    HHI_VA = sum(VA_share**2, na.rm=T),
    Entropy_VA = -1 * sum(VA_share * log(VA_share), na.rm=T),
  ) %>%
  arrange(Year) %>%
  mutate(
    nHHI_E = (HHI_E - 1/N)/(1-1/N),
    nHHI_K = (HHI_K - 1/N)/(1-1/N),
    nHHI_VA = (HHI_VA - 1/N)/(1-1/N),
  )

sector_growth <- df %>%
  ungroup() %>%
  arrange(code, Year) %>%
  group_by(code) %>%
  summarise(
    N = n(),
    E_diff = last(Employment_share)-first(Employment_share),
    VA_diff = last(VA_share) - first(VA_share),
    E_g = E_diff/first(Employment_share),
    VA_g = VA_diff/first(VA_share),
  )

save(df, sector_description, file="08_macro_data.Rda")
load("08_macro_data.Rda", verbose=T)  # df, sector_description

save(sector_dispersion, file="08_macro_sector_dispersion.Rda")
load("08_macro_sector_dispersion.Rda", verbose=T)  # sector_dispersion

save(sector_growth, file="08_macro_sector_growth.Rda")
load("08_macro_sector_growth.Rda", verbose=T)  # sector_dispersion

MACRO_VA_share_wide <- dcast(df[,c("Year", "code", "VA_share")], Year~code)
rownames(MACRO_VA_share_wide) <- MACRO_VA_share_wide$Year
MACRO_VA_share_wide$Year <- NULL

MACRO_E_share_wide <- dcast(df[,c("Year", "code", "Employment_share")], Year~code)
rownames(MACRO_E_share_wide) <- MACRO_E_share_wide$Year
MACRO_E_share_wide$Year <- NULL

MACRO_VA_share_g_wide <- dcast(df[,c("Year", "code", "VA_share_g")], Year~code)
MACRO_VA_share_g_wide <- MACRO_VA_share_g_wide[2:nrow(MACRO_VA_share_g_wide),]
rownames(MACRO_VA_share_g_wide) <- MACRO_VA_share_g_wide$Year
MACRO_VA_share_g_wide$Year <- NULL

MACRO_E_share_g_wide <- dcast(df[,c("Year", "code", "Employment_share_g")], Year~code)
MACRO_E_share_g_wide <- MACRO_E_share_g_wide[2:nrow(MACRO_E_share_g_wide),]
rownames(MACRO_E_share_g_wide) <- MACRO_E_share_g_wide$Year
MACRO_E_share_g_wide$Year <- NULL

MACRO_LP_wide <- dcast(df[,c("Year", "code", "LP")], Year~code)
MACRO_LP_wide <- MACRO_LP_wide[2:nrow(MACRO_LP_wide),]
rownames(MACRO_LP_wide) <- MACRO_LP_wide$Year
MACRO_LP_wide$Year <- NULL

save(MACRO_VA_share_wide, MACRO_E_share_wide, MACRO_VA_share_g_wide, MACRO_E_share_g_wide, MACRO_LP_wide, file="08_macro_sectoral_change.Rda")
load("08_macro_sectoral_change.Rda", verbose=T)  # MACRO_VA_share_wide, MACRO_E_share_wide, MACRO_VA_share_g_wide, MACRO_E_share_g_wide, MACRO_LP_wide



# plot sector growth


plot_sectoral_dev <- function(plot_data, var1, var2, var1_label, var2_label, xlim, ylim, plot_file_name) {
  the_plot <- ggplot() + 
    labs(x=var1_label, y=var2_label) +
    theme_bw() +
    theme(axis.text = element_text(size=16), axis.title=element_text(size=18), legend.position = "none") +
    geom_point(data=plot_data, aes(x=get(var1), y=get(var2), color=code)) +
    geom_text_repel(data=plot_data, aes(x=get(var1), y=get(var2), label=code, color=code), cex=7) +
    xlim(xlim[[1]], xlim[[2]]) + ylim(ylim[[1]], ylim[[2]])
  
  ggsave(plot_file_name, plot=the_plot)
  print(the_plot)
}

plot_sectoral_dev(var1 = "VA_g", var2 = "E_g", var1_label = "Value added share growth", var2_label = "Employment share growth",
                  plot_data = sector_growth, plot_file_name = "08_sectoral_total_growth_rates.pdf",
                  xlim=c(-.5,2), ylim=c(-1,4))
plot_sectoral_dev(var1 = "VA_diff", var2 = "E_diff", var1_label = "Value added share change", var2_label = "Employment share change",
                  plot_data = sector_growth, plot_file_name = "08_sectoral_total_change.pdf",
                  xlim=c(-.02,.025), ylim=c(-.01,.04))
