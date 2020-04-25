library(pacman)
pacman::p_load(dplyr, tidyr)


setwd("~/datalake/CIEDB_2009_2013/")

# GB2002_ISICR4 translation table
GB2002_ISICR4 <- t(data.frame(c(NA, ""),
                              c("A01", NA),
                              c("A02", NA),
                              c("A03", NA),
                              c("B", "06"),
                              c("B", "07"),
                              c("B", "08"),
                              c("B", "09"),
                              c("B", "10"),
                              c("B", "11"),
                              c("B", "12"),
                              c("C10-C12", "13"),
                              c("C10-C12", "14"),
                              c("C10-C12", "15"),
                              c("C10-C12", "16"),
                              c("C13-C15", "17"),
                              c("C13-C15", "18"),
                              c("C13-C15", "19"),
                              c("C16", "20"),
                              c("C17", "22"),
                              c("C18", "23"),
                              c("C19", "25"),
                              c("C20", "26"),
                              c("C20", "28"),
                              c("C21", "27"),
                              c("C22", "29"),
                              c("C22", "30"),
                              c("C23", "31"),
                              c("C24", "32"),
                              c("C24", "33"),
                              c("C25", "34"),
                              c("C26", NA),
                              c("C27", NA),
                              c("C28", "35"),
                              c("C29", "36"),
                              c("C30", "37"),
                              c("C31_C32", "21"),
                              c("C33", NA),
                              c(NA, "24"),
                              c(NA, "38"),
                              c(NA, "39"),
                              c(NA, "40"),
                              c(NA, "41"),
                              c(NA, "42"),
                              c(NA, "43"),
                              c("D35", "44"),
                              c("D35", "45"),
                              c("E36", "46"),
                              c("E37-E39", NA),
                              c("F", NA),
                              c("G45", NA),
                              c("G46", NA),
                              c("G47", NA),
                              c("H49", NA),
                              c("H50", NA),
                              c("H51", NA),
                              c("H52", NA),
                              c("H53", NA),
                              c("I", NA),
                              c("J58", NA),
                              c("J59_J60", NA),
                              c("J61", NA),
                              c("J62_J63", NA),
                              c("K64", NA),
                              c("K65", NA),
                              c("K66", NA),
                              c("L68", NA),
                              c("M69_M70", NA),
                              c("M71", NA),
                              c("M72", NA),
                              c("M73", NA),
                              c("M74_M75", NA),
                              c("N", NA),
                              c("O84", NA),
                              c("P85", NA),
                              c("Q", NA),
                              c("R_S", NA),
                              c("T", NA),
                              c("U", NA), stringsAsFactors=F))
GB2002_ISICR4 <- data.frame(GB2002_ISICR4, row.names=seq(1:nrow(GB2002_ISICR4)),stringsAsFactors=F)
colnames(GB2002_ISICR4) <- c("ISICR4", "GB2002")
#colnames(GB2002_ISICR4) <- c("code", "Sector.Short")
#print(GB2002_ISICR4)

save(GB2002_ISICR4, file="08_GB2002_ISICR4_table.Rda")
load("08_GB2002_ISICR4_table.Rda", verbose=T)  # GB2002_ISICR4
colnames(GB2002_ISICR4) <- c("Sector.ISICR4", "Sector.Short")

load("dataframe_including_FirmType2.Rda", verbose=T)

df <- df %>% 
  ungroup() %>%
  left_join(GB2002_ISICR4, by="Sector.Short", na_matches="never")

df <- df %>% 
  select(ID, Year, Sector.Short, Sector.ISICR4, Sector, Province.Code, Province, FirmType2, Firm.Age,
         Founding.Year, Founding.Month, Phone_ZIP, ID_imputed, PPI, Employment, def_Wages, 
         def_TOAS, def_FIAS, def_Sales, def_VA, def_VA_IO, def_LP, def_CP, def_LP_IO, 
         def_CP_IO, def_C_com, def_C_com_FI, def_RoC_G_FI, def_TFP, def_TFP_IO, WS, WS_IO) %>%
  group_by(Year) %>%
  mutate(Empl_share = Employment / sum(Employment, na.rm=T),
         VA_share = def_VA / sum(def_VA, na.rm=T),
         TOAS_share = def_TOAS / sum(def_TOAS, na.rm=T),
         FIAS_share = def_FIAS / sum(def_FIAS, na.rm=T),
        ) %>%
  ungroup()

df <- df %>% 
  group_by(Year, Sector.Short) %>%
  mutate(Empl_share_sectoral = Employment / sum(Employment, na.rm=T),
         VA_share_sectoral = def_VA / sum(def_VA, na.rm=T),
         TOAS_share_sectoral = def_TOAS / sum(def_TOAS, na.rm=T),
         FIAS_share_sectoral = def_FIAS / sum(def_FIAS, na.rm=T),
  ) %>%
  ungroup() %>%
  group_by(Year, Sector.ISICR4) %>%
  mutate(Empl_share_ISICR4_sectoral = Employment / sum(Employment, na.rm=T),
         VA_share_ISICR4_sectoral = def_VA / sum(def_VA, na.rm=T),
         TOAS_share_ISICR4_sectoral = def_TOAS / sum(def_TOAS, na.rm=T),
         FIAS_share_ISICR4_sectoral = def_FIAS / sum(def_FIAS, na.rm=T),
  ) %>%
  ungroup()

df <- df %>%
  group_by(ID, Sector) %>%
  mutate(def_Wages_pc = def_Wages / Employment,                     # w

         Employment_diff = Employment - lag(Employment,1),          # N
         def_VA_diff = def_VA - lag(def_VA,1),                      # Wages + profit
         def_VA_IO_diff = def_VA_IO - lag(def_VA_IO,1),             # Y
         def_FIAS_diff = def_FIAS - lag(def_FIAS,1),                # K
         def_TOAS_diff = def_TOAS - lag(def_TOAS,1),                # K
         def_LP_diff = def_LP - lag(def_LP,1),                      # W+p/N
         def_LP_IO_diff = def_LP_IO - lag(def_LP_IO,1),             # Y/N
         def_Sales_diff = def_Sales - lag(def_Sales,1),             # Revenue
         def_Wages_pc_diff = def_Wages_pc - lag(def_Wages_pc,1),    # w
         def_Wages_diff = def_Wages - lag(def_Wages,1),             # W
         WS_diff = WS - lag(WS,1),                                  # W/W+p
         WS_IO_diff = WS_IO - lag(WS_IO,1),                         # W/Y
         
         Empl_share_diff = Empl_share - lag(Empl_share,1),
         VA_share_diff = VA_share - lag(VA_share,1),
         Empl_share_sectoral_diff = Empl_share_sectoral - lag(Empl_share_sectoral,1),
         VA_share_sectoral_diff = VA_share_sectoral - lag(VA_share_sectoral,1),
         Empl_share_ISICR4_sectoral_diff = Empl_share_ISICR4_sectoral - lag(Empl_share_ISICR4_sectoral,1),
         VA_share_ISICR4_sectoral_diff = VA_share_ISICR4_sectoral - lag(VA_share_ISICR4_sectoral,1),
         
         Employment_g   = ifelse(lag(Employment,1)>0, Employment_diff / Employment, NA),      # N
         def_VA_g       = ifelse(lag(Employment,1)>0, def_VA_diff / def_VA, NA),              # Wages + profit
         def_VA_IO_g    = ifelse(lag(Employment,1)>0, def_VA_IO_diff / def_VA_IO, NA),        # Y
         def_FIAS_g     = ifelse(lag(Employment,1)>0, def_FIAS_diff / def_FIAS, NA),          # K
         def_TOAS_g     = ifelse(lag(Employment,1)>0, def_TOAS_diff / def_TOAS, NA),          # K
         def_LP_g       = ifelse(lag(Employment,1)>0, def_LP_diff / def_LP, NA),              # W+p/N
         def_LP_IO_g    = ifelse(lag(Employment,1)>0, def_LP_IO_diff / def_LP_IO, NA),        # Y/N
         def_Sales_g    = ifelse(lag(Employment,1)>0, def_Sales_diff / def_Sales, NA),        # Revenue
         def_Wages_pc_g = ifelse(lag(Employment,1)>0, def_Wages_pc_diff / def_Wages_pc, NA),  # w
         def_Wages_g    = ifelse(lag(Employment,1)>0, def_Wages_diff / def_Wages, NA),        # W
         WS_g           = ifelse(lag(Employment,1)>0, WS_diff / WS, NA),                      # W/W+p
         WS_IO_g        = ifelse(lag(Employment,1)>0, WS_IO_diff / WS_IO, NA),                # W/Y
         
         year_diff = Year - lag(Year, 1),                           # year difference

         Empl_share_g   = ifelse(lag(Empl_share,1)>0, Empl_share_diff / Empl_share, NA),      
         VA_share_g   = ifelse(lag(VA_share,1)>0, VA_share_diff / VA_share, NA),      
         Empl_share_sectoral_g   = ifelse(lag(Empl_share_sectoral,1)>0, Empl_share_sectoral_diff / Empl_share_sectoral, NA),      
         VA_share_sectoral_g   = ifelse(lag(VA_share_sectoral,1)>0, VA_share_sectoral_diff / VA_share_sectoral, NA),      
         Empl_share_ISICR4_sectoral_g   = ifelse(lag(Empl_share_ISICR4_sectoral,1)>0, Empl_share_ISICR4_sectoral_diff / Empl_share_ISICR4_sectoral, NA),      
         VA_share_ISICR4_sectoral_g   = ifelse(lag(VA_share_ISICR4_sectoral,1)>0, VA_share_ISICR4_sectoral_diff / VA_share_ISICR4_sectoral, NA),      
         
  )

save(df, file="08_data_short_consistent.Rda")
load(file="08_data_short_consistent.Rda", verbose=T) # df

df <- df %>%
  group_by(ID, Sector) %>%
  complete(Year = seq(min(Year), max(Year))) 

save(df, file="08_data_complete_panels.Rda")
load(file="08_data_complete_panels.Rda", verbose=T) # df

df <- df %>%
  group_by(ID, Sector) %>%
  mutate(def_Capital_intensity = def_TOAS / Employment,             # CI
         def_Investment_rate = def_TOAS_g,                          # IR
         
         def_Capital_intensity_diff = def_Capital_intensity - dplyr::lag(def_Capital_intensity, 1),
         def_Investment_rate_diff = def_Investment_rate - dplyr::lag(def_Investment_rate, 1),
  )
         

df <- df %>% 
  mutate(Employment_diff_ann = Employment_diff/year_diff,            # N
         def_VA_diff_ann = def_VA_diff / year_diff,                  # Wages + Profit
         def_VA_IO_diff_ann = def_VA_IO_diff / year_diff,            # Y
         def_FIAS_diff_ann = def_FIAS_diff / year_diff,              # K
         def_TOAS_diff_ann = def_TOAS_diff / year_diff,              # K
         def_LP_diff_ann = def_LP_diff / year_diff,                  # W+p/N
         def_LP_IO_diff_ann = def_LP_IO_diff / year_diff,            # Y/N
         def_Sales_diff_ann = def_Sales_diff / year_diff,            # Revenue
         def_Wages_pc_diff_ann = def_Wages_pc_diff / year_diff,      # w
         def_Wages_diff_ann = def_Wages_diff / year_diff,            # W
         WS_diff_ann = WS_diff / year_diff,                          # W/W+p
         WS_IO_diff_ann = WS_IO_diff / year_diff,                    # W/Y
         
         def_Capital_intensity_diff_ann = def_Capital_intensity_diff / year_diff,  # CI
         def_Investment_rate_diff_ann = def_Investment_rate_diff / year_diff,      # IR
  
         Employment_g_ann = (Employment_g)**(1/year_diff),           # N
         def_VA_g_ann = (def_VA_g)**(1/year_diff),                   # Wages + Profit
         def_VA_IO_g_ann = (def_VA_IO_g)**(1/year_diff),             # Y
         def_FIAS_g_ann = (def_FIAS_g)**(1/year_diff),               # K
         def_TOAS_g_ann = (def_TOAS_g)**(1/year_diff),               # K
         def_LP_g_ann = (def_LP_g)**(1/year_diff),                   # W+p/N
         def_LP_IO_g_ann = (def_LP_IO_g)**(1/year_diff),             # Y/N
         def_Sales_g_ann = (def_Sales_g)**(1/year_diff),              # Revenue
         def_Wages_pc_g_ann = (def_Wages_pc_g)**(1/year_diff),       # w
         def_Wages_g_ann = (def_Wages_g)**(1/year_diff),             # W
         WS_g_ann = (WS_g)**(1/year_diff),                           # W/W+p
         WS_IO_g_ann = (WS_IO_g)**(1/year_diff),                     # W/Y
         
         Empl_share_g_ann = (Empl_share_g)**(1/year_diff),
         VA_share_g_ann = (VA_share_g)**(1/year_diff),
         Empl_share_sectoral_g_ann = (Empl_share_sectoral_g)**(1/year_diff),      
         VA_share_sectoral_g_ann = (VA_share_sectoral_g)**(1/year_diff),      
         Empl_share_ISICR4_sectoral_g_ann = (Empl_share_ISICR4_sectoral_g)**(1/year_diff),      
         VA_share_ISICR4_sectoral_g_ann = (VA_share_ISICR4_sectoral_g)**(1/year_diff),      
         
  )

save(df, file="08_data_complete_panels_annualized.Rda")
load(file="08_data_complete_panels_annualized.Rda", verbose=T)
