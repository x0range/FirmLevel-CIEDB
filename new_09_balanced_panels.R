library(pacman)
#pacman::p_load(dplyr, tidyr, hexbin, ggplot2, reshape2, ggrepel)
pacman::p_load(dplyr, tidyr)

format_panel3_df <- function (df, new_colnames=c(1999:2013)) {
  rownames(df) <- df$Sector.Short
  df$Sector.Short <- NULL
  colnames(df) <- new_colnames
  df <- t(df)
  return(df)
}

balanced_panel_computation_by_sector_scheme <- function(df, sector_scheme="GB2002") {
  message(paste("Commencing", sector_scheme))
  if (sector_scheme=="Province") {
    isISIC = "Province"
  } else if (sector_scheme=="largestProvince_ISICR4") {
    isISIC = "largestProvince_ISICR4"
  } else if (sector_scheme=="ISICR4") {
    isISIC = "ISICR4"
  } else if (sector_scheme=="GB2002") {
    isISIC = ""
  } # all other sector schemes are not supported and will fail to save as isISIC is not defined
  consistent_panels <- list()
  consistent_panel_years <- list()
  consistent_panel_sizes <- list()
  sectoral_panels <- list()
  sectoral_growth <- list()
  
  
  years <- sort(unique(df$Year))
  years <- years[c(2:length(years))]
  
  if (sector_scheme=="ISICR4"|sector_scheme=="largestProvince_ISICR4") {
    df$Sector.Short <- df$Sector.ISICR4 
    print(table(is.na(df$Sector.Short)))
    df[is.na(df$Sector.Short),]$Sector.Short <- ""
    print(table(is.na(df$Sector.Short)))
  } else if (sector_scheme=="Province") {
    # This treats Provinces as Sectors for convenience
    df$Sector.Short <- df$Province.Code
  }
  df$Sector.ISICR4 <- NULL
  
  if (sector_scheme=="largestProvince_ISICR4") {
    print(nrow(df))
    df = df[df["Province.Code"]==31,]                          # Zhejiang
    print(nrow(df))
  }
  
  for (i in 1:length(years)) {
    yr <- years[[i]]
    
    panel <- df %>%  
      group_by(ID, Sector) %>%
      filter(Year == yr-1 | Year == yr) %>%
      filter(n()==2)
    
    consistent_panels[[i]] <- panel
    consistent_panel_years[[i]] <- yr
    consistent_panel_sizes[[i]] <- nrow(panel)
    
    panel[panel["Firm.Age"]>800&!is.na(panel["Firm.Age"]),"Firm.Age"] <- NA
    panel2 <- panel %>%
      ungroup() %>%
      group_by(Year, Sector.Short) %>%
      summarize(Employment = sum(Employment, na.rm=T),
                def_Wages = sum(def_Wages, na.rm=T),
                def_TOAS = sum(def_TOAS, na.rm=T),
                def_FIAS = sum(def_FIAS, na.rm=T),
                #def_Sales = sum(def_Sales, na.rm=T),
                def_VA = sum(def_VA, na.rm=T),
                def_VA_IO = sum(def_VA_IO, na.rm=T),
                def_LP = mean(def_LP, na.rm=T),
                def_LP_IO = mean(def_LP_IO, na.rm=T),
                #def_CP = mean(def_CP, na.rm=T),
                #def_CP_IO = mean(def_CP_IO, na.rm=T),
                #def_TFP = mean(def_TFP, na.rm=T),
                #def_TFP_IO = mean(def_TFP_IO, na.rm=T),
                #def_C_com = mean(def_C_com, na.rm=T),
                Avg.Emplogment_g = mean(Employment_g, na.rm=T),
                Avg.def_VA_g = mean(def_VA_g, na.rm=T),
                Avg.Firm.Age = mean(Firm.Age, na.rm=T),
                Number = n(),
      ) %>%
      mutate(agg_LP = def_VA / Employment,
             agg_LP_IO = def_VA_IO / Employment,
      )
    
    panel2[panel2["def_VA"]==0,"agg_LP"] <- NA
    panel2[panel2["def_VA_IO"]==0,"agg_LP_IO"] <- NA
    
    sectoral_panels[[i]] <- panel2
    
    sum_Employment_first <- sum(panel2[panel2["Year"]==yr-1, "Employment"])
    sum_Employment_second <- sum(panel2[panel2["Year"]==yr, "Employment"])
    sum_VA_first <- sum(panel2[panel2["Year"]==yr-1, "def_VA"])
    sum_VA_second <- sum(panel2[panel2["Year"]==yr, "def_VA"])
    
    panel3 <- panel2 %>%
      ungroup() %>%
      group_by(Sector.Short) %>%
      summarize(Employment_share_first = first(Employment)/sum_Employment_first,
                Employment_share_second = last(Employment)/sum_Employment_second,
                VA_share_first = first(def_VA) / sum_VA_first,
                VA_share_second = last(def_VA) / sum_VA_second,
                LP_diff = last(agg_LP) - first(agg_LP),
                LP_IO_diff = last(agg_LP_IO) - first(agg_LP_IO),
                #Firm.Age.diff = last(Avg.Firm.Age) - first(Avg.Firm.Age),
                Firm_Age_g = (last(Avg.Firm.Age) - first(Avg.Firm.Age)) / first(Avg.Firm.Age),
                Number = first(Number),
      ) %>%
      mutate(Employment_share_growth = (Employment_share_second - Employment_share_first) / Employment_share_first,
             VA_share_growth = (VA_share_second - VA_share_first) / VA_share_first,
      )
    
    sectoral_growth[[i]] <- panel3
    
    if (sector_scheme=="Province") {
      retain_when_merge = T
    } else {
      retain_when_merge = F
    }

    E_s1 <- panel3[,c("Sector.Short", "Employment_share_first")]
    VA_s1 <- panel3[,c("Sector.Short", "VA_share_first")]
    E_s2 <- panel3[,c("Sector.Short", "Employment_share_second")]
    VA_s2 <- panel3[,c("Sector.Short", "VA_share_second")]
    
    E_g <- panel3[,c("Sector.Short", "Employment_share_growth")]
    VA_g <- panel3[,c("Sector.Short", "VA_share_growth")]
    LP_diff <- panel3[,c("Sector.Short", "LP_diff")]
    LP_IO_diff <- panel3[,c("Sector.Short", "LP_IO_diff")]
    #FA_diff <- panel3[,c("Sector.Short", "Firm.Age.diff")]
    FA_g <- panel3[,c("Sector.Short", "Firm_Age_g")]

    if (i==1) {
      df_E_g <- E_g
      df_VA_g <- VA_g
      df_LP_diff <- LP_diff
      df_LP_IO_diff <- LP_IO_diff
      #df_FA_diff <- FA_diff
      df_FA_g <- FA_g
      df_E_s <- E_s1
      df_VA_s <- VA_s1
    } else {
      df_E_g <- merge(df_E_g, E_g, by="Sector.Short", all=retain_when_merge)
      df_VA_g <- merge(df_VA_g, VA_g, by="Sector.Short", all=retain_when_merge)
      df_LP_diff <- merge(df_LP_diff, LP_diff, by="Sector.Short", all=retain_when_merge)
      df_LP_IO_diff <- merge(df_LP_IO_diff, LP_IO_diff, by="Sector.Short", all=retain_when_merge)
      #df_FA_diff <- merge(df_FA_diff, FA_diff, by="Sector.Short", all=retain_when_merge)
      df_FA_g <- merge(df_FA_g, FA_g, by="Sector.Short", all=retain_when_merge)
    }
    df_E_s <- merge(df_E_s, E_s2, by="Sector.Short", all=retain_when_merge)
    df_VA_s <- merge(df_VA_s, VA_s2, by="Sector.Short", all=retain_when_merge)
  }
  

  if (sector_scheme=="Province") {
    df_E_g[df_E_g==Inf] <- 1.0
    df_FA_g[df_FA_g==Inf] <- 1.0
    df_VA_g[df_VA_g==Inf] <- 1.0
    df_E_g[df_E_g==-1] <- 1.0
    df_FA_g[df_FA_g==-1] <- 1.0
    df_VA_g[df_VA_g==-1] <- 1.0
  } 
  
  df_E_g <- format_panel3_df(df_E_g)
  df_VA_g <- format_panel3_df(df_VA_g)
  df_LP_diff <- format_panel3_df(df_LP_diff)
  df_LP_IO_diff <- format_panel3_df(df_LP_IO_diff)
  df_FA_g <- format_panel3_df(df_FA_g)
  
  df_E_s <- format_panel3_df(df_E_s, new_colnames=c(1998:2013))
  df_VA_s <- format_panel3_df(df_VA_s, new_colnames=c(1998:2013))
  
  
  save(consistent_panels, consistent_panel_years, consistent_panel_sizes, sectoral_panels, sectoral_growth, file = paste("09_", isISIC, "_data_panels_list.Rda", sep=""))
  save(df_E_g, df_VA_g, df_LP_diff, df_LP_IO_diff, df_FA_g, df_E_s, df_VA_s, file = paste("09_", isISIC, "_sectoral_change.Rda", sep=""))
  save(df_E_s, df_VA_s, file = paste("09_", isISIC, "_sectoral_shares_from_balanced.Rda", sep=""))
  load(paste("09_", isISIC, "_data_panels_list.Rda", sep=""), verbose=T)
  load(paste("09_", isISIC, "_sectoral_change.Rda", sep=""), verbose=T) # df_E_g, df_VA_g, df_LP_diff, df_LP_IO_diff, df_FA_g, df_E_s, df_VA_s,
  load(paste("09_", isISIC, "_sectoral_shares_from_balanced.Rda", sep=""), verbose=T) # df_E_s, df_VA_s
}



# main entry point

setwd("~/datalake/CIEDB_2009_2013/")
load(file="08_data_short_consistent.Rda", verbose=T)   # df

balanced_panel_computation_by_sector_scheme(df, "ISICR4")
balanced_panel_computation_by_sector_scheme(df, "GB2002")
balanced_panel_computation_by_sector_scheme(df, "Province")
balanced_panel_computation_by_sector_scheme(df, "largestProvince_ISICR4")
