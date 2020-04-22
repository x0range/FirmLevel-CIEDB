library(pacman)
pacman::p_load(dplyr, reshape2, ggplot2, directlabels)


sector_year_cast <- function(sectoral_df, var="nHHI_TOAS", columns_restriction=c()) {
  long <- sectoral_df %>%
    select(Year, Sector.Short, !!var)
  wide <- dcast(long, Year~Sector.Short)
  #print(wide)
  if (length(columns_restriction)>0) {
    wide <- wide[, c("Year", columns_restriction)]
  }
  rownames(wide) <- wide$Year
  long <- melt(wide, id.vars=c("Year"))
  wide$Year <- NULL
  colnames(long) <- c("Year", "Sector", "value")
  return(list("wide"=wide, "long"=long))
}

plot_sectoral_dev <- function(df, var, label, filename_infix="", logscale=F, columns_restriction=c()) {
  long_df = sector_year_cast(df, var, columns_restriction=columns_restriction)$long
  the_plot <- ggplot(long_df, aes(x=Year, y=value, group=Sector, color=Sector)) + 
    theme_bw() +
    theme(axis.text = element_text(size=16), axis.title=element_text(size=18)) +
    geom_line() + 
    ylab(label) +
    scale_colour_discrete(guide = 'none') +
    #scale_x_discrete(expand=c(0, 1)) +
    geom_dl(aes(label = Sector), method = list(dl.combine("first.points", "last.points"), cex = 1.3)) 
  if (logscale) {
    the_plot <- the_plot +
      scale_y_continuous(trans='log10')
  }
  print(the_plot)
  big = ifelse(length(columns_restriction)>0,"_BG","")
  ggsave(paste("09", filename_infix, "_Sectoral_Accounts_", big, var, ".pdf",sep=""), plot=the_plot)
}


computations_by_sector_scheme <- function(df, sector_scheme="GB2002") {
  if (sector_scheme=="GB2002") {
    filename_infix <- ""
  } else if (sector_scheme=="ISICR4") {
    filename_infix <- "_ISICR4"
    df$Sector.Short <- df$Sector.ISICR4
  } else {
    # fail
    return(NA)
  }
  df$Sector.ISICR4 <- NULL
  
  # reduce data size
  df <- df %>% 
    ungroup() %>%
    select(ID, Sector.Short, Year, Empl_share_sectoral, VA_share_sectoral, 
           #TOAS_share_sectoral, FIAS_share_sectoral,
           Firm.Age, Employment, def_Wages, def_VA, def_VA_IO, def_TOAS, def_FIAS) 
  
  # accounts summary by sector
  sectoral <- df %>%
    #arrange(Year, Sector.Short, ID) %>%
    group_by(Year, Sector.Short) %>%
    mutate(
      FIAS_share_sectoral = def_FIAS / sum(def_FIAS, na.rm=T),
      TOAS_share_sectoral = def_TOAS / sum(def_TOAS, na.rm=T),
    ) %>%
    summarize(N = n(),
              HHI_Empl = ifelse(sum(Empl_share_sectoral, na.rm=T)==0, NA, sum(Empl_share_sectoral**2, na.rm=T)),
              Entropy_Empl = ifelse(sum(Empl_share_sectoral, na.rm=T)==0, NA, -1 * sum(Empl_share_sectoral * log(Empl_share_sectoral), na.rm=T)),
              HHI_VA = ifelse(sum(VA_share_sectoral, na.rm=T)<=0, NA, sum(VA_share_sectoral**2, na.rm=T)),
              Entropy_VA = ifelse(sum(VA_share_sectoral, na.rm=T)<=0, NA, -1 * sum(VA_share_sectoral * log(VA_share_sectoral), na.rm=T)),
              HHI_FIAS = ifelse(sum(FIAS_share_sectoral, na.rm=T)==0, NA, sum(FIAS_share_sectoral**2, na.rm=T)),
              Entropy_FIAS = ifelse(sum(FIAS_share_sectoral, na.rm=T)==0, NA, -1 * sum(FIAS_share_sectoral * log(FIAS_share_sectoral), na.rm=T)),
              HHI_TOAS = ifelse(sum(TOAS_share_sectoral, na.rm=T)==0, NA, sum(TOAS_share_sectoral**2, na.rm=T)),
              Entropy_TOAS = ifelse(sum(TOAS_share_sectoral, na.rm=T)==0, NA, -1 * sum(TOAS_share_sectoral * log(TOAS_share_sectoral), na.rm=T)),
              Avg.Firm_Age = mean(Firm.Age, na.rm=T),
              Employment = sum(Employment, na.rm=T),
              Wage_Bill = sum(def_Wages, na.rm=T),
              VA = sum(def_VA, na.rm=T),
              VA_IO = sum(def_VA_IO, na.rm=T),
              TOAS = sum(def_TOAS, na.rm=T),
              FIAS = sum(def_FIAS, na.rm=T),
    ) %>%
    group_by(Year) %>%
    mutate(
      # fucking sums
      sum_N = sum(N),
      sum_Employment = sum(Employment),
      sum_VA = sum(VA),
      sum_VA_IO = sum(VA_IO),
      sum_TOAS = sum(TOAS),
      sum_FIAS = sum(FIAS),
      sum_Wage_Bill = sum(Wage_Bill),
      # shares
      Firms_share = N / sum_N,
      Employment_share = Employment / sum_Employment,
      #VA_share = ifelse(sum_VA>0, VA / sum_VA, NA),
      VA_share = VA / sum_VA,
      VA_IO_share = ifelse(sum_VA_IO>0, VA_IO / sum_VA_IO, NA),
      TOAS_share = TOAS / sum_TOAS,
      FIAS_share = FIAS / sum_FIAS,
      Wage_Bill_share = ifelse(sum_Wage_Bill>0, Wage_Bill / sum_Wage_Bill, NA),
      # sectoral sample ratios
      LP_agg = ifelse(VA!=0&Employment>0, VA / Employment, NA),
      LP_IO_agg = ifelse(VA_IO!=0&Employment>0, VA_IO / Employment, NA),
      Avg.wages = ifelse(Wage_Bill>0&Employment>0, Wage_Bill / Employment, NA),
      WS = ifelse(VA!=0&Wage_Bill>0, Wage_Bill / VA, NA),
      WS_IO = ifelse(VA_IO!=0&Wage_Bill>0, Wage_Bill / VA_IO, NA),
      # normalized HHI
      nHHI_Empl = (HHI_Empl-(1/N))/(1-(1/N)),
      nHHI_VA = (HHI_VA-(1/N))/(1-(1/N)),
      nHHI_TOAS = (HHI_TOAS-(1/N))/(1-(1/N)),
      nHHI_FIAS = (HHI_FIAS-(1/N))/(1-(1/N)),
    ) %>%
    filter(!is.na(Sector.Short) & Sector.Short!="")
  
  
  # Print selection of df (not generally required)
  if (exists("do_print")) {
    if (do_print==TRUE) {
      print_frame <- sectoral %>%
        select(Sector.Short, Year, N, nHHI_Empl, nHHI_VA, nHHI_TOAS, nHHI_FIAS, Entropy_Empl, Entropy_VA, Entropy_TOAS, Entropy_FIAS, Firms_share, Employment_share, VA_share, LP_agg, Avg.wages, Avg.Firm_Age, WS)
      print(print_frame, n=25)
      #print_frame <- sectoral %>%
      #  select(Sector.Short, Year, nHHI_TOAS, Entropy_TOAS, Firms_share, Employment_share, VA_share, LP_agg, Avg.wages, Avg.Firm_Age, WS)
      #print(print_frame, n=25)
    }
  }
  
  save(sectoral, file=paste("09", filename_infix, "_sectoral_accounts.Rda", sep=""))
  load(paste("09", filename_infix, "_sectoral_accounts.Rda", sep=""), verbose=T) #sectoral
  
  
  # Compute sectoral monopolization/entropy/competitiveness/dispersion
  rv = sector_year_cast(sectoral, "nHHI_TOAS")
  nHHI_TOAS_long = rv$long
  nHHI_TOAS_wide = rv$wide
  rv = sector_year_cast(sectoral, "Entropy_TOAS")
  Entropy_TOAS_long = rv$long
  Entropy_TOAS_wide = rv$wide
  
  save(nHHI_TOAS_wide, Entropy_TOAS_wide, nHHI_TOAS_long, Entropy_TOAS_long, file=paste("09", filename_infix, "_sectoral_monopolization.Rda", sep=""))
  load(paste("09", filename_infix, "_sectoral_monopolization.Rda", sep=""), verbose=T) #nHHI_TOAS_wide, Entropy_TOAS_wide, nHHI_TOAS_long, Entropy_TOAS_long
  
  
  # Plot development of some variables
  if (sector_scheme=="GB2002") {
    big_sectors = c("06", "13", "17", "18", "19", "26", "30", "31", "32", "34", "35", "36", "37", "44")
  } else if (sector_scheme=="ISICR4") {
    big_sectors = c("B", "C10-C12", "C13-C15", "C20", "C22", "C23", "C24", "C25", "C28", "C29", "C30", "D35")
  }
  sectoral$Avg_Firm_Age <- sectoral$Avg.Firm_Age
  
  plot_sectoral_dev(sectoral, "nHHI_TOAS", "HHI", filename_infix=filename_infix, logscale=T, columns_restriction=big_sectors)
  plot_sectoral_dev(sectoral, "Entropy_TOAS", "Entropy", filename_infix=filename_infix, logscale=F, columns_restriction=big_sectors)
  plot_sectoral_dev(sectoral, "Employment_share", "Employment share", filename_infix=filename_infix, logscale=F, columns_restriction=big_sectors)
  plot_sectoral_dev(sectoral, "VA_share", "Value added share", filename_infix=filename_infix, logscale=F, columns_restriction=big_sectors)
  plot_sectoral_dev(sectoral, "Avg_Firm_Age", "Average firm age", filename_infix=filename_infix, logscale=T, columns_restriction=big_sectors)
  plot_sectoral_dev(sectoral, "Avg_Firm_Age", "Average firm age", filename_infix=filename_infix, logscale=T)
  
  plot_sectoral_dev(sectoral, "nHHI_TOAS", "HHI", filename_infix=filename_infix, logscale=T)
  plot_sectoral_dev(sectoral, "Entropy_TOAS", "Entropy", filename_infix=filename_infix, logscale=F)
  
  
  # not executed
  if (F) {
    sectoral <- sectoral %>%
      ungroup() %>%
      group_by(Sector.Short) %>%
      arrange(Year) %>%
      mutate(year_diff = Year -lag(Year, 1),
             Employment_share_g = ifelse(year_diff==1, (Employment_share - lag(Employment_share,1)) / lag(Employment_share,1), NA),
      )
  }
  
}

# main entry point

setwd("~/datalake/CIEDB_2009_2013/")
load(file="08_data_complete_panels_annualized.Rda", verbose=T)  # df

computations_by_sector_scheme(df, sector_scheme="GB2002")
computations_by_sector_scheme(df, sector_scheme="ISICR4")
