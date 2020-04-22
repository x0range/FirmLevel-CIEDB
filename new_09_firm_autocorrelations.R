library(pacman)
pacman::p_load(dplyr,tidyr,purrr,npsm,ggplot2)


correlation_with_ci <- function(x, y) {
  dfx <- data.frame("x"=c(x[[1]]), "y"=c(y[[1]]))
  #print(dfx)
  x <- NULL
  y <- NULL
  dfx <- na.omit(dfx)
  #print(dfx)
  rn <- nrow(dfx)
  if (rn<1) {
    rc <- NA
    ri <- NA
  } else {
    rc <- cor(dfx$x, dfx$y, method="pearson", use="pairwise.complete.obs")
    # Uses apparently too much memory; so we go without it
    #ri <- try(cor.boot.ci(dfx$x, dfx$y, method="pearson", conf = 0.95, nbs = 3000))
    ri <- NA
  }
  if (length(ri)<2) {
    ri <- c(NA, NA)
  }
  return(list(n=rn, coeff=rc, low=ri[[1]], high=ri[[2]]))
}

plot_ac <- function(plot_df, ylabel, the_filename, plot_layout=NA) {
  if (is.na(plot_layout)) {
    plot_layout = c(0.0,  # ymin
                    1.0,  # ymax
                    0.6,  # inlay ymin
                    1.0,  # inlay ymax
                    0.5,  # inlay xmin
                    6.0)  # inlay xmax
  }
  #print(plot_df)
  
  inner_plot <- ggplot(data=plot_df, aes(x=lag, y=n)) + geom_line(colour="blue") +
    xlab("Lag") +
    ylab("# Obs.") +
    theme_bw() +
    theme(axis.text = element_text(size=12), axis.title=element_text(size=14),
          legend.title=element_text(size=14), legend.text=element_text(size=12)) 
  
  the_plot <- ggplot(data=plot_df, aes(x=lag, y=coeff)) + 
    geom_line(colour="blue") +
    #geom_ribbon(aes(ymin=plot_df$low, ymax=plot_df$high), colour="blue", fill="blue", linetype=2, alpha=0.1) +
    xlab("Lag") +
    ylab(ylabel) +
    ylim(plot_layout[[1]], plot_layout[[2]]) +
    theme_bw() +
    theme(axis.text = element_text(size=12), axis.title=element_text(size=14),
          legend.title=element_text(size=14), legend.text=element_text(size=12)) +
    #theme_minimal()+ # minimal theme
    #theme(axis.text.x = element_text(size = 12),
    #      axis.text.y = element_text(size = 12),
    #)
    annotation_custom(
      ggplotGrob(inner_plot), 
      ymin = plot_layout[[3]], ymax = plot_layout[[4]], xmin = plot_layout[[5]], xmax = plot_layout[[6]]
    )
  print(the_plot)
  ggsave(the_filename, the_plot)
}


get_ac <- function(df_long, var_col="var") {
  #df_long <- do.call(data.frame,lapply(df_long, function(x) replace(x, is.infinite(x),NA)))
  #print(head(df_long))
  df_long <- df_long %>% 
    ungroup() %>%
    select(c("Year", "ID", "Sector.Short", var_col))
  #select(get(year_col), get(idx_col), get(var_col))
  #print(head(df_long))
  
  colnames(df_long) <- c("Year", "ID", "Sector.Short", "var")
  
  df_long[is.nan(unlist(df_long[,"var"])),"var"] <- NA
  df_long[is.infinite(unlist(df_long[,"var"])),"var"] <- NA
  #df_long[is.nan(df_long[,"var"]),"var"] <- NA
  #df_long[is.infinite(df_long[,"var"]),"var"] <- NA
  #print(head(df_long))
  
  cordf <- df_long %>%
    dplyr::arrange(ID, Sector.Short, Year) %>%
    dplyr::group_by(ID, Sector.Short) %>%
    dplyr::mutate(
      lag_1  = dplyr::lag(var, 1),
      lag_2  = dplyr::lag(var, 2),
      lag_3  = dplyr::lag(var, 3),
      lag_4  = dplyr::lag(var, 4),
      lag_5  = dplyr::lag(var, 5),
      lag_6  = dplyr::lag(var, 6),
      lag_7  = dplyr::lag(var, 7),
      lag_8  = dplyr::lag(var, 8),
      lag_9  = dplyr::lag(var, 9),
      lag_10 = dplyr::lag(var, 10),
      lag_11 = dplyr::lag(var, 11),
      lag_12 = dplyr::lag(var, 12),
      lag_13 = dplyr::lag(var, 13),
      lag_14 = dplyr::lag(var, 14),
    )
  
  acdf = data.frame("lag" = c(), "coeff" = c(), "low" = c(), "high" = c(), "n" = c()) 
  for (lag in 1:14) {
    #print(lag)
    col = lag + 4
    rv = correlation_with_ci(cordf[,4], cordf[,col])
    this_row = data.frame("lag" = c(lag), "coeff" = c(rv$coeff), "low" = c(rv$low), "high" = c(rv$high), "n" = c(rv$n)) 
    #print(this_row)
    acdf <- rbind(acdf, this_row)  
  }
  
  return(acdf)
}



setwd("~/datalake/CIEDB_2009_2013/")
load(file="08_data_complete_panels_annualized.Rda", verbose=T)  # df

if (F) {  #test data
  df <- dplyr::data_frame(ID = sample(c("a", "b", "c"), size = 100, replace = T), 
                          Sector.Short = sample(c("06", "07"), size = 100, replace = T), 
                          Year = sample(seq(1980:2015), size = 100, replace = T), 
                          Empl_share = sample.int(30, size = 100, replace = T)) 
  df <- df %>%
    arrange(ID, Sector.Short, Year) %>%
    mutate(VA_share = cumsum(Empl_share),
           Empl_share_sectoral = cumsum(Empl_share),
           VA_share_sectoral = cumsum(Empl_share),
    )
}

# reduce data size
df <- df %>% 
  ungroup() %>%
  arrange(ID, Year) %>%
  select(ID, Year, Sector.Short, Empl_share, VA_share, Empl_share_sectoral, VA_share_sectoral)


acdf_VA_ss <- get_ac(df, var_col="VA_share_sectoral")
acdf_VA_s <- get_ac(df, var_col="VA_share")
acdf_E_ss <- get_ac(df, var_col="Empl_share")
acdf_E_s <- get_ac(df, var_col="Empl_share_sectoral")

save(acdf_E_s, acdf_VA_s, acdf_E_ss, acdf_VA_ss, file="09_firm_level_autocorrelations.Rda")
load("09_firm_level_autocorrelations.Rda", verbose=T) # acorrs

#ylabel="Autocorrelation", the_filename="plot.pdf", plot_layout=NA
plot_ac(plot_df=acdf_E_s, ylabel="Autocorrelation Employment Shares", the_filename="09_firmlevel_autocorr_E_s.pdf")
plot_ac(plot_df=acdf_E_ss, ylabel="Autocorrelation Employment Shares (Sectoral)", the_filename="09_firmlevel_autocorr_E_ss.pdf")
plot_ac(plot_df=acdf_VA_s, ylabel="Autocorrelation VA Shares", the_filename="09_firmlevel_autocorr_VA_s.pdf", plot_layout=c(0.0, 1.0, 0.05, 0.45, 0.5, 6.0))
plot_ac(plot_df=acdf_VA_ss, ylabel="Autocorrelation VA Shares (Sectoral)", the_filename="09_firmlevel_autocorr_VA_ss.pdf")



if (F) {
acorrs <- df %>%
  tidyr::nest(data=c(Empl_share, VA_share, Empl_share_sectoral, VA_share_sectoral)) %>%
  dplyr::mutate(acorr_result_E = purrr::map(data, ~ acf(.x$Empl_share, na.action=na.pass, plot = F)),
                acorr_Empl = purrr::map(acorr_result_E, ~ drop(.x$acf)),
                acorr_result_V = purrr::map(data, ~ acf(.x$VA_share, na.action=na.pass, plot = F)),
                acorr_VA =  purrr::map(acorr_result_V, ~ drop(.x$acf)),
                acorr_result_ES = purrr::map(data, ~ acf(.x$Empl_share_sectoral, na.action=na.pass, plot = F)),
                acorr_Empl_sectoral = purrr::map(acorr_result_ES, ~ drop(.x$acf)),
                acorr_result_VS = purrr::map(data, ~ acf(.x$VA_share_sectoral, na.action=na.pass, plot = F)),
                acorr_VA_sectoral = purrr::map(acorr_result_VS, ~ drop(.x$acf)),
                ) %>%
  tidyr::unnest(acorr_Empl, acorr_VA, acorr_Empl_sectoral, acorr_VA_sectoral) %>%
  dplyr::select(ID, Sector.Short, acorr_Empl, acorr_VA, acorr_Empl_sectoral, acorr_VA_sectoral) %>%
  dplyr::group_by(ID, Sector.Short) %>%
  dplyr::mutate(lag = seq(0, n() - 1)) 

save(acorrs, file="09_firm_level_autocorrelations.Rda")
load("09_firm_level_autocorrelations.Rda", verbose=T) # acorrs

acorrs_sectoral <- acorrs %>% 
  dplyr::group_by(Sector.Short, lag) %>%
  dplyr::summarise(Empl_mean = mean(acorr_Empl, na.rm=T),
                   Empl_median = quantile(acorr_Empl, 0.5, na.rm=T),
                   Empl_q25 = quantile(acorr_Empl, 0.25, na.rm=T),
                   Empl_q75 = quantile(acorr_Empl, 0.75, na.rm=T),
                   VA_mean = mean(acorr_VA, na.rm=T),
                   VA_median = quantile(acorr_VA, 0.5, na.rm=T),
                   VA_q25 = quantile(acorr_VA, 0.25, na.rm=T),
                   VA_q75 = quantile(acorr_VA, 0.75, na.rm=T),
                   Empl_sectoral_mean = mean(acorr_Empl_sectoral, na.rm=T),
                   Empl_sectoral_median = quantile(acorr_Empl_sectoral, 0.5, na.rm=T),
                   Empl_sectoral_q25 = quantile(acorr_Empl_sectoral, 0.25, na.rm=T),
                   Empl_sectoral_q75 = quantile(acorr_Empl_sectoral, 0.75, na.rm=T),
                   VA_sectoral_mean = mean(acorr_VA_sectoral, na.rm=T),
                   VA_sectoral_median = quantile(acorr_VA_sectoral, 0.5, na.rm=T),
                   VA_sectoral_q25 = quantile(acorr_VA_sectoral, 0.25, na.rm=T),
                   VA_sectoral_q75 = quantile(acorr_VA_sectoral, 0.75, na.rm=T),
                   N = n(),
  )

acorrs_all <- acorrs %>% 
  dplyr::group_by(lag) %>%
  dplyr::summarise(Empl_mean = mean(acorr_Empl, na.rm=T),
                   Empl_median = quantile(acorr_Empl, 0.5, na.rm=T),
                   Empl_q25 = quantile(acorr_Empl, 0.25, na.rm=T),
                   Empl_q75 = quantile(acorr_Empl, 0.75, na.rm=T),
                   VA_mean = mean(acorr_VA, na.rm=T),
                   VA_median = quantile(acorr_VA, 0.5, na.rm=T),
                   VA_q25 = quantile(acorr_VA, 0.25, na.rm=T),
                   VA_q75 = quantile(acorr_VA, 0.75, na.rm=T),
                   Empl_sectoral_mean = mean(acorr_Empl_sectoral, na.rm=T),
                   Empl_sectoral_median = quantile(acorr_Empl_sectoral, 0.5, na.rm=T),
                   Empl_sectoral_q25 = quantile(acorr_Empl_sectoral, 0.25, na.rm=T),
                   Empl_sectoral_q75 = quantile(acorr_Empl_sectoral, 0.75, na.rm=T),
                   VA_sectoral_mean = mean(acorr_VA_sectoral, na.rm=T),
                   VA_sectoral_median = quantile(acorr_VA_sectoral, 0.5, na.rm=T),
                   VA_sectoral_q25 = quantile(acorr_VA_sectoral, 0.25, na.rm=T),
                   VA_sectoral_q75 = quantile(acorr_VA_sectoral, 0.75, na.rm=T),
                   N = n(),
  )

save(acorrs_sectoral, acorrs_all, file="09_firm_level_autocorrelation_summary.Rda")
load("09_firm_level_autocorrelation_summary.Rda", verbose=T) # acorrs_sectoral, acorrs_all

print_frame <- acorrs_all %>%
  select(lag, Empl_mean, VA_mean, Empl_sectoral_mean, VA_sectoral_mean, N)
print(print_frame, n=15)

print_frame <- acorrs_sectoral %>%
  select(Sector.Short, lag, Empl_mean, VA_mean, Empl_sectoral_mean, VA_sectoral_mean, N)
print(print_frame[print_frame$lag<3,], n=125)
}

