library(pacman)
pacman::p_load(reshape2, dplyr, npsm, ggplot2, egg)


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
    ri <- try(cor.boot.ci(dfx$x, dfx$y, method="pearson", conf = 0.95, nbs = 3000))
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
                    0.0,  # inlay ymin
                    0.4,  # inlay ymax
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
    geom_ribbon(aes(ymin=plot_df$low, ymax=plot_df$high), colour="blue", fill="blue", linetype=2, alpha=0.1) +
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

plot_macro_dispersion <- function(plot_df, ylabel, the_filename) {
  plot_df_long <- melt(plot_df, index.vars="Year")
  colnames(plot_df_long) <- c("Year", "Variable", "var")
  the_plot <- ggplot(data=plot_df_long, aes(x=Year, y=var, group=Variable, colour=Variable)) + 
    geom_point() +
    geom_line() +
    xlab("Year") +
    ylab(ylabel) +
    theme_bw() +
    theme(axis.text = element_text(size=12), axis.title=element_text(size=14),
          legend.title=element_text(size=14), legend.text=element_text(size=12)) 
  print(the_plot)
  ggsave(the_filename, the_plot)
}

get_ac <- function(df_long, year_col="Year", idx_col="index", var_col="var", ylabel="Autocorrelation", the_filename="plot.pdf", plot_layout=NA) {
  #df_long <- do.call(data.frame,lapply(df_long, function(x) replace(x, is.infinite(x),NA)))
  #print(head(df_long))
  df_long <- df_long %>% 
    ungroup() %>%
    select(year_col, idx_col, var_col)
  #select(get(year_col), get(idx_col), get(var_col))
  #print(head(df_long))
  
  colnames(df_long) <- c("Year", "index", "var")
  
  df_long[is.nan(unlist(df_long[,"var"])),"var"] <- NA
  df_long[is.infinite(unlist(df_long[,"var"])),"var"] <- NA
  #df_long[is.nan(df_long[,"var"]),"var"] <- NA
  #df_long[is.infinite(df_long[,"var"]),"var"] <- NA
  #print(head(df_long))
  
  cordf <- df_long %>%
    dplyr::arrange(index, Year) %>%
    dplyr::group_by(index) %>%
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
    col = lag + 3
    rv = correlation_with_ci(cordf[,3], cordf[,col])
    this_row = data.frame("lag" = c(lag), "coeff" = c(rv$coeff), "low" = c(rv$low), "high" = c(rv$high), "n" = c(rv$n)) 
    #print(this_row)
    acdf <- rbind(acdf, this_row)  
  }
  
  plot_ac(plot_df=acdf, ylabel=ylabel, the_filename=the_filename, plot_layout=plot_layout)
  return(acdf)
}

get_ac_from_wide <- function(df_wide, ylabel="Autocorrelation", the_filename="plot.pdf", plot_layout=NA, take_first_difference=F) {
  if (class(df_wide)=="matrix") {
    df_long <- reshape2::melt(df_wide)
  } else {
    df_wide$Year <- rownames(df_wide)
    df_long <- reshape2::melt(df_wide, idx.variables="Year")
  }
  colnames(df_long) <- c("Year", "index", "var")
  
  if (take_first_difference) {
    df_long <- df_long %>%
      arrange(index, Year) %>%
      group_by(index) %>%
      mutate_at(vars(-index, -Year), funs(. - dplyr::lag(., 1))) %>%
      ungroup()
  }
  
  rv <- get_ac(df_long, ylabel=ylabel, the_filename=the_filename, plot_layout=plot_layout)
  return(rv)
  #df_VA_s_long <- data.frame(df_VA_s_long)
}


# main entry point

setwd("~/datalake/CIEDB_2009_2013/")
#load(file="data_short_consistent.Rda", verbose=T)

load("09_sectoral_change.Rda", verbose=T) # df_E_g, df_VA_g, df_LP_diff, df_LP_IO_diff, df_FA_g, df_E_s, df_VA_s
load("09_sectoral_shares_from_balanced.Rda", verbose=T)         # df_E_s, df_VA_s
load("10_sectoral_structural_change_plot_data.Rda", verbose=T)  # plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g, plot_MACRO_E_g, plot_MACRO_VA_g
load("09_sectoral_monopolization.Rda", verbose=T) # nHHI_TOAS_wide, Entropy_TOAS_wide, nHHI_TOAS_long, Entropy_TOAS_long
load("08_macro_sectoral_change.Rda", verbose=T)  # MACRO_VA_share_wide, MACRO_E_share_wide, MACRO_VA_share_g_wide, MACRO_E_share_g_wide, MACRO_LP_wide
load("08_macro_sector_dispersion.Rda", verbose=T)  # sector_dispersion



ac_E_s <- get_ac_from_wide(df_E_s, ylabel="Autocorrelation Employment Shares", the_filename="11_ac_E_s.pdf", plot_layout=NA)
ac_VA_s <- get_ac_from_wide(df_VA_s, ylabel="Autocorrelation VA Shares", the_filename="11_ac_VA_s.pdf", plot_layout=c(0.0, 1.0, 0.6, 1.0, 9.0, 14.5))
ac_EVA_s <- get_ac_from_wide(df_E_s*df_VA_s, ylabel="Autocorrelation Employment Shares * VA Shares", the_filename="11_ac_EVA_s.pdf", plot_layout=c(0.0, 1.0, 0.6, 1.0, 9.0, 14.5))

ac_E_bp <- get_ac_from_wide(plot_df_E_g, ylabel="Autocorrelation Employment Balanced Panels", the_filename="11_ac_E_bp.pdf", plot_layout=NA)
ac_VA_bp <- get_ac_from_wide(plot_df_VA_g, ylabel="Autocorrelation VA Balanced Panels", the_filename="11_ac_VA_bp.pdf", plot_layout=c(-0.2, 1.0, 0.55, 1.0, 9.0, 14.5))
ac_EVA_bp <- get_ac_from_wide(plot_df_VA_g*plot_df_E_g, ylabel="Autocorrelation Employment Balanced Panels * VA Balanced Panels", the_filename="11_ac_EVA_bp.pdf", plot_layout=c(-0.4, 1.0, 0.45, 1.0, 9.0, 14.5))

ac_E_g <- get_ac_from_wide(df_E_g, ylabel="Autocorrelation Employment Growth", the_filename="11_ac_E_g.pdf", plot_layout=c(-1.0, 1.0, 0.3, 1.0, 9.0, 14.5))
ac_VA_g <- get_ac_from_wide(df_VA_g, ylabel="Autocorrelation VA Growth", the_filename="11_ac_VA_g.pdf", plot_layout=c(-1.0, 1.0, 0.3, 1.0, 9.0, 14.5))
ac_EVA_g <- get_ac_from_wide(df_VA_g*df_E_g, ylabel="Autocorrelation Employment Growth * VA Growth", the_filename="11_ac_EVA_g.pdf", plot_layout=c(-1.0, 1.0, 0.3, 1.0, 9.0, 14.5))

ac_nHHI <- get_ac_from_wide(nHHI_TOAS_wide, ylabel="Autocorrelation HHI", the_filename="11_ac_nHHI.pdf", plot_layout=c(0.0, 1.0, 0.6, 1.0, 0.5, 6.0))
ac_ENTR <- get_ac_from_wide(Entropy_TOAS_wide, ylabel="Autocorrelation Entropy", the_filename="11_ac_ENTR.pdf", plot_layout=NA)

ac_diff_nHHI <- get_ac_from_wide(nHHI_TOAS_wide, ylabel="Autocorrelation HHI Change", the_filename="11_ac_diff_nHHI.pdf", plot_layout=c(-1.0, 1.0, 0.2, 1.0, 0.5, 6.0), take_first_difference=T)
ac_diff_ENTR <- get_ac_from_wide(Entropy_TOAS_wide, ylabel="Autocorrelation Entropy Change", the_filename="11_ac_diff_ENTR.pdf", plot_layout=c(-1.0, 1.0, 0.2, 1.0, 0.5, 6.0), take_first_difference=T)

mac_E_s <- get_ac_from_wide(MACRO_E_share_wide, ylabel="Autocorrelation Employment Shares", the_filename="11_mac_E_s.pdf", plot_layout=c(0.75, 1.0, 0.75, .86, .5, 6.0))
mac_VA_s <- get_ac_from_wide(MACRO_VA_share_wide, ylabel="Autocorrelation VA Shares", the_filename="11_mac_VA_s.pdf", plot_layout=c(0.75, 1.0, 0.75, .86, .5, 6.0))
mac_EVA_s <- get_ac_from_wide(MACRO_E_share_wide*MACRO_VA_share_wide, ylabel="Autocorrelation Employment Shares * VA Shares", the_filename="11_mac_EVA_s.pdf", plot_layout=c(0.75, 1.0, 0.75, .86, .5, 6.0))

mac_E_g <- get_ac_from_wide(MACRO_E_share_g_wide, ylabel="Autocorrelation Employment Growth", the_filename="11_mac_E_g.pdf", plot_layout=c(-0.2, 1.0, 0.55, 1.0, 9.0, 14.5))
mac_VA_g <- get_ac_from_wide(MACRO_VA_share_g_wide, ylabel="Autocorrelation VA Growth", the_filename="11_mac_VA_g.pdf", plot_layout=c(-0.2, 1.0, 0.55, 1.0, 9.0, 14.5))
mac_EVA_g <- get_ac_from_wide(MACRO_E_share_g_wide*MACRO_VA_share_g_wide, ylabel="Autocorrelation Employment Growth * VA Growth", the_filename="11_mac_EVA_g.pdf", plot_layout=c(-0.4, 1.0, 0.5, 1.0, 9.0, 14.5))

mac_diff_E_s <- get_ac_from_wide(MACRO_E_share_wide, ylabel="Autocorrelation Employment Share Change", the_filename="11_mac_diff_E_s.pdf", plot_layout=c(0.0, 1.0, 0.0, .45, 9., 14.5), take_first_difference=T)
mac_diff_VA_s <- get_ac_from_wide(MACRO_VA_share_wide, ylabel="Autocorrelation VA Share Change", the_filename="11_mac_diff_VA_s.pdf", plot_layout=c(-0.4, 1.0, 0.45, 1.0, .5, 6.0), take_first_difference=T)



save(ac_E_s, ac_VA_s, ac_EVA_s, ac_E_bp, ac_VA_bp, ac_EVA_bp, ac_E_g, ac_VA_g, ac_EVA_g, ac_nHHI, ac_ENTR, ac_diff_nHHI, ac_diff_ENTR, mac_E_g, mac_VA_g, mac_EVA_g, mac_E_s, mac_VA_s, mac_EVA_s, mac_diff_E_s, mac_diff_VA_s, file="11_sectoral_autocorrelations.Rda")
load("11_sectoral_autocorrelations.Rda", verbose=T)    #ac_E_s, ac_VA_s, ac_EVA_s, ac_E_bp, ac_VA_bp, ac_EVA_bp, ac_E_g, ac_VA_g, ac_EVA_g, ac_nHHI, ac_ENTR, ac_diff_nHHI, ac_diff_ENTR, mac_E_g, mac_VA_g, mac_EVA_g, mac_E_s, mac_VA_s, mac_EVA_s, mac_diff_E_s, mac_diff_VA_s


df_md <- sector_dispersion %>%
  select(Year, nHHI_E, nHHI_K, nHHI_VA)
colnames(df_md) <- c("Year", "Employment", "Capital", "Value Added")
plot_macro_dispersion(df_md, ylabel="normalized HHI", the_filename="11_aggregated_HHI.pdf")

df_md <- sector_dispersion %>%
  select(Year, Entropy_E, Entropy_K, Entropy_VA)
colnames(df_md) <- c("Year", "Employment", "Capital", "Value Added")
plot_macro_dispersion(df_md, ylabel="Entropy", the_filename="11_aggregated_Entropy.pdf")











if (F) {
  ac_E_g <- apply(plot_df_E_g, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  ac_VA_g <- apply(plot_df_VA_g, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  ac_EVA_g <- apply(plot_df_E_g*plot_df_VA_g, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  
  mac_E_g <- apply(plot_MACRO_E_g, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  mac_VA_g <- apply(plot_MACRO_VA_g, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  mac_EVA_g <- apply(plot_MACRO_E_g*plot_MACRO_VA_g, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  
  ac_E_s <- apply(df_E_s, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  ac_VA_s <- apply(df_VA_s, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  ac_EVA_s <- apply(df_E_s*df_VA_s, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  
  mac_E_s <- apply(MACRO_E_share_wide, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  mac_VA_s <- apply(MACRO_VA_share_wide, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  mac_EVA_s <- apply(MACRO_E_share_wide*MACRO_VA_share_wide, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  
  ac_nHHI <- apply(nHHI_TOAS_wide, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  ac_Entropy <- apply(Entropy_TOAS_wide, 2, function(x)acf(c(x), na.action=na.pass, plot=F)$acf)
  
  save(ac_E_g, ac_VA_g, ac_EVA_g, ac_E_s, ac_VA_s, ac_EVA_s, file="11_sectoral_autocorrelations.Rda")
}

if (F) {
  pacman::p_load(dplyr,tidyr,purrr,reshape2)
  
  acorrs <- Entropy_TOAS_long %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Sector, Year) %>%
    dplyr::group_by(Sector) %>%
    tidyr::nest(data=c(value)) %>%
    dplyr::mutate(acorr_result = purrr::map(data, ~ acf(.x$value, na.action=na.pass, plot = F)),
                  acorr = purrr::map(acorr_result, ~ drop(.x$acf)),
    ) %>%
    tidyr::unnest(acorr) %>%
    dplyr::select(Sector, acorr) %>%
    dplyr::group_by(Sector) %>%
    dplyr::mutate(lag = seq(0, n() - 1)) 
  
}

