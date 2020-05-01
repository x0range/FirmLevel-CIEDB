## ------------------------------------------------------------------------
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,RColorBrewer,colorspace,knitr,tidyr,devtools)

setwd("~/eecon/git/Amadeus-Datawork")
devtools::load_all("fittinglevy")
setwd("~/datalake/CIEDB_2009_2013/")

#load("China_data_set_incl_compounds.Rda")   # loads df
load("dataframe_including_FirmType2.Rda")   # loads df

country_names = c("PR China")

names(df)

  df_cut <- df %>% 
    select(ID, Year, Sector.Short, Province, Province.Code, FirmType2, Employment, Employment_g, def_LP, def_LP_IO,  def_LP_IO_g, 
           def_LP_IO_lr, def_LP_diff, def_LP_IO_diff, def_TFP_g, def_RoC_G_FI, def_VA, def_VA_IO, def_FIAS_g, def_TFP_IO_diff,
           def_VA_diff, def_VA_g, Employment, Employment_diff, Employment_g) %>%     # exchanged FirmType for FirmType2
    filter(Employment > 0) %>% # Size index
    mutate(COMPCAT = ifelse((Employment >= 0 & Employment < 50), 1, 
                             ifelse((Employment >= 50 & Employment < 250), 2,
                             ifelse((Employment >= 250 & Employment < 1500), 3,4))))  
  
 

## ------------------------------------------------------------------------


      
    ## Size proportion

      Size_p <- df_cut %>% 
        select(ID, Year, COMPCAT, Employment, COMPCAT) %>%
        na.omit() %>%
        group_by(Year) %>%
        summarise(S_p = length(COMPCAT[COMPCAT == 1])/n(),
                  M_p = length(COMPCAT[COMPCAT == 2])/n(),
                  L_p = length(COMPCAT[COMPCAT == 3])/n(),
                  VL_p = length(COMPCAT[COMPCAT == 4])/n(),
                  Self_emp = length(Employment[Employment == 1])/n(),
                  n = n())
      
    ## Industry
      
       Ind_p <- df_cut %>%
        select(ID, Year, Sector.Short) %>%
        na.omit() %>%
        group_by(Year, Sector.Short) %>%
        summarise (n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        group_by(Year)

       n <- Ind_p %>% # to check the # of obs
         group_by(Year) %>%
         summarise(n = sum(n))
     
       Ind_p <-  spread(Ind_p[-c(3)], Sector.Short, freq)
       Ind_p$n <- n[[2]]
       
        
       ## Firm type
       Type_p <- df_cut %>%
        select(ID, Year, FirmType2) %>%
        na.omit() %>%
        group_by(FirmType2) %>%
        filter(length(ID) > 10) %>% # removing the firm types less than 10 obs
        group_by(Year, FirmType2) %>%
        summarise (n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        group_by(Year)

       n <- Type_p %>%
         group_by(Year) %>%
         summarise(n = sum(n))
     
       Type_p <-  spread(Type_p[-c(3)], FirmType2, freq)
       Type_p$n <- n[[2]]
       
       
       ## Province
       Pro_p <- df_cut %>%
        select(ID, Year, Province) %>%
        na.omit() %>%
        group_by(Year, Province) %>%
        summarise (n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        group_by(Year)

       n <- Pro_p %>%
         group_by(Year) %>%
         summarise(n = sum(n))
     
       Pro_p <-  spread(Pro_p[-c(3)], Province, freq)
       Pro_p$n <- n[[2]]
       
## ------------------------------------------------------------------------

### Plot for the proportions. One by one manually...
#setwd("~/Desktop/CIEDB_2009_2013/Figures ")

 pdf(paste("Proportion.pdf", sep = ""), height = 5, width =8)
  par(mfrow=c(2,2), mar=c(3, 2.5, 1, 4), mgp=c(1.,.1,0), tck=-.01, oma=c(0,0,2,1))
  
     color_this <- brewer.pal(5, "Dark2")
      
      plot(Size_p$Year, Size_p$S_p,  yaxt = "n", xaxt = "n",main = "Size Proportion", cex.main = 1., xlab = "Year", ylab = "Proportion", lty="blank", pch = 20, cex = .0, ylim = c(0, 1))
    
  axis(side = 1, at = Size_p$Year, labels = substr(Size_p$Year, 3,4), lwd = 0.3, cex.axis=0.6)
  axis(side = 2, lwd = 0.3, cex.axis=0.6)

for(y in 1:5){
  lines(Size_p$Year, Size_p[[c(y+1)]], cex = 0.5, col = color_this[y], lwd = 1.5)  
} 

leg = c("S", "M", "L", "VL", "Self")
         
 legend(par('usr')[2], par('usr')[4],  xpd=NA, legend = leg, lty = rep(1, c(ncol(Ind_p)-2)), lwd = 2, col = color_this,  bty = "n", cex = .5, ncol = 1)
 
     plot(Ind_p$Year, Ind_p$`06`, col = color_this[1], yaxt = "n", xaxt = "n", main = "Industrty Proportion", cex.main = 1., xlab = "Year", ylab = "Proportion", lty="blank", pch = 20, cex = .0, ylim = c(0, .1))  
     
      axis(side = 1, at = Ind_p$Year, labels = substr(Ind_p$Year, 3,4), lwd = 0.3, cex.axis=0.6)
  axis(side = 2, lwd = 0.3, cex.axis=0.6)

  color_this <- rainbow_hcl(40)
  leg = names(Ind_p[-c(1,ncol(Ind_p))])
  
for(y in 1:c(ncol(Ind_p)-2)){
  lines(Ind_p$Year, Ind_p[[c(y+1)]], cex = 0.5,  lwd = 1., col = color_this[y])  
}

legend(par('usr')[2], par('usr')[4],  xpd=NA, legend = leg, lty = rep(1, c(ncol(Ind_p)-2)), lwd = 2, col = color_this,  bty = "n", cex = .5, ncol = 2)
 
##

  plot(Type_p$Year, Type_p$`110`, col = color_this[1], yaxt = "n", xaxt = "n", main = "Firm-Type Proportion", cex.main = 1., xlab = "Year", ylab = "Proportion", lty="blank", pch = 20, cex = .0, ylim = c(0, .4))  
     
      axis(side = 1, at = Type_p$Year, labels =substr(Type_p$Year, 3,4), lwd = 0.3, cex.axis=0.6)
  axis(side = 2, lwd = 0.3, cex.axis=0.6)
  

  color_this <- rainbow_hcl(ncol(Type_p))
  leg = names(Type_p[-c(1,ncol(Type_p))])
  
for(y in 1:c(ncol(Type_p)-2)){
  lines(Type_p$Year, Type_p[[c(y+1)]], cex = 0.5,  lwd = 1., col = color_this[y])  
}

legend(par('usr')[2], par('usr')[4],  xpd=NA, legend = leg, lty = rep(1, c(ncol(Type_p)-2)), lwd = 2, col = color_this,  bty = "n", cex = .5, ncol = 2)
  
##
  plot(Pro_p$Year, Pro_p$`01`, col = color_this[1], yaxt = "n", xaxt = "n", main = "Province Proportion", cex.main = 1., xlab = "Year", ylab = "Proportion", lty="blank", pch = 20, cex = .0, ylim = c(0, .25))  
     
      axis(side = 1, at = Pro_p$Year, labels =substr(Pro_p$Year, 3,4), lwd = 0.3, cex.axis=0.6)
  axis(side = 2, lwd = 0.3, cex.axis=0.6)
  

  color_this <- rainbow_hcl(ncol(Pro_p))
  leg = names(Pro_p[-c(1,ncol(Pro_p))])
  
for(y in 1:c(ncol(Pro_p)-2)){
  lines(Pro_p$Year, Pro_p[[c(y+1)]], cex = 0.5,  lwd = 1., col = color_this[y])  
}

legend(par('usr')[2], par('usr')[4],  xpd=NA, legend = leg, lty = rep(1, c(ncol(Pro_p)-2)), lwd = 2, col = color_this,  bty = "n", cex = .5, ncol = 2)
  
#mtext("Proportion of Size", side=3, line=1, outer=TRUE,cex= 1.)
  dev.off()
#detach("package:tidyr", unload=TRUE)

## ------------------------------------------------------------------------
## Prod variables


fun_plot_marginal <- function(pdf_name, title, cond_name, var_name, x_lab, c_names, neg_cut, pov_cut, cut_num, plot_name){
  ### LP_change
  
    pdf(paste(pdf_name, ".pdf", sep = ""), height = 4.8, width = 4)
  par(mfrow=c(1,1), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,2,0))

  dd <- df_cut %>%
    select(ID, Year, COMPCAT, Sector.Short, Province, Province.Code, FirmType2, def_LP, def_LP_IO, def_LP_IO_g, def_LP_IO_lr, 
           def_LP_diff, def_LP_IO_diff, def_TFP_IO_diff, def_RoC_G_FI, def_FIAS_g, Employment, Employment_g, Employment_diff, 
           def_VA_diff, def_VA, def_VA_g) 

  var_ind <- match(var_name, colnames(dd))
  cond_ind <- match(cond_name, colnames(dd))
  
  dd <- as.data.frame(dd)
  names(dd)
  print(colnames(dd))
  print(cond_name)
  print(cond_ind)
  dd$Cond <- dd[, cond_ind] # conditional on this variable (categorical variable)
  dd$Var <- dd[, var_ind] # take this variable 
  
  #dd$Var[!is.finite(dd$Var)] <- NA
  
  dd <- dd %>%
    select(ID, Cond, Var) %>%
    na.omit() %>%
    filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tails
    group_by(Cond) %>%
    filter(length(ID) > cut_num) # cut_num: the minimum # of obs for each class

  if (nrow(dd)>cut_num) {
      dd_info <- dd %>% # for the min and maz value of the y and x axis
        group_by(Cond) %>%
        summarise(x_min = min(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$mids),
                  x_max = max(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$mids),
                  y_min = min(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$density[hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$density > 0]),
                  y_max = max(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$density)
                  )
       
      x_min <- min(dd_info$x_min)
      y_min <- min(dd_info$y_min)
      x_max <- max(dd_info$x_max)
      y_max <- max(dd_info$y_max) 
      
      c_uni <- sort(unique(dd$Cond))

      color_ind <- rainbow_hcl(length(c_names))

      plot(c(x_min, x_max), c(y_min, y_max), cex = 0,  log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = title, cex.main = 0.8)
      axis(side = 1, lwd = 0.3, cex.axis=0.9)
      axis(side = 2, lwd = 0.3, cex.axis=.9)
      
      q_25 <- function(n){ ## to use the pch argument in the plot function with more than 25 categories (sectors and provinces)
        apply(data.frame(n), 1, function(x) ifelse(x > 25, x-25,x)  )
      } 
      
       for(c in 1:length(c_uni)){
      print(c_uni[c])
       c_lp <- dd$Var[dd$Cond == c_uni[c]] # get each category
       c_hist <- hist(c_lp, breaks = seq(min(c_lp), max(c_lp),l= 100+1), plot = F)   
       c_ind <- which(c_names%in%c_uni[c]) # ignore this (it is from another script where I keep the legend to be consistent across countries)
      points(c_hist$mids, c_hist$density, pch = q_25(c_ind), cex = 0.4, col = color_ind[c_ind])
      
      levy_fit <- levy_fitting(dat_t = c_lp, bin_num = length(c_hist$mids)+1, include_standarderror=FALSE, include_Soofi=FALSE, fitting_method="QT") # Levy estimation
      levy_q <- dstable(c_hist$mids, levy_fit$levy_para[1], levy_fit$levy_para[2], levy_fit$levy_para[3], levy_fit$levy_para[4])
      lines(c_hist$mids, levy_q, col = color_ind[c_ind], lwd = 1., lty = 1) # Levy fit

       }

    c_ind <-  which(c_names%in%c_uni)


        legend("topright", legend = c_names[c_ind], pch = q_25(c_ind), col = color_ind[c_ind], bty='n', xpd=NA, cex = .8, ncol = 3)

  }

    #mtext(paste(title), side=3, line=1, outer=TRUE,cex= .9)
  dev.off()
  
}


## ------------------------------------------------------------------------
neg_cut <- 0.005
pov_cut <- 0.995
## cross-sectional plots of LP and LP\_change (country-year)

setwd("./Figures/")

############ Additional plots
year_name <- unique(df_cut$Year)

fun_plot_marginal(pdf_name = "Figure_Country_Year_VA_IHateMyLife", title = "Log Density of Value Added by Year", cond_name = "Year", var_name = "def_VA", x_lab = "VA", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)
fun_plot_marginal(pdf_name = "Figure_Country_Year_VA_Diff_IHateMyLife", title = "Log Density of Value Added Change by Year", cond_name = "Year", var_name = "def_VA_diff", x_lab = "VA Change", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)
fun_plot_marginal(pdf_name = "Figure_Country_Year_VA_Growth_IHateMyLife", title = "Log Density of Value Added Growth by Year", cond_name = "Year", var_name = "def_VA_g", x_lab = "VA Growth (%)", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)
fun_plot_marginal(pdf_name = "Figure_Country_Year_EMPL_IHateMyLife", title = "Log Density of Employment by Year", cond_name = "Year", var_name = "Employment", x_lab = "Employment", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)
fun_plot_marginal(pdf_name = "Figure_Country_Year_EMPL_Diff_IHateMyLife", title = "Log Density of Employment Change by Year", cond_name = "Year", var_name = "Employment_diff", x_lab = "Employment Change", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)
fun_plot_marginal(pdf_name = "Figure_Country_Year_EMPL_Growth_IHateMyLife", title = "Log Density of Employment Growth by Year", cond_name = "Year", var_name = "Employment_g", x_lab = "Employment Growth (%)", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

library(finity)

for (i in 1:4) {
  res_char = as.character(i)
  signif_char = ""
  for (var in c("def_VA", "def_VA_diff", "def_VA_g", "Employment", "Employment_diff", "Employment_g", "def_LP", "def_LP_diff")) {
    res <- finity::finite_moment_test(unlist(df_cut[,var]), i, ignore_errors = T)
    res_char <- paste(res_char, as.character(round(res[[1]], 2)), sep=" & ")
    signif_char <- paste(signif_char, as.character(round(res[[2]], 2)), sep=" & ")
  }
  message(paste(res_char, "\\"))
  message(paste(signif_char, "\\"))
}
############ End Additional plots



fun_plot_marginal(pdf_name = "Figure_Country_Year_LP", title = "Log Density of Labor Productivty by Year", cond_name = "Year", var_name = "def_LP_IO", x_lab = "LP", c_names = Size_p$Year, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Year_LP_Growth", title = "Log Density of Labor Productivty Growth by Year", cond_name = "Year", var_name = "def_LP_IO_g", x_lab = "LP Growth(%)", c_names = Size_p$Year,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Year_LP_LogReturn", title = "Log Density of Labor Productivty Log Returns by Year", cond_name = "Year", var_name = "def_LP_IO_lr", x_lab = "LP Log Returns", c_names = Size_p$Year,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Year_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Year", cond_name = "Year", var_name = "def_LP_diff", x_lab = "LP Change", c_names = Size_p$Year,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Year_LP_Diff", title = "Log Density of Labor Productivty Change by Year", cond_name = "Year", var_name = "def_LP_IO_diff", x_lab = "LP Change", c_names = Size_p$Year,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Year_TFP_Change", title = "Log Density of TFP Change by Year", cond_name = "Year", var_name = "def_TFP_IO_diff", x_lab = "TFP (%)", c_names = Size_p$Year,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Year_RoFIAS", title = "Log Density of Profitability (RoFIAS) by Year", cond_name = "Year", var_name = "def_RoC_G_FI", x_lab = "Profitability (RoFIAS)", c_names = Size_p$Year,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Year_IR", title = "Log Density of the Investment Rate by Year", cond_name = "Year", var_name = "def_FIAS_g", x_lab = "Investment Rate", c_names = Size_p$Year,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

## cross-sectional plots of LP and LP\_change (country-size)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP", title = "Log Density of Labor Productivty by Size", cond_name = "COMPCAT", var_name = "def_LP_IO", x_lab = "LP", c_names = c(1:4), neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP_Growth", title = "Log Density of Labor Productivty Growth by Size", cond_name = "COMPCAT", var_name = "def_LP_IO_g", x_lab = "LP Growth(%)", c_names = c(1:4),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP_LogReturn", title = "Log Density of Labor Productivty Log Returns by Size", cond_name = "COMPCAT", var_name = "def_LP_IO_lr", x_lab = "LP Log Returns", c_names = c(1:4),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Size", cond_name = "COMPCAT", var_name = "def_LP_diff", x_lab = "LP Change", c_names = c(1:4),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP_Diff", title = "Log Density of Labor Productivty Change by Size", cond_name = "COMPCAT", var_name = "def_LP_IO_diff", x_lab = "LP Change", c_names = c(1:4),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Size_TFP_Change", title = "Log Density of TFP Change by Size", cond_name = "COMPCAT", var_name = "def_TFP_IO_diff", x_lab = "TFP Change(%)", c_names = c(1:4),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Size_RoFIAS", title = "Log Density of Profitability (RoFIAS) by Size", cond_name = "COMPCAT", var_name = "def_RoC_G_FI", x_lab = "Profitability (RoFIAS)", c_names = c(1:4),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Size_IR", title = "Log Density of the Investment Rate by Size", cond_name = "COMPCAT", var_name = "def_FIAS_g", x_lab = "Investment Rate", c_names = c(1:4),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

## cross-sectional plots of LP and LP\_change (country-industry)
ind_name <- as.numeric(names(Ind_p)[-c(1, ncol(Ind_p))])


fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP", title = "Log Density of Labor Productivty by Sector", cond_name = "Sector.Short", var_name = "def_LP_IO", x_lab = "LP", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP_Growth", title = "Log Density of Labor Productivty Growth by Sector", cond_name = "Sector.Short", var_name = "def_LP_IO_g",x_lab = "LP Growth(%)", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP_LogReturn", title = "Log Density of Labor Productivty Log Returns by Sector", cond_name = "Sector.Short", var_name = "def_LP_IO_lr",x_lab = "LP Log Returns", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Sector", cond_name = "Sector.Short", var_name = "def_LP_diff",x_lab = "LP Change", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP_Diff", title = "Log Density of Labor Productivty Change by Sector", cond_name = "Sector.Short", var_name = "def_LP_IO_diff",x_lab = "LP Change", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_TFP_Change", title = "Log Density of TFP Change by Sector", cond_name = "Sector.Short", var_name = "def_TFP_IO_diff", x_lab = "TFP Change(%)", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_RoFIAS", title = "Log Density of Profitability (RoFIAS) by Sector", cond_name = "Sector.Short", var_name = "def_RoC_G_FI", x_lab = "Profitability (RoFIAS)", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_IR", title = "Log Density of the Investment Rate by Sector", cond_name = "Sector.Short", var_name = "def_FIAS_g", x_lab = "Investment Rate", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

## cross-sectional plots of LP and LP\_change (province-type)
pro_name <- names(Pro_p)[-c(1, ncol(Pro_p))]

fun_plot_marginal(pdf_name = "Figure_Country_Province_LP", title = "Log Density of Labor Productivty by Province", cond_name = "Province", var_name = "def_LP_IO", x_lab = "LP", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Province_LP_Growth", title = "Log Density of Labor Productivty Growth by Province", cond_name = "Province", var_name = "def_LP_IO_g", x_lab = "LP Growth(%)", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Province_LP_LogReturns", title = "Log Density of Labor Productivty Log Returns by Province", cond_name = "Province", var_name = "def_LP_IO_lr", x_lab = "LP Log Returns", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Province_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Province", cond_name = "Province", var_name = "def_LP_diff", x_lab = "LP Change", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Province_LP_Diff", title = "Log Density of Labor Productivty Change by Province", cond_name = "Province", var_name = "def_LP_IO_diff", x_lab = "LP Change", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Province_TFP_Change", title = "Log Density of TFP Change by Province", cond_name = "Province", var_name = "def_TFP_IO_diff", x_lab = "TFP Change(%)", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Province_RoFIAS", title = "Log Density of Profitability (RoFIAS) by Province", cond_name = "Province", var_name = "def_RoC_G_FI", x_lab = "Profitability (RoFIAS)", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Province_IR", title = "Log Density of the Investment Rate by Province", cond_name = "Province", var_name = "def_FIAS_g", x_lab = "Investment Rate", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)




## cross-sectional plots of LP and LP\_change (province-type)
type_name <- names(Type_p)[-c(1, ncol(Type_p))]


fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_LP", title = "Log Density of Labor Productivty by Firm Type", cond_name = "FirmType2", var_name = "def_LP_IO", x_lab = "LP", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_LP_Growth", title = "Log Density of Labor Productivty Growth by Firm Type", cond_name = "FirmType2", var_name = "def_LP_IO_g", x_lab = "LP Growth(%)", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_LP_LogReturn", title = "Log Density of Labor Productivty Log Returns by Firm Type", cond_name = "FirmType2", var_name = "def_LP_IO_lr", x_lab = "LP Log Returns", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Firm Type", cond_name = "FirmType2", var_name = "def_LP_diff", x_lab = "LP Change", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_LP_Diff", title = "Log Density of Labor Productivty Change by Firm Type", cond_name = "FirmType2", var_name = "def_LP_IO_diff", x_lab = "LP Change", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_TFP_Change", title = "Log Density of TFP Change by Firm Type", cond_name = "FirmType2", var_name = "def_TFP_IO_diff", x_lab = "TFP Change(%)", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_RoFIAS", title = "Log Density of Profitability (RoFIAS) by Firm Type", cond_name = "FirmType2", var_name = "def_RoC_G_FI", x_lab = "Profitability (RoFIAS)", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_IR", title = "Log Density of the Investment Rate by Firm Type", cond_name = "FirmType2", var_name = "def_FIAS_g", x_lab = "Investment Rate", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)



## ------------------------------------------------------------------------
#purl("Productivity_Analysis_Data.Rmd")  

