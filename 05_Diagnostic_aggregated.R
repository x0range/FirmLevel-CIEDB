## ------------------------------------------------------------------------
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,RColorBrewer,colorspace,knitr,tidyr)

load("China_data_set_incl_compounds.Rda")   # loads df

country_names = c("PR China")

names(df)

  df_cut <- df %>% 
    select(ID, Year, Sector.Short, Province.Code, FirmType2, Employment,Employment_g, def_LP, def_LP_g, def_Zeta, RoC_G_FI, def_VA) %>%     # exchanged FirmType for FirmType2
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
        select(ID, Year, FirmType) %>%
        na.omit() %>%
        group_by(FirmType) %>%
        filter(length(ID) > 10) %>% # removing the firm types less than 10 obs
        group_by(Year, FirmType) %>%
        summarise (n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        group_by(Year)

       n <- Type_p %>%
         group_by(Year) %>%
         summarise(n = sum(n))
     
       Type_p <-  spread(Type_p[-c(3)], FirmType, freq)
       Type_p$n <- n[[2]]
       
       
       ## Province
       Pro_p <- df_cut %>%
        select(ID, Year, Province.Code) %>%
        na.omit() %>%
        group_by(Year, Province.Code) %>%
        summarise (n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        group_by(Year)

       n <- Pro_p %>%
         group_by(Year) %>%
         summarise(n = sum(n))
     
       Pro_p <-  spread(Pro_p[-c(3)], Province.Code, freq)
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

  dd <- df_cut #%>%
  #  select(ID, Year, COMPCAT, Sector.Short, Province.Code, FirmType, def_LP, def_LP_g, def_Zeta) 

  var_ind <- match(var_name, colnames(dd2))
  cond_ind <- match(cond_name, colnames(dd2))
  
  dd <- as.data.frame(dd)
  names(dd)
  dd$Cond <- dd[, cond_ind] # conditional on this variable (categorical variable)
  dd$Var <- dd[, var_ind] # take this variable 

  dd <- dd %>%
    select(ID, Cond, Var) %>%
    na.omit() %>%
    filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tails
    group_by(Cond) %>%
    filter(length(ID) > cut_num) # cut_num: the minimum # of obs for each class
  
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

   }

c_ind <-  which(c_names%in%c_uni)


    legend("topright", legend = c_names[c_ind], pch = q_25(c_ind), col = color_ind[c_ind], bty='n', xpd=NA, cex = .8, ncol = 3)


    #mtext(paste(title), side=3, line=1, outer=TRUE,cex= .9)
  dev.off()
  
}


## ------------------------------------------------------------------------
neg_cut <- 0.005
pov_cut <- 0.995
## cross-sectional plots of LP and LP\_change (country-year)



fun_plot_marginal(pdf_name = "Figure_Country_Year_LP", title = "Log Density of Labor Productivty by Year", cond_name = "Year", var_name = "def_LP", x_lab = "LP", c_names = Size_p$Year, neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Year_LP_Growth", title = "Log Density of Labor Productivty Growth by Year", cond_name = "Year", var_name = "def_LP_g", x_lab = "LP Growth(%)", c_names = Size_p$Year,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Year_TFP_Growth", title = "Log Density of TFP Growth by Year", cond_name = "Year", var_name = "def_Zeta", x_lab = "TFP (%)", c_names = Size_p$Year,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

## cross-sectional plots of LP and LP\_change (country-size)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP", title = "Log Density of Labor Productivty by Size", cond_name = "COMPCAT", var_name = "def_LP", x_lab = "LP", c_names = c(1:4), neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Size_LP_Growth", title = "Log Density of Labor Productivty Growth by Size", cond_name = "COMPCAT", var_name = "def_LP_g", x_lab = "LP Growth(%)", c_names = c(1:4),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Size_TFP_Growth", title = "Log Density of TFP Growth by Size", cond_name = "COMPCAT", var_name = "def_Zeta", x_lab = "TFP Growth(%)", c_names = c(1:4),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)


## cross-sectional plots of LP and LP\_change (country-industry)
ind_name <- as.numeric(names(Ind_p)[-c(1, ncol(Ind_p))])


fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP", title = "Log Density of Labor Productivty by Sector", cond_name = "Sector.Short", var_name = "def_LP", x_lab = "LP", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_LP_Growth", title = "Log Density of Labor Productivty Growth by Sector", cond_name = "Sector.Short", var_name = "def_LP_g",x_lab = "LP Growth(%)", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Industry_TFP_Growth", title = "Log Density of TFP Growth by Sector", cond_name = "Sector.Short", var_name = "def_Zeta", x_lab = "TFP Growth(%)", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

## cross-sectional plots of LP and LP\_change (province-type)
pro_name <- as.numeric(names(Pro_p)[-c(1, ncol(Pro_p))])


fun_plot_marginal(pdf_name = "Figure_Country_Province_LP", title = "Log Density of Labor Productivty by Province", cond_name = "Province", var_name = "def_LP", x_lab = "LP", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Province_LP_Growth", title = "Log Density of Labor Productivty Growth by Province", cond_name = "Province", var_name = "def_LP_g", x_lab = "LP Growth(%)", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Province_TFP_Growth", title = "Log Density of TFP Growth by Province", cond_name = "Province", var_name = "def_Zeta", x_lab = "TFP Growth(%)", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)



## cross-sectional plots of LP and LP\_change (province-type)
type_name <- as.numeric(names(Type_p)[-c(1, ncol(Type_p))])


fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_LP", title = "Log Density of Labor Productivty by Firm Type", cond_name = "FirmType2", var_name = "def_LP", x_lab = "LP", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_LP_Growth", title = "Log Density of Labor Productivty Growth by Firm Type", cond_name = "FirmType2", var_name = "def_LP_g", x_lab = "LP Growth(%)", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)

fun_plot_marginal(pdf_name = "Figure_Country_Firm_Type_TFP_Growth", title = "Log Density of TFP Growth by Firm Type", cond_name = "FirmType2", var_name = "def_Zeta", x_lab = "TFP Growth(%)", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 5000)


## ------------------------------------------------------------------------
purl("Productivity_Analysis_Data.Rmd")  

