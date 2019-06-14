# 0.1 loading of required modules
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,RColorBrewer,colorspace,tidyr,numbers)


# 0.2 definition of functions 

## helper function to use the pch argument in the plot function with more than 25 categories (sectors and provinces)
# Arguments: n: Type vector of int, column index
# Returns: vector of int. Elementwise: n if n>25, otherwise n-25
q_25 <- function(n){
  apply(data.frame(n), 1, function(x) ifelse(x > 25, x-25,x)  )
} 

## plotter function. Will plot and save as pdf: a density of variable by condition (another variable) in panels by year
# Arguments:
#   pdf_name:   Type string:    Output file name.
#   title:      Type string:    Plot title
#   cond_name:  Type string:    Name of condition column (will be used to obtain index of condition column)
#   var_name:   Type string:    Name of variable column (will be used to obtain index of variable column)
#   x_lab:      Type string:    variable label (used for x axis label in plot)   
#   c_names:    Type vector:    category names in condition column to be used in legend
#   neg_cut:    Type double:    negative cutoff point (what quantile at the lower end will be removed as outlier)
#   pov_cut:    Type double:    positive cutoff point (one minus what quantile at the lower end will be removed as outlier)
#   cut_num:    Type int:       minimum number of observations by category (otherwise the category is not plotted)
#   plot_name:  Type ?:         (not used)
fun_plot_marginal_years <- function(pdf_name, title, cond_name, var_name, x_lab, c_names, neg_cut, pov_cut, cut_num, plot_name){
  #browser()
  # declare output file name
  pdf(paste(pdf_name, "_by_years.pdf", sep = ""), height = 4.8, width = 4)
  
  # reduce dataframe to required variables  
  # TODO: accept data frame df_cut as variable instead of operating with global variables
  dd2 <- df_cut %>%
    select(ID, Year, COMPCAT, Sector.Short, Province, FirmType2, def_LP_IO, def_LP_IO_g, def_LP_IO_lr, def_LP_diff, def_LP_IO_diff, def_TFP_IO_diff, def_TFP_g, def_RoC_G_FI, def_FIAS_g) 
  dd2 <- as.data.frame(dd2)
  
  # obtain index of condition and variable columns
  var_ind <- match(var_name, colnames(dd2))
  cond_ind <- match(cond_name, colnames(dd2))
  # select relevant variables  
  dd2$Cond <- dd2[, cond_ind] # conditional on this variable (categorical variable)
  dd2$Var <- dd2[, var_ind] # take this variable 
 
  # compute margins of plottable area (x_min, x_max, y_min, y_max) for whole dataframe (not by year)
  dd3 <- dd2 %>%
    select(ID, Cond, Var) %>%
    na.omit() %>%
    filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tails
    group_by(Cond) %>%
    filter(length(ID) > cut_num) # cut_num: the minimum # of obs for each class
  dd3_info <- dd3 %>% # for the min and maz value of the y and x axis
    group_by(Cond) %>%
    summarise(x_min = min(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$mids),
              x_max = max(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$mids),
              y_min = min(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$density[hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$density > 0]),
              y_max = max(hist(Var, breaks = seq(min(Var), max(Var),l= 100+1), plot = F)$density)
              )
  x_min <- min(dd3_info$x_min)
  y_min <- min(dd3_info$y_min)
  x_max <- max(dd3_info$x_max)
  y_max <- max(dd3_info$y_max) * 1.5
  
  
  # define layout of multiplot
  par(mfrow = c(4, 4))
  par(cex = 0.6)
  par(mar = c(0, 0, 0, 0), oma = c(4, 4, 3.5, 0.5))
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))

  
  # loop through years
  i = 0 # counter 
  for (year in 1998:2013) {
      i = i + 1 # increment counter
        
      # select entries for this period
      dd <- dd2[dd2["Year"]==year,]
      
      # prepare data frame for density plotting
      dd <- dd %>%
        select(ID, Cond, Var) %>%
        na.omit() %>%
        filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tails
        group_by(Cond) %>%
        filter(length(ID) > cut_num) # cut_num: the minimum # of obs for each class
      
      # obtain entries for legend
      c_uni <- sort(unique(dd$Cond))
      
      # define colors
      color_ind <- rainbow_hcl(length(c_names))
      
      # prepare canvas
      plot(c(x_min, x_max), c(y_min, y_max), cex = 0,  log = "y", yaxt = "n", xaxt = "n", cex.main = 0.5, xlab = x_lab,  cex.xlab=0.5, ylab = "Log-Density", cex.ylab=0.5, main = as.character(year), line=-1, cex.main = 0.5)
      
      # plot axes labels only if this canvas is at the margin
      if (i %in% c(13, 14, 15, 16)) {
        axis(side = 1, lwd = 0.2, cex.axis=.5)
      }
      if (i %in% c(1, 5, 9, 13)) {
        axis(side = 2, lwd = 0.2, cex.axis=.5)
      }
      
      # plot density only if we have data  
      if (length(dd$Cond)>0) {
        print(paste(as.character(year), "plotted"))
        for(c in 1:length(c_uni)){
            # compute densities
            c_cat <- dd$Var[dd$Cond == c_uni[c]]    # get each category
            c_hist <- hist(c_cat, breaks = seq(min(c_cat), max(c_cat),l= 100+1), plot = F)   
            c_ind <- which(c_names%in%c_uni[c])     # ignore this (it is from another script where I keep the legend to be consistent across countries)
            # create scatter plot
            points(c_hist$mids, c_hist$density, pch = q_25(c_ind), cex = 0.3, col = color_ind[c_ind])
        }
        
        # include legend
        c_ind <-  which(c_names%in%c_uni)
        # actually plot legend only for one year, otherwise the plot gets too convoluted
        if (year==2007) {
          legend("topright", legend = c_names[c_ind], pch = q_25(c_ind), col = color_ind[c_ind], bty='n', xpd=NA, cex = .4, ncol = div(length(c_ind),14)+1)
        }
      } else {
        print(paste(as.character(year), "not plotted"))
      }
      
      # set plot title  
      mtext(paste(title), side=3, line=1, outer=TRUE, cex= .9)
    }
  
  # close pdf  
  dev.off()
  
}


### main entry point 

# 1. set working directory to where the data is
#setwd("~/dat/CIEDB_2009_2013/")

# 2. load and prepare data

#load("China_data_set_incl_compounds.Rda")   # loads df
load("dataframe_including_FirmType2.Rda")   # loads df

# define country name
country_names = c("PR China")

#names(df)

# create generic firm size column
df_cut <- df %>% 
    select(ID, Year, Sector.Short, Province, FirmType2, Employment, Employment_g, def_LP_IO, def_LP_IO_g, def_LP_IO_lr, def_LP_diff, def_LP_IO_diff, def_TFP_IO_diff, def_TFP_g, def_RoC_G_FI, def_VA, def_VA_IO, def_RoC_G_FI, def_FIAS_g) %>%
    filter(Employment > 0) %>% # Size index
    mutate(COMPCAT = ifelse((Employment >= 0 & Employment < 50), "S", 
                             ifelse((Employment >= 50 & Employment < 250), "M",
                             ifelse((Employment >= 250 & Employment < 1500), "L", "VL"))))  

# 3. Change working directory to where the plots should be stored
setwd("./Figures")


# 4. define environment variables for plotting: cutoffs

neg_cut <- 0.005
pov_cut <- 0.995


# 5. create plots

##TESTS
#type_name <- unique(df$FirmType2)
#fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_LP_Diff", title = "Log Density of Labor Productivty Change by Firm Type", cond_name = "FirmType2", var_name = "def_LP_IO_diff", x_lab = "LP Change", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)
#fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Firm Type", cond_name = "FirmType2", var_name = "def_LP_diff", x_lab = "LP Change", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)
#
##TESTS END

## 5.1 cross-sectional plots of LP and LP\_change (country-size)

# TODO: remove hard coded column indices from following function calls; work with column names instead!

fun_plot_marginal_years(pdf_name = "Figure_Country_Size_LP", title = "Log Density of Labor Productivty by Size", cond_name = "COMPCAT", var_name = "def_LP_IO", x_lab = "LP", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Size_LP_Growth", title = "Log Density of Labor Productivty Growth by Size", cond_name = "COMPCAT", var_name = "def_LP_IO_g", x_lab = "LP Growth(%)", c_names = c("S", "M", "L", "VL"),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Size_LP_LogReturn", title = "Log Density of Labor Productivty Log Return by Size", cond_name = "COMPCAT", var_name = "def_LP_IO_lr", x_lab = "LP Log Returns", c_names = c("S", "M", "L", "VL"),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Size_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Size", cond_name = "COMPCAT", var_name = "def_LP_diff", x_lab = "LP Change", c_names = c("S", "M", "L", "VL"),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Size_LP_Diff", title = "Log Density of Labor Productivty Change by Size", cond_name = "COMPCAT", var_name = "def_LP_IO_diff", x_lab = "LP Change", c_names = c("S", "M", "L", "VL"),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Size_TFP_Change", title = "Log Density of TFP Change by Size", cond_name = "COMPCAT", var_name = "def_TFP_IO_diff", x_lab = "TFP Change(%)", c_names = c("S", "M", "L", "VL"),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Size_RoFIAS", title = "Log Density of Profitability (RoFIAS) by Size", cond_name = "COMPCAT", var_name = "def_RoC_G_FI", x_lab = "Profitability (RoFIAS)", c_names = c("S", "M", "L", "VL"),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Size_IR", title = "Log Density of the Investment Rate by Size", cond_name = "COMPCAT", var_name = "def_FIAS_g", x_lab = "Investment Rate", c_names = c("S", "M", "L", "VL"),  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)


## 5.2 cross-sectional plots of LP and LP\_change (country-industry)
ind_name <- unique(df$Sector.Short)

fun_plot_marginal_years(pdf_name = "Figure_Country_Industry_LP", title = "Log Density of Labor Productivty by Sector", cond_name = "Sector.Short", var_name = "def_LP_IO", x_lab = "LP", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Industry_LP_Growth", title = "Log Density of Labor Productivty Growth by Sector", cond_name = "Sector.Short", var_name = "def_LP_IO_g", x_lab = "LP Growth(%)", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Industry_LP_LogReturn", title = "Log Density of Labor Productivty Log Returns by Sector", cond_name = "Sector.Short", var_name = "def_LP_IO_lr", x_lab = "LP Log Returns", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Industry_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Sector", cond_name = "Sector.Short", var_name = "def_LP_diff", x_lab = "LP Change", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Industry_LP_Diff", title = "Log Density of Labor Productivty Change by Sector", cond_name = "Sector.Short", var_name = "def_LP_IO_diff", x_lab = "LP Change", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Industry_TFP_Change", title = "Log Density of TFP Change by Sector", cond_name = "Sector.Short", var_name = "def_TFP_IO_diff", x_lab = "TFP Change(%)", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Industry_RoFIAS", title = "Log Density of Profitability (RoFIAS) by Sector", cond_name = "Sector.Short", var_name = "def_RoC_G_FI", x_lab = "Profitability (RoFIAS)", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Industry_IR", title = "Log Density of the Investment Rate by Sector", cond_name = "Sector.Short", var_name = "def_FIAS_g", x_lab = "Investment Rate", c_names = ind_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

## 5.3 cross-sectional plots of LP and LP\_change (province-type)
pro_name <- unique(df$Province)

fun_plot_marginal_years(pdf_name = "Figure_Country_Province_LP", title = "Log Density of Labor Productivty by Province", cond_name = "Province", var_name = "def_LP_IO", x_lab = "LP", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Province_LP_Growth", title = "Log Density of Labor Productivty Growth by Province", cond_name = "Province", var_name = "def_LP_IO_g", x_lab = "LP Growth(%)", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Province_LP_LogReturn", title = "Log Density of Labor Productivty Log Returns by Province", cond_name = "Province", var_name = "def_LP_IO_lr", x_lab = "LP Log Returns", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Province_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Province", cond_name = "Province", var_name = "def_LP_diff", x_lab = "LP Change", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Province_LP_Diff", title = "Log Density of Labor Productivty Change by Province", cond_name = "Province", var_name = "def_LP_IO_diff", x_lab = "LP Change", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Province_TFP_Change", title = "Log Density of TFP Change by Province", cond_name = "Province", var_name = "def_TFP_IO_diff", x_lab = "TFP Change(%)", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Province_RoFIAS", title = "Log Density of Profitability (RoFIAS) by Province", cond_name = "Province", var_name = "def_RoC_G_FI", x_lab = "Profitability (RoFIAS)", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Province_IR", title = "Log Density of the Investment Rate by Province", cond_name = "Province", var_name = "def_FIAS_g", x_lab = "Investment Rate", c_names = pro_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)


## 5.4 cross-sectional plots of LP and LP\_change (province-type)
type_name <- unique(df$FirmType2)

fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_LP", title = "Log Density of Labor Productivty by Firm Type", cond_name = "FirmType2", var_name = "def_LP_IO", x_lab = "LP", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_LP_Growth", title = "Log Density of Labor Productivty Growth by Firm Type", cond_name = "FirmType2", var_name = "def_LP_IO_g", x_lab = "LP Growth(%)", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_LP_LogReturn", title = "Log Density of Labor Productivty Log Returns by Firm Type", cond_name = "FirmType2", var_name = "def_LP_IO_lr", x_lab = "LP Log Returns", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_LP_imp_Diff", title = "Log Density of Labor Productivty (imp) Change by Firm Type", cond_name = "FirmType2", var_name = "def_LP_diff", x_lab = "LP Change", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_LP_Diff", title = "Log Density of Labor Productivty Change by Firm Type", cond_name = "FirmType2", var_name = "def_LP_IO_diff", x_lab = "LP Change", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_TFP_Change", title = "Log Density of TFP Change by Firm Type", cond_name = "FirmType2", var_name = "def_TFP_IO_diff", x_lab = "TFP Change(%)", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_RoFIAS", title = "Log Density of Profitability (RoFIAS) by Firm Type", cond_name = "FirmType2", var_name = "def_RoC_G_FI", x_lab = "Profitability (RoFIAS)", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)

fun_plot_marginal_years(pdf_name = "Figure_Country_Firm_Type_IR", title = "Log Density of the Investment Rate by Firm Type", cond_name = "FirmType2", var_name = "def_FIAS_g", x_lab = "Investment Rate", c_names = type_name,  neg_cut = neg_cut, pov_cut = pov_cut, cut_num = 1000)



