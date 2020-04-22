# Script to perform Levy-alpha stable fits and AEP/Subbotin fits in comparison

############ 0. Basic Set up ############
## 0.1. loading of required libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,StableEstim,lmomco,devtools)

## 0.2 loading local packages
# functions from packages
setwd("~/eecon/git/Amadeus-Datawork/")
devtools::load_all("fittinglevy")
devtools::load_all("fittingAEP")
setwd("~/datalake/CIEDB_2009_2013/")

## 0.3. Setting of basic functions

# the fuction for the fitting result
# Arguments:
#   dat:         Type dataframe: Data
#   cond_name:   Type string:    Name of condition column (will be used to obtain index of condition column)
#   var_name:    Type string:    Name of variable column (will be used to obtain index of variable column)
#   c_names:     Type vector:    category names in condition column to be used in legend
#   neg_cut:     Type double:    negative cutoff point (what quantile at the lower end will be removed as outlier)
#   pov_cut:     Type double:    positive cutoff point (one minus what quantile at the lower end will be removed as outlier)
#   cut_num:     Type int:       the minimum size of each compound category to be fitted; smaller categories are ignored
#   compute_AIC: Type bool:      indicator if Akaike IC should be computed as goodness measure. Takes long.
fun_fit_levy_AEP <- function(dat, bin_num, cond_name, var_name, c_names, neg_cut, pov_cut, cut_num, compute_AIC=TRUE) { 
  #result_list <- list()
  #c_uni_list <- list()
  #c_uni_list_2 <- list()
  #c_uni_num_list <- list()
  all_list <- list()
  res_list <- list()
  
  # define S3 class of dat as data.frame. Up to this point, it is c("grouped_df", "tbl_df", "tbl", "data.frame") because this is what dplyr commands produce.
  dat <- data.frame(dat)
  
  # obtain index of condition and variable columns
  var_ind <- match(var_name, colnames(dat))
  cond_ind <- match(cond_name, colnames(dat))
  # select relevant variables  
  dat$Cond <- dat[, cond_ind] # conditional on this variable (categorical variable)
  dat$Var <- dat[, var_ind] # take this variable 
    
  # loop through years
  #print("Entering year loop")
  i = 0 # counter 
  for (year in 1998:2013) {
    #print(paste("  Year", as.character(year)))
    i = i + 1 # increment counter
    
    zz <- dat[dat$Year==year,] %>%
      select(ID, Year, Var, Cond) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail
      group_by(Cond) %>%
      filter(length(ID) > cut_num) # set the minimum number of obs for each class



    if (nrow(zz) == 0) {
      res_list <- NA
    } else {
        
      # get unique classes
      c_uni <- unique(zz$Cond) # unique classes
      c_uni_name <- c()
      c_uni_num <- c() 
      c_uni_name_2 <- c()
      
      # get id numbers for unique classes
      for (j in 1:length(c_uni)) {
        c_uni_num[j] <- which(c_names %in% c_uni[j])
      }
      
      # get sorted class id names and numbers
      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]
      
      #if(cond_ind == 4){
      #  c_uni_name_2 <- ind_name_table$ind_names_alphabet[c_uni_num]
      #} else{
      #  c_uni_name_2 <- c_uni_name
      #}
      
      #loop through classes and fit these
      res_list <- list()
      #print("  Entering class loop")
      for (c in 1:length(c_uni_name)) {
        #print(paste("    Class", as.character(c_uni_name[[c]])))
        #print(paste("      ", c, "out of", length(c_uni)))
        p_data <- zz$Var[zz$Cond == c_uni_name[c]] # this is not a dataframe but a simple array
        #print(length(p_data))                              # is it?
        # fit Levy alpha-stable and extract return values
        fit_levy <- levy_fitting(dat_t = p_data, bin_num = bin_num, include_bootstrap=FALSE) 
        levy_parameters <- fit_levy$levy_para
        levy_soofi_ID <- fit_levy$levy_soofi
        
        # fit AEP and extract return values
        fit_AEP <- Sub_fun_LM(p_data)
        AEP_parameters <- fit_AEP$para
        
        if (!is.null(AEP_parameters)) {
          # obtain AEP soofi ID score (requires obs_mid, est_sub_lm, obs_p)
          # get binned data
          # TODO: binning code is duplicated from fittinglevy/levy_fitting.R. To be cleaned up
          p_data_h <- hist(p_data, plot = F, breaks = seq(min(p_data), max(p_data), l = bin_num)) # binning the data
          obs_mid <- p_data_h$mids # location of the bin
          obs_p <- p_data_h$counts / sum(p_data_h$counts) # normalized counts of the bin: the normalized empirical density
          # normalized predicted density from the sub model 
          pred_p_sub_b <- pdfaep4(obs_mid, fit_AEP)
          pred_p_sub <- pred_p_sub_b/sum(pred_p_sub_b)
          # soofi score
          AEP_soofi_ID <- (1 - soofi_gen(obs_p, pred_p_sub)) * 100.
        }
          
        # cross validation
        #CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "Levy") # Cross validation function
        #CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "AEP") Cross validation function
        #c_list[[c]] <- list(cv_levy, cv_AEP) # Cross validation function

        ## rounding 
        if (!is.null(AEP_parameters)) {
          AEP_parameters_print <- list(round(AEP_parameters[[1]], 4),  round(AEP_parameters[[2]], 4),  round(AEP_parameters[[3]], 4),  round(AEP_parameters[[4]], 4))
          AEP_soofi_ID_print <- round(AEP_soofi_ID, 4)
        } else {
          AEP_parameters_print <- list("","","","")
          AEP_soofi_ID_print <- ""
          AEP_soofi_ID <- NA
        }

        # AIC
        if (compute_AIC) {
            #print("Computing AIC now")
            levy_aicv <- levy_AIC(para_levy=levy_parameters, observations=obs_mid)
            #print("Levy AIC computed")
            if ((!is.null(AEP_parameters))&&(is.aep4(fit_AEP))&&(fit_AEP$ifailtext!="TAU4 is estimated as below limits of AEP4, Kappa fit instead")) { # second, third condition necessary, since AIC computation fails otherwise
                #print(fit_AEP)
              AEP_aicv <- AEP_AIC(fit_AEP=fit_AEP, obs_mid=obs_mid)
              #print("AEP AIC computed")
            } else {
              AEP_aicv <- ""
            }
            if (is.numeric(AEP_aicv)) {
                rounded_AEP_aicv = round(AEP_aicv, 4)
            } else {
                rounded_AEP_aicv = AEP_aicv
            }
            res_list[[c]] <- list(year_name=year, class_name=c_uni_name[[c]], class_num=c, year_num=i, number_observations=length(p_data), levy_parameters=levy_parameters, levy_soofi_ID=levy_soofi_ID, levy_aic=levy_aicv, AEP_parameters=AEP_parameters, AEP_soofi_ID=AEP_soofi_ID, AEP_aic=AEP_aicv)
            print(paste(var_name, "&", cond_name, "&", year, "&", c_uni_name[[c]], "&",  c, "&",  i, "&",  length(p_data), "&", round(levy_parameters[[1]], 4), "&",  round(levy_parameters[[2]], 4), "&",  round(levy_parameters[[3]], 4), "&",  round(levy_parameters[[4]], 4), "&",  round(levy_soofi_ID, 2), "&",  round(levy_aicv, 4), "&",  AEP_parameters_print[[1]], "&",  AEP_parameters_print[[2]], "&",  AEP_parameters_print[[3]], "&",  AEP_parameters_print[[4]], "&",  AEP_soofi_ID_print, "&",  rounded_AEP_aicv, "\\"))
        } else {
            res_list[[c]] <- list(year_name=year, class_name=c_uni_name[[c]], class_num=c, year_num=i, number_observations=length(p_data), levy_parameters=levy_parameters, levy_soofi_ID=levy_soofi_ID, AEP_parameters=AEP_parameters, AEP_soofi_ID=AEP_soofi_ID)
            print(paste(var_name, "&", cond_name, "&", year, "&", c_uni_name[[c]], "&",  length(p_data), "&", round(levy_parameters[[1]], 4), "&",  round(levy_parameters[[2]], 4), "&",  round(levy_parameters[[3]], 4), "&",  round(levy_parameters[[4]], 4), "&",  round(levy_soofi_ID, 2), "&", "&", AEP_parameters_print[[1]], "&",  AEP_parameters_print[[2]], "&",  AEP_parameters_print[[3]], "&",  AEP_parameters_print[[4]], "&",  AEP_soofi_ID_print, "&", "\\"))
        }            
      }
      #c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      #c_uni_list_2[[k]] <- c_uni_name_2 #
      #c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      #result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
    all_list[[i]] <- res_list
    #browser()
  }
  return(all_list)
}

# main entry point


### main entry point 

# 1. set working directory to where the data is
#setwd("~/dat/CIEDB_2009_2013/")

# 2. load and prepare data

#load("China_data_set_incl_compounds.Rda")   # loads df
load("dataframe_including_FirmType2.Rda")   # loads df
print("Loaded data set. Commencing cleaning...")

# define country name
country_names = c("PR China")

#names(df)

# create generic firm size column
df_cut <- df %>% 
    select(ID, Year, Sector.Short, Province, FirmType2, Employment, Employment_g, def_LP, def_LP_IO, def_LP_IO_g, def_LP_IO_lr, def_LP_diff, def_LP_IO_diff, def_TFP_IO_diff, def_RoC_G_FI, def_VA, def_VA_IO, def_FIAS_g) %>%
    filter(Employment > 0) %>% # Size index
    mutate(COMPCAT = ifelse((Employment >= 0 & Employment < 50), "S", 
                             ifelse((Employment >= 50 & Employment < 250), "M",
                             ifelse((Employment >= 250 & Employment < 1500), "L", "VL"))))  

# Add ISIC R4 sectors

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

save(GB2002_ISICR4, file="05_GB2002_ISICR4_table.Rda")
load("05_GB2002_ISICR4_table.Rda", verbose=T)  # GB2002_ISICR4
colnames(GB2002_ISICR4) <- c("Sector.ISICR4", "Sector.Short")

df_cut <- df_cut %>% 
  ungroup() %>%
  left_join(GB2002_ISICR4, by="Sector.Short", na_matches="never") %>%
  ungroup()

print("Prepared data. Commencing fit...")

# 3. Change working directory to where the plots should be stored
#setwd("~/dat/CIEDB_2009_2013/Figures")
setwd("./Figures")


# 4. define environment variables for plotting: cutoffs

neg_cut <- 0.005
pov_cut <- 0.995


# 5. create plots

#TESTS
#year_name <- sort(unique(df$Year))
#fit_results_year_IRT <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_FIAS_g", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
#fit_results_year_LPG <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_LP_diff", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
#fit_results_year_ROC <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_RoC_G_FI", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)

#TEST END

## 5.0 year only plots
year_name <- sort(unique(df$Year))
load(file="China_fit_results_year.Rda")

fit_results_year_LPR <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_LP_IO", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPI <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_LP_IO_diff", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_ROC <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_RoC_G_FI", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_IRT <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_FIAS_g", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
#save(fit_results_year_LPR, fit_results_year_LPI, fit_results_year_ROC, fit_results_year_IRT, fit_results_year_VAD, file="China_fit_results_year.Rda")
fit_results_year_LPG <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_LP_IO_g", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
#save(fit_results_year_LPR, fit_results_year_LPG, fit_results_year_LPI, fit_results_year_ROC, fit_results_year_IRT, fit_results_year_VAD, file="China_fit_results_year.Rda")
fit_results_year_LIM <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_LP", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPL <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_LP_IO_lr", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_LP_diff", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_TFP <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_TFP_IO_diff", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_VAD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Year", var_name = "def_VA", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)

save(fit_results_year_LIM, fit_results_year_LPR, fit_results_year_LPG, fit_results_year_LPL, fit_results_year_LPD, fit_results_year_LPI, fit_results_year_TFP, fit_results_year_ROC, fit_results_year_IRT, fit_results_year_VAD, file="China_fit_results_year.Rda")

## 5.1 cross-sectional (size)

# TODO: remove hard coded column indices from following function calls; work with column names instead!
load(file="China_fit_results_size.Rda")

fit_results_year_LIM <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_LP", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPR <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_LP_IO", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPG <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_LP_IO_g", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPL <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_LP_IO_lr", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_LP_diff", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPI <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_LP_IO_diff", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_TFP <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_TFP_IO_diff", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_ROC <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_RoC_G_FI", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_IRT <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_FIAS_g", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_VAD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "COMPCAT", var_name = "def_VA", c_names = c("S", "M", "L", "VL"), neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)

save(fit_results_year_LIM, fit_results_year_LPR, fit_results_year_LPG, fit_results_year_LPL, fit_results_year_LPD, fit_results_year_LPI, fit_results_year_TFP, fit_results_year_ROC, fit_results_year_IRT, fit_results_year_VAD, file="China_fit_results_size.Rda")

## 5.2 cross-sectional (industry GB2002)
ind_name <- unique(df$Sector.Short)
load(file="China_fit_results_sect.Rda")

fit_results_year_LIM <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_LP", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPR <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_LP_IO", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPG <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_LP_IO_g", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPL <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_LP_IO_lr", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_LP_diff", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPI <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_LP_IO_diff", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_TFP <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_TFP_IO_diff", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_ROC <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_RoC_G_FI", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_IRT <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_FIAS_g", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_VAD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.Short", var_name = "def_VA", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)

save(fit_results_year_LIM, fit_results_year_LPR, fit_results_year_LPG, fit_results_year_LPL, fit_results_year_LPD, fit_results_year_LPI, fit_results_year_TFP, fit_results_year_ROC, fit_results_year_IRT, fit_results_year_VAD, file="China_fit_results_sect.Rda")

## 5.3 cross-sectional (province)
pro_name <- unique(df$Province)
load(file="China_fit_results_prov.Rda")

fit_results_year_LIM <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_LP", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPR <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_LP_IO", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPG <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_LP_IO_g", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPL <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_LP_IO_lr", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_LP_diff", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPI <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_LP_IO_diff", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_TFP <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_TFP_IO_diff", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_ROC <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_RoC_G_FI", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_IRT <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_FIAS_g", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_VAD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Province", var_name = "def_VA", c_names = pro_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)

save(fit_results_year_LIM, fit_results_year_LPR, fit_results_year_LPG, fit_results_year_LPL, fit_results_year_LPD, fit_results_year_LPI, fit_results_year_TFP, fit_results_year_ROC, fit_results_year_IRT, fit_results_year_VAD, file="China_fit_results_prov.Rda")

## 5.4 cross-sectional (firm type)
type_name <- unique(df$FirmType2)
load(file="China_fit_results_type.Rda")

fit_results_year_LIM <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_LP", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPR <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_LP_IO", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPG <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_LP_IO_g", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPL <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_LP_IO_lr", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_LP_diff", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPI <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_LP_IO_diff", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_TFP <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_TFP_IO_diff", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_ROC <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_RoC_G_FI", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_IRT <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_FIAS_g", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_VAD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "FirmType2", var_name = "def_VA", c_names = type_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)

save(fit_results_year_LIM, fit_results_year_LPR, fit_results_year_LPG, fit_results_year_LPL, fit_results_year_LPD, fit_results_year_LPI, fit_results_year_TFP, fit_results_year_ROC, fit_results_year_IRT, fit_results_year_VAD, file="China_fit_results_type.Rda")

## 5.5 cross-sectional (industry ISIC R4)
ind_name <- unique(df_cut$Sector.ISICR4)
load(file="China_fit_results_sect_ISICR4.Rda")

fit_results_year_LIM <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_LP", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPR <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_LP_IO", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPG <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_LP_IO_g", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPL <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_LP_IO_lr", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_LP_diff", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_LPI <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_LP_IO_diff", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_TFP <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_TFP_IO_diff", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_ROC <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_RoC_G_FI", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_IRT <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_FIAS_g", c_names = ind_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)
fit_results_year_VAD <- fun_fit_levy_AEP(dat=df_cut, bin_num=100, cond_name = "Sector.ISICR4", var_name = "def_VA", c_names = year_name, neg_cut = neg_cut, pov_cut = pov_cut, cut_num=1000)

save(fit_results_year_LIM, fit_results_year_LPR, fit_results_year_LPG, fit_results_year_LPL, fit_results_year_LPD, fit_results_year_LPI, fit_results_year_TFP, fit_results_year_ROC, fit_results_year_IRT, fit_results_year_VAD, file="China_fit_results_sect_ISICR4.Rda")
