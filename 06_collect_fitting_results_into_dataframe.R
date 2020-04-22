
parse_result_list <- function(lst, fit_Variable, separator_Variable) {
    #print("Enter")
    df3 <- data.frame(Fit_Variable=character(), Separator_Variable=character(), Year=integer(), Class=character(), class_idx=integer(), year_idx=integer(), Observations=integer(), Levy_alpha=double(), Levy_beta=double(), Levy_gamma=double(), Levy_delta=double(), Levy_Soofi_ID=double(), AEP_xi=double(), AEP_alpha=double(), AEP_kappa=double(), AEP_h=double(), AEP_Soofi_ID=double(), stringsAsFactors=FALSE)
    for (j in 1:length(lst)) {
        #print(j)
        #print(fit_Variable)
        #print(separator_Variable)
        if (!is.null(lst[[j]]) && !is.na(lst[[j]]) && length(lst[[j]])>0) {
            #print(lst[[j]])
            for (i in 1:length(lst[[j]])) {
                #print(j)
                #print(i)
                #print(fit_Variable)
                #print(separator_Variable)
                if (!is.null(lst[[j]][[i]]$AEP_parameters)) {
                    df3[nrow(df3)+1,] = list(fit_Variable, separator_Variable, lst[[j]][[i]]$year_name, lst[[j]][[i]]$class_name, lst[[j]][[i]]$year_num, lst[[j]][[i]]$class_num, lst[[j]][[i]]$number_observations, lst[[j]][[i]]$levy_parameters[[1]], lst[[j]][[i]]$levy_parameters[[2]], lst[[j]][[i]]$levy_parameters[[3]], lst[[j]][[i]]$levy_parameters[[4]],  lst[[j]][[i]]$levy_soofi_ID, lst[[j]][[i]]$AEP_parameters[[1]], lst[[j]][[i]]$AEP_parameters[[2]], lst[[j]][[i]]$AEP_parameters[[3]], lst[[j]][[i]]$AEP_parameters[[4]], lst[[j]][[i]]$AEP_soofi_ID)
                } else {
                    df3[nrow(df3)+1,] = list(fit_Variable, separator_Variable, lst[[j]][[i]]$year_name, lst[[j]][[i]]$class_name, lst[[j]][[i]]$year_num, lst[[j]][[i]]$class_num, lst[[j]][[i]]$number_observations, lst[[j]][[i]]$levy_parameters[[1]], lst[[j]][[i]]$levy_parameters[[2]], lst[[j]][[i]]$levy_parameters[[3]], lst[[j]][[i]]$levy_parameters[[4]],  lst[[j]][[i]]$levy_soofi_ID, NA, NA, NA, NA, lst[[j]][[i]]$AEP_soofi_ID)
                }
                #print(tail(df3))
            }
        }
    }
    df3$Soofi_Difference <- df3$Levy_Soofi_ID - df3$AEP_Soofi_ID
    return(df3)
}

df3 <- data.frame(Fit_Variable=character(), Separator_Variable=character(), Year=integer(), Class=character(), class_idx=integer(), year_idx=integer(), Observations=integer(), Levy_alpha=double(), Levy_beta=double(), Levy_gamma=double(), Levy_delta=double(), Levy_Soofi_ID=double(), AEP_xi=double(), AEP_alpha=double(), AEP_kappa=double(), AEP_h=double(), AEP_Soofi_ID=double(), stringsAsFactors=FALSE)

file_Name_List <- c("Figures/China_fit_results_prov.Rda", "Figures/China_fit_results_sect.Rda", "Figures/China_fit_results_size.Rda", "Figures/China_fit_results_type.Rda", "Figures/China_fit_results_year.Rda", "Figures/China_fit_results_sect_ISICR4.Rda")
    
separator_Variables <- c("Province", "Sector", "Size", "FirmType", "Year", "Sector.ISICR4")

variable_List <- c("fit_results_year_LIM", "fit_results_year_LPR", "fit_results_year_LPG", "fit_results_year_LPL", "fit_results_year_LPD", "fit_results_year_LPI", "fit_results_year_TFP", "fit_results_year_ROC", "fit_results_year_IRT")

variable_Name_List <- c("Labor productivity (imputed)", "Labor productivity", "Labor productivity growth", "Labor productivity log return", "Labor productivity (imputed) difference", "Labor productivity difference", "TFP", "Return on Capital", "Investment rate")

for (j in 1:length(file_Name_List)) {
    #print(file_Name_List[[j]])
    #print(file.exists(file_Name_List[[j]]))
    if (file.exists(file_Name_List[[j]])) {
        filename <- file_Name_List[[j]]
        load(filename, verbose=T)
        separator_Variable <- separator_Variables[[j]]
        for (i in 1:length(variable_List)) {
            if (exists(variable_List[[i]])) {
                current_Variable <- get(variable_List[[i]])
                df_next <- parse_result_list(current_Variable, variable_Name_List[[i]], separator_Variable)
                df3 <- rbind(df3, df_next)     
            }
        }
        fit_results_year_LPR <- NULL
        fit_results_year_LPG <- NULL
        fit_results_year_LPL <- NULL
        fit_results_year_LPD <- NULL
        fit_results_year_LPI <- NULL
        fit_results_year_TFP <- NULL
        fit_results_year_ROC <- NULL
        fit_results_year_IRT <- NULL
    }
}

setwd("~/datalake/CIEDB_2009_2013/")
save(df3, file="06_China_fit_results_dataframe.Rda")
#save(df3, file="China_fit_results_dataframe.Rda")
