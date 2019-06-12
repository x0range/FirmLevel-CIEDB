require(feather)
require(dplyr)

fun_data_clean <- function(data_c){
  data_c$ID_imputed[is.na(data_c$ID_imputed)] <- data_c$Phone_ZIP[is.na(data_c$ID_imputed)]
  #print(colnames(data_c))
  data_c$ID <- data_c$ID_imputed
  
  data_c <- data_c %>%
    #filter(!is.na(CLOSDATE_year)) %>% # year
    #filter(!is.na(IDNR)) %>% # firm indicator 
    #filter(CLOSDATE_year >= 2006 & CLOSDATE_year <= 2015) %>% # year cut 
    #filter(CONSOL == 'C1' | CONSOL == 'U1') %>% #  consolidate or unconsolidate with no companion statements 
    distinct(ID, Year, .keep_all = TRUE) %>% # remove duplicated rows
    
    # remove nonsensical values
    mutate(Employment = replace(Employment, Employment <= 0, NA), 
           TOAS = replace(TOAS, TOAS <= 0, NA),
           FIAS = replace(FIAS, FIAS <= 0, NA),
           TOAS = replace(TOAS, TOAS < FIAS, -1),
           FIAS = replace(FIAS, TOAS == -1, NA),
           TOAS = replace(TOAS, TOAS == -1, NA),
           Wages = replace(Wages, Wages < 0, NA),
           Sales = replace(Sales, Sales < 0, NA),
           Sales = replace(Intermediate.Input, Intermediate.Input < 0, NA)) %>%
            
    
    arrange(ID, Year) %>% # arranging data by firm and year
    
    # deflate variables
    
    mutate(
    #       def_RCEM = as.numeric(RCEM) / PPI,  # returns
    #       def_RTAS = as.numeric(RTAS) / PPI,
    #       def_EBTA = as.numeric(EBTA) / PPI,  # earnings
    #       def_EBIT = as.numeric(EBIT) / PPI,
    #       def_PL   = as.numeric(PL)   / PPI,  # profits
    #       def_PLAT = as.numeric(PLAT) / PPI,
    #       def_CF   = as.numeric(CF)   / PPI,  # cash flow
           def_EBTA.Gross.Profit = as.numeric(EBTA.Gross.Profit) / PPI,  #profits
           def_EBTA.Operating.Profit = as.numeric(EBTA.Operating.Profit) / PPI,  #
           def_Wages = as.numeric(Wages) / PPI,  # wages
           def_Deprecation = as.numeric(Deprecation) / PPI,  # depreciation
           def_TOAS = as.numeric(TOAS) / PPI,  # assets
           def_FIAS = as.numeric(FIAS) / PPI,
           def_Sales = as.numeric(Sales) / PPI,   #sales
           def_Intermediate.Input = as.numeric(Intermediate.Input) / PPI,   #Intermediate.Input
           def_Output = as.numeric(Output) / PPI   #Intermediate.Input
    ) #%>%
   
  print("Part A")
  data_c <- data_c %>%

    mutate(VA = as.numeric(EBTA.Gross.Profit) + as.numeric(Wages), #Imputed Value added (EBTA is earning before depreciation)
           VA_AD = as.numeric(EBTA.Operating.Profit) + as.numeric(Wages), #Imputed Value added (after depreciation, taxes etc)
           VA_IO = as.numeric(Output) - as.numeric(Intermediate.Input), #Value added computed from output and intermediate input instead
           
           LP = as.numeric(VA)/as.numeric(Employment), # Labor productivity
           CP = as.numeric(VA)/(as.numeric(FIAS)+as.numeric(Deprecation)), # capital productivity with fixed asset
           
           LP_AD= as.numeric(VA_AD)/as.numeric(Employment), # Labor productivity after depreciation (after depreciation, taxes etc)
           CP_AD = as.numeric(VA_AD)/as.numeric(FIAS), # capital productivity with fixed asset (after depreciation, taxes etc)
           
           LP_IO= as.numeric(VA_IO)/as.numeric(Employment), # Labor productivity
           CP_IO = as.numeric(VA_IO)/(as.numeric(FIAS)+as.numeric(Deprecation)), # capital productivity with fixed asset

           C_com = as.numeric(TOAS)/as.numeric(Wages), # capital intensity with total asset
           C_com_FI = as.numeric(FIAS)/as.numeric(Wages), # capital intensity with fixed asset
           
           #RoC_G = as.numeric(CF)/(as.numeric(TOAS)+as.numeric(Deprecation)), # gross profit rate with interest, CF = cash flow (gross profit + depreciation)
           RoC_G_FI = as.numeric(EBTA.Gross.Profit)/(as.numeric(FIAS)+as.numeric(Deprecation)), # gross profit rate without interest 
           
           #RoC_G_AD = as.numeric(PL)/as.numeric(TOAS), # gross profit rate with interest after depreciation
           RoC_G_AD_FI = as.numeric(EBTA.Operating.Profit)/as.numeric(FIAS), # gross profit rate without interest after depreciation 
           
           #RoC_N = as.numeric(PLAT)/as.numeric(TOAS), # net profit rate (after tax) 
             
           WS = as.numeric(Wages)/as.numeric(VA), # wage share
           WS_AD = as.numeric(Wages)/as.numeric(VA_AD), # wage share after depreciation
           WS_IO = as.numeric(Wages)/as.numeric(VA_IO), # wage share, with VA computed from output, input
            
           PW = as.numeric(EBTA.Gross.Profit)/as.numeric(Wages), # profit wage ratio
           PW_AD = as.numeric(EBTA.Operating.Profit)/as.numeric(Wages), # profit wage ratio after depreciation
           
           # And the same variables again deflated
           #  ...except for WS and PW as these are dimensionless and would be deflated with the same deflators
           
           def_VA = as.numeric(def_EBTA.Gross.Profit) + as.numeric(def_Wages), #Imputed Value added (EBTA is earning before depreciation)
           def_VA_AD = as.numeric(def_EBTA.Operating.Profit) + as.numeric(def_Wages), #Imputed Value added (EBTA is earning before depreciation)
           def_VA_IO = as.numeric(def_Output) - as.numeric(def_Intermediate.Input), #Value added computed from output and intermediate input instead
           
           def_LP = as.numeric(def_VA)/as.numeric(Employment), # Labor productivity
           def_CP = as.numeric(def_VA)/(as.numeric(def_FIAS)+as.numeric(def_Deprecation)), # capital productivity with fixed asset
           
           def_LP_AD = as.numeric(def_VA_AD)/as.numeric(Employment), # Labor productivity after depreciation
           def_CP_AD = as.numeric(def_VA_AD)/as.numeric(def_FIAS), # capital productivity with fixed asset
           
           def_LP_IO = as.numeric(def_VA_IO)/as.numeric(Employment), # Labor productivity
           def_CP_IO = as.numeric(def_VA_IO)/(as.numeric(def_FIAS)+as.numeric(def_Deprecation)), # capital productivity with fixed asset
           
           def_C_com = as.numeric(def_TOAS)/as.numeric(def_Wages), # capital intensity with total asset
           def_C_com_FI = as.numeric(def_FIAS)/as.numeric(def_Wages), # capital intensity with fixed asset
           
           #def_RoC_G = as.numeric(def_CF)/(as.numeric(def_TOAS)+as.numeric(def_Deprecation)), # gross profit rate with interest, CF = cash flow (gross profit + depreciation)
           def_RoC_G_FI = as.numeric(def_EBTA.Gross.Profit)/(as.numeric(def_FIAS)+as.numeric(def_Deprecation)), # gross profit rate without interest 
           
           #def_RoC_G_AD = as.numeric(def_PL)/as.numeric(def_TOAS), # gross profit rate with interest after depreciation
           def_RoC_G_AD_FI = as.numeric(def_EBTA.Operating.Profit)/as.numeric(def_FIAS)#, # gross profit rate without interest after depreciation 
           
           #def_RoC_N = as.numeric(def_PLAT)/as.numeric(def_TOAS) # net profit rate (after tax) 
           #  
           ) #%>% 

  print("Part A.2")
  data_c <- data_c %>%

    mutate(TFP = VA / (LP**WS + CP**(1-WS)), # TFP, imputed from accounting identity TFP = Y / (weighted inputs)
           TFP_AD = VA_AD / (LP_AD**WS_AD + CP_AD**(1-WS_AD)), # after depreciation
           TFP_IO = VA_IO / (LP_IO**WS_IO + CP_IO**(1-WS_IO)), # after depreciation
           def_TFP = def_VA / (def_LP**WS + def_CP**(1-WS)), # deflated
           def_TFP_AD = def_VA_AD / (def_LP_AD**WS_AD + def_CP_AD**(1-WS_AD)), 
           def_TFP_IO = def_VA_IO / (def_LP_IO**WS_IO + def_CP_IO**(1-WS_IO)) 
           )
           
  # firm size growth
  
  print("Part B")
  data_c <- data_c %>%
  
    group_by(ID) %>% # group by firm index
    
    mutate(Employment_g = (as.numeric(Employment) - 
                       lag(as.numeric(Employment),1))/lag(as.numeric(Employment),1),    # wrt employment 
           FIAS_g = (as.numeric(FIAS) - 
                       lag(as.numeric(FIAS),1))/lag(as.numeric(FIAS),1),    # wrt fixed assets -> this is the investment rate
           TOAS_g = (as.numeric(TOAS) - 
                       lag(as.numeric(TOAS),1))/lag(as.numeric(TOAS),1),    # wrt total assets
           SALE_g = (as.numeric(Sales) - 
                       lag(as.numeric(Sales),1))/lag(as.numeric(Sales),1),    # wrt sales
           
           # And the same variables again defalated
           
           def_FIAS_g = (as.numeric(def_FIAS) - 
                       lag(as.numeric(def_FIAS),1))/lag(as.numeric(def_FIAS),1),    # wrt fixed assets -> this is the investment rate
           def_TOAS_g = (as.numeric(def_TOAS) - 
                       lag(as.numeric(def_TOAS),1))/lag(as.numeric(def_TOAS),1),    # wrt total assets
           def_SALE_g = (as.numeric(def_Sales) - 
                       lag(as.numeric(def_Sales),1))/lag(as.numeric(def_Sales),1)    # wrt sales
           ) #%>% 
    
  # productivity growth

  print("Part C")
  data_c <- data_c %>%
    
    mutate(CP_g = (CP - lag(CP,1))/lag(CP,1),
           CP_AD_g = (CP_AD - lag(CP_AD,1))/lag(CP_AD,1),                   # capital productivity undeflated
           LP_g = (LP - lag(LP,1))/lag(LP,1),
           LP_AD_g = (LP_AD - lag(LP_AD,1))/lag(LP_AD,1),                   # labor productivity undeflated
           
           def_CP_g = (def_CP - lag(def_CP,1))/lag(def_CP,1),
           def_CP_AD_g = (def_CP_AD - lag(def_CP_AD,1))/lag(def_CP_AD,1),   # capital productivity deflated
           def_LP_g = (def_LP - lag(def_LP,1))/lag(def_LP,1),
           def_LP_AD_g = (def_LP_AD - lag(def_LP_AD,1))/lag(def_LP_AD,1),   # labor productivity deflated
           
           TFP_g = (TFP - lag(TFP,1))/lag(TFP,1),
           TFP_AD_g = (TFP_AD - lag(TFP_AD,1))/lag(TFP_AD,1),                 # TFP undeflated
           def_TFP_g = (def_TFP - lag(def_TFP,1))/lag(def_TFP,1),
           def_TFP_AD_g = (def_TFP_AD - lag(def_TFP_AD,1))/lag(def_TFP_AD,1),      # TFP deflated (both capital and labor productivity)
           
           CP_IO_g = (CP_IO - lag(CP_IO,1))/lag(CP_IO,1),                   # capital productivity undeflated
           LP_IO_g = (LP_IO - lag(LP_IO,1))/lag(LP_IO,1),                   # labor productivity undeflated
           def_CP_IO_g = (def_CP_IO - lag(def_CP_IO,1))/lag(def_CP_IO,1),   # capital productivity deflated
           def_LP_IO_g = (def_LP_IO - lag(def_LP_IO,1))/lag(def_LP_IO,1),   # labor productivity deflated
           TFP_IO_g = (TFP_IO - lag(TFP_IO,1))/lag(TFP_IO,1),                 # TFP undeflated
           def_TFP_IO_g = (def_TFP_IO - lag(def_TFP_IO,1))/lag(def_TFP_IO,1)      # TFP deflated (both capital and labor productivity)
           
           ) #%>% # G_CP
    
  # etc
    
  print("Part D")
  data_c <- data_c %>%

    mutate(def_FIAS_g_diff = def_FIAS_g - lag(def_FIAS_g,1)     # investment rate change
    mutate(PW_g = (PW - lag(PW,1))/lag(PW,1)) %>%               #
    mutate(PW_AD_g = (PW_AD - lag(PW_AD,1))/lag(PW_AD,1))       #

  # log returns
  
  print("Part E")
  data_c <- data_c %>%

    # firm size
    mutate(Employment_lr = log(as.numeric(Employment)/lag(as.numeric(Employment),1)),    # wrt employment 
           FIAS_lr = log(as.numeric(FIAS)/lag(as.numeric(FIAS),1)),    # wrt fixed assets
           TOAS_lr = log(as.numeric(TOAS)/lag(as.numeric(TOAS),1)),    # wrt total assets
           SALE_lr = log(as.numeric(Sales)/lag(as.numeric(Sales),1)),    # wrt sales
           
           # And the same variables again defalated
           
           def_FIAS_lr = log(as.numeric(def_FIAS)/lag(as.numeric(def_FIAS),1)),   # wrt fixed assets
           def_TOAS_lr = log(as.numeric(def_TOAS)/lag(as.numeric(def_TOAS),1)),   # wrt total assets
           def_SALE_lr = log(as.numeric(def_Sales)/lag(as.numeric(def_Sales),1))    # wrt sales
           ) %>% 
    
    # productivity 
    
    mutate(CP_lr = log(CP/lag(CP,1)),
           CP_AD_lr = log(CP_AD/lag(CP_AD,1)),                   # capital productivity undeflated
           LP_lr = log(LP/lag(LP,1)),
           LP_AD_lr = log(LP_AD/lag(LP_AD,1)),                   # labor productivity undeflated
           
           def_CP_lr = log(def_CP/lag(def_CP,1)),
           def_CP_AD_lr = log(def_CP_AD/lag(def_CP_AD,1)),       # capital productivity deflated
           def_LP_lr = log(def_LP/lag(def_LP,1)),
           def_LP_AD_lr = log(def_LP_AD/lag(def_LP_AD,1)),       # labor productivity deflated
           
           TFP_lr = log(TFP/lag(TFP,1)),
           TFP_AD_lr = log(TFP_AD/lag(TFP_AD,1)),               # TFP undeflated
           def_TFP_lr = log(def_TFP/lag(def_TFP,1)),
           def_TFP_AD_lr = log(def_TFP_AD/lag(def_TFP_AD,1)),    # TFP deflated 
           
           CP_IO_lr = log(CP_IO/lag(CP_IO,1)),                   # capital productivity undeflated
           LP_IO_lr = log(LP_IO/lag(LP_IO,1)),                   # labor productivity undeflated
           def_CP_IO_lr = log(def_CP_IO/lag(def_CP_IO,1)),       # capital productivity deflated
           def_LP_IO_lr = log(def_LP_IO/lag(def_LP_IO,1)),       # labor productivity deflated
           TFP_IO_lr = log(TFP_IO/lag(TFP_IO,1)),               # TFP undeflated
           def_TFP_IO_lr = log(def_TFP_IO/lag(def_TFP_IO,1))    # TFP deflated 
           ) %>% # G_CP
    
    # etc
    
    mutate(PW_lr = log(PW/lag(PW,1))) %>% #
    mutate(PW_AD_lr = log(PW_AD/lag(PW_AD,1))) #%>% #

  print("Part F")
  data_c <- data_c %>%
    # first difference variables
    mutate(CP_diff = CP - lag(CP,1),
           CP_AD_diff = CP_AD - lag(CP_AD,1),                   # capital productivity undeflated
           LP_diff = LP - lag(LP,1),
           LP_AD_diff = LP_AD - lag(LP_AD,1),                   # labor productivity undeflated
           
           def_CP_diff = def_CP - lag(def_CP,1),
           def_CP_AD_diff = def_CP_AD - lag(def_CP_AD,1),       # capital productivity deflated
           def_LP_diff = def_LP - lag(def_LP,1),
           def_LP_AD_diff = def_LP_AD - lag(def_LP_AD,1),       # labor productivity deflated
           
           TFP_diff = TFP - lag(TFP, 1),
           TFP_AD_diff = TFP_AD - lag(TFP_AD, 1),               # TFP undeflated
           def_TFP_diff = def_TFP - lag(def_TFP, 1),
           def_TFP_AD_diff = def_TFP_AD - lag(def_TFP_AD, 1),    # TFP deflated 
           
           CP_IO_diff = CP_IO - lag(CP_IO,1),                   # capital productivity undeflated
           LP_IO_diff = LP_IO - lag(LP_IO,1),                   # labor productivity undeflated
           def_CP_IO_diff = def_CP_IO - lag(def_CP_IO,1),       # capital productivity deflated
           def_LP_IO_diff = def_LP_IO - lag(def_LP_IO,1),       # labor productivity deflated
           TFP_IO_diff = TFP_IO - lag(TFP_IO, 1),               # TFP undeflated
           def_TFP_IO_diff = def_TFP_IO - lag(def_TFP_IO, 1)    # TFP deflated 
           ) 

  print("Part G")
  return(data_c)
}



df <- read_feather("All_years_reduced.feather")
df <- data.frame(df)
load("China_PPI_by_industry_and_year.Rda") # loads defl_reshaped
print("Loading completed")
df <- merge(x=df, y=defl_reshaped, by=c("Sector.Short", "Year"), all.x=T)
print("Merging completed")
df <- fun_data_clean(df)
print("Computing of compound measure completed")
save(df, file="China_data_set_incl_compounds.Rda")
