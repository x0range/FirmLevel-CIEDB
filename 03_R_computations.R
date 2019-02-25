require(feather)
require(dplyr)

fun_data_clean <- function(data_c){
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
           Sales = replace(Sales, Sales < 0, NA)) %>%
            
    
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
           def_Sales = as.numeric(Sales) / PPI   #sales
    ) #%>%
   
  print("Part A")
  data_c <- data_c %>%

    mutate(VA = as.numeric(EBTA.Gross.Profit) + as.numeric(Wages), #Imputed Value added (EBTA is earning before depreciation)
           VA_AD = as.numeric(EBTA.Operating.Profit) + as.numeric(Wages), #Imputed Value added (EBTA is earning before depreciation)
           
           LP= as.numeric(VA)/as.numeric(Employment), # Labor productivity
           CP = as.numeric(VA)/(as.numeric(FIAS)+as.numeric(Deprecation)), # capital productivity with fixed asset
           
           LP_AD= as.numeric(VA_AD)/as.numeric(Employment), # Labor productivity after depreciation
           CP_AD = as.numeric(VA_AD)/as.numeric(FIAS), # capital productivity with fixed asset
           
           C_com = as.numeric(TOAS)/as.numeric(Wages), # capital intensity with total asset
           C_com_FI = as.numeric(FIAS)/as.numeric(Wages), # capital intensity with fixed asset
           
           #RoC_G = as.numeric(CF)/(as.numeric(TOAS)+as.numeric(Deprecation)), # gross profit rate with interest, CF = cash flow (gross profit + depreciation)
           RoC_G_FI = as.numeric(EBTA.Gross.Profit)/(as.numeric(FIAS)+as.numeric(Deprecation)), # gross profit rate without interest 
           
           #RoC_G_AD = as.numeric(PL)/as.numeric(TOAS), # gross profit rate with interest after depreciation
           RoC_G_AD_FI = as.numeric(EBTA.Operating.Profit)/as.numeric(FIAS), # gross profit rate without interest after depreciation 
           
           #RoC_N = as.numeric(PLAT)/as.numeric(TOAS), # net profit rate (after tax) 
             
           WS = as.numeric(Wages)/as.numeric(VA), # wage share
           WS_AD = as.numeric(Wages)/as.numeric(VA_AD), # wage share after depreciation
            
           PW = as.numeric(EBTA.Gross.Profit)/as.numeric(Wages), # profit wage ratio
           PW_AD = as.numeric(EBTA.Operating.Profit)/as.numeric(Wages), # profit wage ratio after depreciation
           
           # And the same variables again deflated
           #  ...except for WS and PW as these are dimensionless and would be deflated with the same deflators
           
           def_VA = as.numeric(def_EBTA.Gross.Profit) + as.numeric(def_Wages), #Imputed Value added (EBTA is earning before depreciation)
           def_VA_AD = as.numeric(def_EBTA.Operating.Profit) + as.numeric(def_Wages), #Imputed Value added (EBTA is earning before depreciation)
           
           def_LP= as.numeric(def_VA)/as.numeric(Employment), # Labor productivity
           def_CP = as.numeric(def_VA)/(as.numeric(def_FIAS)+as.numeric(def_Deprecation)), # capital productivity with fixed asset
           
           def_LP_AD= as.numeric(def_VA_AD)/as.numeric(Employment), # Labor productivity after depreciation
           def_CP_AD = as.numeric(def_VA_AD)/as.numeric(def_FIAS), # capital productivity with fixed asset
           
           def_C_com = as.numeric(def_TOAS)/as.numeric(def_Wages), # capital intensity with total asset
           def_C_com_FI = as.numeric(def_FIAS)/as.numeric(def_Wages), # capital intensity with fixed asset
           
           #def_RoC_G = as.numeric(def_CF)/(as.numeric(def_TOAS)+as.numeric(def_Deprecation)), # gross profit rate with interest, CF = cash flow (gross profit + depreciation)
           def_RoC_G_FI = as.numeric(def_EBTA.Gross.Profit)/(as.numeric(def_FIAS)+as.numeric(def_Deprecation)), # gross profit rate without interest 
           
           #def_RoC_G_AD = as.numeric(def_PL)/as.numeric(def_TOAS), # gross profit rate with interest after depreciation
           def_RoC_G_AD_FI = as.numeric(def_EBTA.Operating.Profit)/as.numeric(def_FIAS), # gross profit rate without interest after depreciation 
           
           #def_RoC_N = as.numeric(def_PLAT)/as.numeric(def_TOAS) # net profit rate (after tax) 
           #  
           ) #%>% 


  print("Part B")
  data_c <- data_c %>%
  
    group_by(ID) %>% # group by firm index
    
    # firm size growth
    mutate(Employment_g = (as.numeric(Employment) - 
                       lag(as.numeric(Employment),1))/lag(as.numeric(Employment),1),    # wrt employment 
           FIAS_g = (as.numeric(FIAS) - 
                       lag(as.numeric(FIAS),1))/lag(as.numeric(FIAS),1),    # wrt fixed assets
           TOAS_g = (as.numeric(TOAS) - 
                       lag(as.numeric(TOAS),1))/lag(as.numeric(TOAS),1),    # wrt total assets
           SALE_g = (as.numeric(Sales) - 
                       lag(as.numeric(Sales),1))/lag(as.numeric(Sales),1),    # wrt sales
           
           # And the same variables again defalated
           
           def_FIAS_g = (as.numeric(def_FIAS) - 
                       lag(as.numeric(def_FIAS),1))/lag(as.numeric(def_FIAS),1),    # wrt fixed assets
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
           
           Zeta = CP_g * (1-WS) + LP_g * WS,
           Zeta_AD = CP_AD_g * (1-WS_AD) + LP_AD_g * WS_AD,                 # TFP undeflated
           lpdef_Zeta = CP_g * (1-WS) + def_LP_g * WS,
           lpdef_Zeta_AD = CP_AD_g * (1-WS_AD) + def_LP_AD_g * WS_AD,       # TFP with only capital productivity deflated (labor productivity undeflated)
           def_Zeta = def_CP_g * (1-WS) + def_LP_g * WS,
           def_Zeta_AD = def_CP_AD_g * (1-WS_AD) + def_LP_AD_g * WS_AD      # TFP deflated (both capital and labor productivity)
           ) #%>% # G_CP
    
    # etc
    
  print("Part D")
  data_c <- data_c %>%

    mutate(PW_g = (PW - lag(PW,1))/lag(PW,1)) %>% #
    mutate(PW_AD_g = (PW_AD - lag(PW_AD,1))/lag(PW_AD,1)) #
    
  print("Part E")
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
