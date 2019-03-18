
function_set_FirmType2 <- function(df, FT_numbers, FT2_name) {
  for (FT_number in FT_numbers) {
    df$FirmType2[df$FirmType==FT_number] <- FT2_name
  }
  return(df)
}


setwd("/home/mercury/dat/CIEDB_2009_2013/")
load("China_data_set_incl_compounds.Rda", verbose=T)
df$FirmType2 <- NA

df <- function_set_FirmType2(df, c(110,141,151), "SOE")
df <- function_set_FirmType2(df, c(120,130,142), "Collective owned")
df <- function_set_FirmType2(df, c(159,160), "Shareholding")
df <- function_set_FirmType2(df, c(171,172,173,174), "Private")
df <- function_set_FirmType2(df, c(210,220,230,240,290), "HK, Macao, Taiwan owned")
df <- function_set_FirmType2(df, c(310,320,330,340,390), "Foreign owned")
df <- function_set_FirmType2(df, c(143, 149, 190), "Other domestic")

print(table(df$FirmType2))

save(df, file="dataframe_including_FirmType2.Rda")
