library(pacman)
pacman::p_load(dplyr, plm, xtable, tseries, lmtest, stargazer, heavy, finity) #, ggplot2, tidyr, plm


record_summary <- function(mod, name_mod="model", outfile=NA) {
  summary <- summary(mod)
  coefficients_latex <- xtable(summary$coefficients, digits=4)
  anova_latex <- if(is.null(mod$effects)) NULL else xtable(anova(mod))
  if (!is.na(outfile)) {
    call = paste(capture.output(print(mod$call)), collapse="")
    cat(paste("\n%", name_mod, "\n%", call, "\n% Coefficients \n\n"), file=outfile, append=TRUE)
    print.xtable(coefficients_latex, file=outfile, append=T)
    cat(paste("\n\n% ANOVA\n\n"), file=outfile, append=TRUE)
    if(!is.null(anova_latex)) print.xtable(anova_latex, file=outfile, append=T)
    cat(paste("\n\n\n"), file=outfile, append=TRUE)
  }
  return_object = list("name"=name_mod, "mod"=mod$call, "coefficients_latex"=coefficients_latex, "anova_latex"=anova_latex)
} 

# main entry point

setwd("~/datalake/CIEDB_2009_2013/")

# load MACRO sectoral vars
load("08_macro_data.Rda", verbose=T)  # df, sector_description

MACRO <- df %>%
  arrange(code, Year) %>%
  group_by(code) %>%
  mutate(Sector.ISICR4 = code,
         Sector_Employment_g = dplyr::lead(Employment_g, 1),
         Sector_VA_g = dplyr::lead(VA_g, 1)) %>%
  ungroup() %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  select(Year, Sector.ISICR4, Sector_Employment_g, Sector_VA_g)

# load firm level variables
load(file="08_data_complete_panels_annualized.Rda", verbose=T) # df

df <- df %>% 
  ungroup() %>%
  left_join(MACRO, by=c("Year", "Sector.ISICR4"), na_matches="never") %>%
  mutate(Firm.Size = ifelse((Employment >= 0 & Employment < 50), "S", 
                            ifelse((Employment >= 50 & Employment < 250), "M",
                                   ifelse((Employment >= 250 & Employment < 1500), "L", "VL"))))  %>%
  select(ID, Year, Sector, Sector.ISICR4, FirmType2, Employment, Employment_g_ann, Sector_Employment_g, def_LP, def_LP_diff, def_VA, 
         def_VA_g_ann, Sector_VA_g, Firm.Age, Firm.Size, FirmType2, Province) %>%
  group_by(ID, Sector)

#df2 <- df[which(is.finite(df$Employment_g_ann)),]
df3 <- do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))






### Most recent set of heavylm regressions

if (F) {
  
  va_lm024 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year)  + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_lm024)")
  print(summary(va_lm024))
  va_lm025 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_lm025)")
  print(summary(va_lm025))
  va_lm026 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_lm026)")
  print(summary(va_lm026))
  va_lm027 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_lm027)")
  print(summary(va_lm027))
  
  e_lm024 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g + def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_lm024)")
  print(summary(e_lm024))
  e_lm025 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g + I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_lm025)")
  print(summary(e_lm025))
  e_lm026 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g + def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_lm026)")
  print(summary(e_lm026))
  e_lm027 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g + I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_lm027)")
  print(summary(e_lm027))
  
}


# most recen set of lm ols regressions

if (F) {
  va_ols024 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year)  + factor(Sector.ISICR4), 
                  data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_ols024)")
  print(summary(va_ols024))
  va_ols025 <- lm(def_VA_g_ann ~ Sector_VA_g + I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                  data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_ols025)")
  print(summary(va_ols025))
  va_ols026 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                  data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_ols026)")
  print(summary(va_ols026))
  va_ols027 <- lm(def_VA_g_ann ~ Sector_VA_g + I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                  data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_ols027)")
  print(summary(va_ols027))
  
  e_ols024 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                 data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_ols024)")
  print(summary(e_ols024))
  e_ols025 <- lm(Employment_g_ann ~ Sector_Employment_g + I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                 data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_ols025)")
  print(summary(e_ols025))
  e_ols026 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                 data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_ols026)")
  print(summary(e_ols026))
  e_ols027 <- lm(Employment_g_ann ~ Sector_Employment_g + I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                 data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_ols027)")
  print(summary(e_ols027))

  va_ols028 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                 data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_ols028)")
  print(summary(va_ols028))
  va_ols029 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                 data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(va_ols029)")
  print(summary(va_ols029))
  e_ols028 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                 data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_ols028)")
  print(summary(e_ols028))
  e_ols029 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                 data=df3, na.action = na.exclude, family=Cauchy())
  print("summary(e_ols029)")
  print(summary(e_ols029))
  
  #summary_latex <- capture.output(stargazer(e_ols024, e_ols025, e_ols026, e_ols027))#, e_ols028, e_ols029))
  summary_latex <- capture.output(stargazer(e_ols024, e_ols025, e_ols026, e_ols027, e_ols028, e_ols029))
  cat(paste("\n%", "Employment models; comparison:", "\n\n"), file="09_regressions_v05.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_v05.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_v05.txt", append=TRUE)
  print(summary_latex)
  
  #summary_latex <- capture.output(stargazer(va_ols024, va_ols025, va_ols026, va_ols027))#, va_ols028, va_ols029))
  summary_latex <- capture.output(stargazer(va_ols024, va_ols025, va_ols026, va_ols027, va_ols028, va_ols029))
  cat(paste("\n%", "Employment models; comparison:", "\n\n"), file="09_regressions_v05.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_v05.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_v05.txt", append=TRUE)
  print(summary_latex)
  
}


# very first set of regressions. Do not run.

if (F) {
  #cor_coeff <- cor(df$Employment_diff_ann, df$def_VA_diff_ann, use="complete.obs")
  e_lm00 <- lm(Employment_g_ann ~ def_LP + def_LP_diff + Firm.Age, 
               data=df3, na.action = na.exclude)
  e_lm00_save <- record_summary(e_lm00, "e_lm00")
  
  # no fixed effects
  e_lm01 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff + Firm.Age, 
               data=df3, na.action = na.exclude)
  e_lm01_save <- record_summary(e_lm01, "e_lm01", outfile="09_regressions.txt")
  
  # Type and Size FE
  e_lm02 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size), 
               data=df3, na.action = na.exclude)
  e_lm02_save <- record_summary(e_lm02, "e_lm02", outfile="09_regressions.txt")
  
  # Type and Size and Sector FE
  e_lm03 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Sector.ISICR4), 
               data=df3, na.action = na.exclude)
  e_lm03_save <- record_summary(e_lm03, "e_lm03", outfile="09_regressions.txt")
  
  # Type and Size and Sector and Province FE
  e_lm04 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province), 
               data=df3, na.action = na.exclude)
  e_lm04_save <- record_summary(e_lm04, "e_lm04", outfile="09_regressions.txt")
  
  e_lm09 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
               data=df3, na.action = na.exclude)
  e_lm09_save <- record_summary(e_lm09, "e_lm09", outfile="09_regressions.txt")
  
  e_lm05 <- plm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size), 
                data=df3,
                index=c("ID", "Year"),
                model="within",                 #model="random"    #model="pooling"
                na.action = na.exclude,
  )
  e_lm05_save <- record_summary(e_lm05, "e_lm05", outfile="09_regressions.txt")
  
  e_lm06 <- plm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size), 
                data=df3,
                index=c("ID", "Year"),
                model="random",                 #model="random"    #model="pooling"
                na.action = na.exclude,
  )
  e_lm06_save <- record_summary(e_lm06, "e_lm06", outfile="09_regressions.txt")
  phtest(e_lm05, e_lm06)
  
  e_lm07 <- plm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size), 
                data=df3,
                index=c("ID", "Year"),
                model="pooling",                 #model="random"    #model="pooling"
                na.action = na.exclude,
  )
  e_lm07_save <- record_summary(e_lm07, "e_lm07", outfile="09_regressions.txt")
  
  e_lm08 <- plm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Year), 
                data=df3,
                index=c("ID", "Year"),
                model="within",                 #model="random"    #model="pooling"
                na.action = na.exclude,
  )
  e_lm08_save <- record_summary(e_lm08, "e_lm08", outfile="09_regressions.txt")
  
  summary_latex <- capture.output(stargazer(e_lm00, e_lm01, e_lm02, e_lm03, e_lm04, e_lm05, e_lm06, e_lm07, e_lm08))
  cat(paste("\n%", "Employment models; comparison:", "\n\n"), file="09_regressions.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions.txt", append=TRUE)
  
  # I have severe doubts if these tests are reliable with these large numbers of observations
  # they basically think that everything is significant and sill reject any null
  # That is: fixed effects better (phtest), time fixed effects (plmtest, although pFtest comes out as 1.0), serial correlation (pbgtest), 
  #          no unit root (adf.test), heteroskedasticity (bptest)
  
  cat(paste(capture.output(phtest(e_lm05, e_lm06)), combine="\n"), file="09_regressions.txt", append=TRUE)
  cat(paste(capture.output(plmtest(e_lm07, effect="twoways", type="ghm")), combine="\n"), file="09_regressions.txt", append=TRUE)
  cat(paste(capture.output(pFtest(e_lm08, e_lm07)), combine="\n"), file="09_regressions.txt", append=TRUE)
  
  
  cat(paste(capture.output(pbgtest(e_lm05)), combine="\n"), file="09_regressions.txt", append=TRUE)                    # yes, there is serial correlation, but since the time series are short, this should be not a problem
  #cannot do crossectional correlation tests, needs 1TB memory. Probably not a concern because of large data size. Is it?
  #pcdtest(va_lm05, test = c("lm"))   # Breusch-Pagan LM
  #pcdtest(va_lm05, test = c("cd"))   # Pesaran CD
  cat(paste(capture.output(adf.test(na.omit(df3$def_VA_g_ann), k=2)), combine="\n"), file="09_regressions.txt", append=TRUE)
  cat(paste(capture.output(bptest(e_lm05, studentize=T)), combine="\n"), file="09_regressions.txt", append=TRUE)
  
  # basic; no Sector growth, no FE
  va_hlm00 <- heavy::heavyLm(def_VA_g_ann ~ def_LP + def_LP_diff + Firm.Age, 
                             data=df3, na.action = na.exclude)
  va_lm00_save <- record_summary(va_hlm00, "va_hlm00", outfile="09_regressions_new.txt")
  
  # no fixed effects, but sector growth
  va_hlm09 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude)
  va_hlm09_save <- record_summary(va_hlm09, "va_hlm09", outfile="09_regressions_new.txt")
  
  # no fixed effects
  va_hlm01 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff + Firm.Age, 
                             data=df3, na.action = na.exclude)
  va_lm01_save <- record_summary(va_hlm01, "va_hlm01", outfile="09_regressions_new.txt")
  
  # Sector FE
  va_hlm11 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude)
  #va_hlm11_save <- record_summary(va_hlm11, "va_lm11", outfile="09_regressions_new.txt")
  #va_hlm11_save <- record_summary(va_hlm11, "va_hlm11", outfile="09_regressions_new.txt")
  
  # TODO: try also with errors Cauchy() instead of heavylm default Stundet()
  va_chlm09 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                              data=df3, na.action = na.exclude, family=Cauchy())
  va_chlm09_save <- record_summary(va_chlm09, "va_chlm09", outfile="09_regressions_new.txt")
  
}


# second set of regressions Do not run.

if (F) {
  
  # Employment
  
  
  # nothing; placeholder
  e_lm0 <- lm(Employment_g_ann ~ 1, 
              data=df3, na.action = na.exclude)
  e_lm0_save <- record_summary(e_lm000, "e_lm0")
  
  # basic; no Sector growth, no FE
  e_lm000 <- lm(Employment_g_ann ~ def_LP + def_LP_diff + Firm.Age, 
                data=df3, na.action = na.exclude)
  
  # no fixed effects, but sector growth
  e_lm001 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff + Firm.Age, 
                data=df3, na.action = na.exclude)
  
  # Sector FE
  e_lm002 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff +  Firm.Age + factor(Sector.ISICR4), 
                data=df3, na.action = na.exclude)
  
  # all fixed effects
  e_lm003 <- lm(Employment_g_ann ~ Sector_Employment_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                data=df3, na.action = na.exclude)
  
  # basic; no Sector growth, no FE
  e_lm010 <- lm(Employment_g_ann ~ def_LP*def_LP_diff*Firm.Age, 
                data=df3, na.action = na.exclude)
  
  # no fixed effects, but sector growth
  e_lm011 <- lm(Employment_g_ann ~ Sector_Employment_g*def_LP*def_LP_diff*Firm.Age, 
                data=df3, na.action = na.exclude)
  
  # Sector FE
  e_lm012 <- lm(Employment_g_ann ~ Sector_Employment_g*def_LP*def_LP_diff*Firm.Age + factor(Sector.ISICR4), 
                data=df3, na.action = na.exclude)
  
  # all fixed effects
  e_lm013 <- lm(Employment_g_ann ~ Sector_Employment_g*def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                data=df3, na.action = na.exclude)
  
  # basic; no Sector growth, no FE
  e_lm020 <- heavy::heavyLm(Employment_g_ann ~ def_LP*def_LP_diff*Firm.Age, 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  # no fixed effects, but sector growth
  e_lm021 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g*def_LP*def_LP_diff*Firm.Age, 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  # Sector FE
  e_lm022 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g*def_LP*def_LP_diff*Firm.Age + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  # all fixed effects
  e_lm023 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g*def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm000_save <- record_summary(e_lm000, "e_lm000", outfile="09_regressions_e_4d.txt")
  e_lm001_save <- record_summary(e_lm001, "e_lm001", outfile="09_regressions_e_4d.txt")
  e_lm002_save <- record_summary(e_lm002, "e_lm002", outfile="09_regressions_e_4d.txt")
  e_lm003_save <- record_summary(e_lm003, "e_lm003", outfile="09_regressions_e_4d.txt")
  e_lm010_save <- record_summary(e_lm010, "e_lm010", outfile="09_regressions_e_4d.txt")
  e_lm011_save <- record_summary(e_lm011, "e_lm011", outfile="09_regressions_e_4d.txt")
  e_lm012_save <- record_summary(e_lm012, "e_lm012", outfile="09_regressions_e_4d.txt")
  e_lm013_save <- record_summary(e_lm013, "e_lm013", outfile="09_regressions_e_4d.txt")
  e_lm020_save <- record_summary(e_lm020, "e_lm020", outfile="09_regressions_e_4d.txt")
  e_lm021_save <- record_summary(e_lm021, "e_lm021", outfile="09_regressions_e_4d.txt")
  e_lm022_save <- record_summary(e_lm022, "e_lm022", outfile="09_regressions_e_4d.txt")
  e_lm023_save <- record_summary(e_lm023, "e_lm023", outfile="09_regressions_e_4d.txt")
  
  
  
  
  
  summary_latex <- capture.output(stargazer(e_lm000, e_lm001, e_lm002, e_lm003, e_lm010, e_lm011, e_lm012, e_lm013, e_lm0))#, e_lm07, e_lm08))
  cat(paste("\n%", "Employment models; comparison:", "\n\n"), file="09_regressions_e_4d.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_e_4d.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_e_4d.txt", append=TRUE)
  
  summary_latex <- capture.output(stargazer(e_lm000, e_lm001, e_lm002, e_lm003, e_lm010, e_lm011, e_lm012, e_lm013, e_lm0, e_lm0, e_lm0, e_lm0))#, e_lm07, e_lm08))
  cat(paste("\n%", "Employment models; comparison:", "\n\n"), file="09_regressions_e_4d.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_e_4d.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_e_4d.txt", append=TRUE)
  
  summary_latex <- capture.output(stargazer(e_lm010, e_lm011, e_lm012, e_lm013, e_lm0))#, e_lm07, e_lm08))
  cat(paste("\n%", "Employment models; comparison:", "\n\n"), file="09_regressions_e_4d.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_e_4d.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_e_4d.txt", append=TRUE)
  
  summary_latex <- capture.output(stargazer(e_lm000, e_lm003, e_lm010, e_lm011, e_lm012, e_lm013))#, e_lm07, e_lm08))
  cat(paste("\n%", "Employment models; comparison:", "\n\n"), file="09_regressions_e_4d.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_e_4d.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_e_4d.txt", append=TRUE)
  
  
  
  ## VA
  
  
  # nothing; placeholder
  va_lm0 <- lm(def_VA_g_ann ~ 1, 
               data=df3, na.action = na.exclude)
  
  # basic; no Sector growth, no FE
  va_lm000 <- lm(def_VA_g_ann ~ def_LP + def_LP_diff + Firm.Age, 
                 data=df3, na.action = na.exclude)
  
  # no fixed effects, but sector growth
  va_lm001 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff + Firm.Age, 
                 data=df3, na.action = na.exclude)
  
  # Sector FE
  va_lm002 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff +  Firm.Age + factor(Sector.ISICR4), 
                 data=df3, na.action = na.exclude)
  
  # all fixed effects
  va_lm003 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                 data=df3, na.action = na.exclude)
  
  # basic; no Sector growth, no FE
  va_lm010 <- lm(def_VA_g_ann ~ def_LP*def_LP_diff*Firm.Age, 
                 data=df3, na.action = na.exclude)
  
  # no fixed effects, but sector growth
  va_lm011 <- lm(def_VA_g_ann ~ Sector_VA_g*def_LP*def_LP_diff*Firm.Age, 
                 data=df3, na.action = na.exclude)
  
  # Sector FE
  va_lm012 <- lm(def_VA_g_ann ~ Sector_VA_g*def_LP*def_LP_diff*Firm.Age + factor(Sector.ISICR4), 
                 data=df3, na.action = na.exclude)
  
  # all fixed effects
  va_lm013 <- lm(def_VA_g_ann ~ Sector_VA_g*def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                 data=df3, na.action = na.exclude)
  
  # basic; no Sector growth, no FE
  va_lm020 <- heavy::heavyLm(def_VA_g_ann ~ def_LP*def_LP_diff*Firm.Age, 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  # no fixed effects, but sector growth
  va_lm021 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g*def_LP*def_LP_diff*Firm.Age, 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  # Sector FE
  va_lm022 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g*def_LP*def_LP_diff*Firm.Age + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  # all fixed effects
  va_lm023 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g*def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  
  
  va_lm000_save <- record_summary(va_lm000, "va_lm000", outfile="09_regressions_va_4d.txt")
  va_lm001_save <- record_summary(va_lm001, "va_lm001", outfile="09_regressions_va_4d.txt")
  va_lm002_save <- record_summary(va_lm002, "va_lm002", outfile="09_regressions_va_4d.txt")
  va_lm003_save <- record_summary(va_lm003, "va_lm003", outfile="09_regressions_va_4d.txt")
  va_lm010_save <- record_summary(va_lm010, "va_lm010", outfile="09_regressions_va_4d.txt")
  va_lm011_save <- record_summary(va_lm011, "va_lm011", outfile="09_regressions_va_4d.txt")
  va_lm012_save <- record_summary(va_lm012, "va_lm012", outfile="09_regressions_va_4d.txt")
  va_lm013_save <- record_summary(va_lm013, "va_lm013", outfile="09_regressions_va_4d.txt")
  va_lm020_save <- record_summary(va_lm020, "va_lm020", outfile="09_regressions_va_4d.txt")
  va_lm021_save <- record_summary(va_lm021, "va_lm021", outfile="09_regressions_va_4d.txt")
  va_lm022_save <- record_summary(va_lm022, "va_lm022", outfile="09_regressions_va_4d.txt")
  va_lm023_save <- record_summary(va_lm023, "va_lm023", outfile="09_regressions_va_4d.txt")
  
  
  
  
  
  summary_latex <- capture.output(stargazer(va_lm000, va_lm001, va_lm002, va_lm003, va_lm010, va_lm011, va_lm012, va_lm013, va_lm0))#, va_lm07, va_lm08))
  cat(paste("\n%", "VA models; comparison:", "\n\n"), file="09_regressions_va_4d.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_va_4d.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_va_4d.txt", append=TRUE)
  
  summary_latex <- capture.output(stargazer(va_lm000, va_lm001, va_lm002, va_lm003, va_lm010, va_lm011, va_lm012, va_lm013, va_lm0, va_lm0, va_lm0, va_lm0))#, va_lm07, va_lm08))
  cat(paste("\n%", "VA models; comparison:", "\n\n"), file="09_regressions_va_4d.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_va_4d.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_va_4d.txt", append=TRUE)
  
  summary_latex <- capture.output(stargazer(va_lm010, va_lm011, va_lm012, va_lm013, va_lm0))#, va_lm07, va_lm08))
  cat(paste("\n%", "VA models; comparison:", "\n\n"), file="09_regressions_va_4d.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_va_4d.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_va_4d.txt", append=TRUE)
  
  summary_latex <- capture.output(stargazer(va_lm000, va_lm003, va_lm010, va_lm011, va_lm012, va_lm013))#, va_lm07, va_lm08))
  cat(paste("\n%", "VA models; comparison:", "\n\n"), file="09_regressions_va_4d.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions_va_4d.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions_va_4d.txt", append=TRUE)
  
  
  
  
  #### NExt
  
  va_lm023 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g*def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm030 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm031 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm032 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP_diff) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm033 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm034 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP*def_LP_diff) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm035 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm036 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm037 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm038 <- heavy::heavyLm(def_VA_g_ann ~ I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm039 <- heavy::heavyLm(def_VA_g_ann ~ def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  print("summary(va_lm023)")
  print(summary(va_lm023))
  print("summary(va_lm030)")
  print(summary(va_lm030))
  print("summary(va_lm031)")
  print(summary(va_lm031))
  print("summary(va_lm032)")
  print(summary(va_lm032))
  print("summary(va_lm033)")
  print(summary(va_lm033))
  print("summary(va_lm034)")
  print(summary(va_lm034))
  print("summary(va_lm035)")
  print(summary(va_lm035))
  print("summary(va_lm036)")
  print(summary(va_lm036))
  print("summary(va_lm037)")
  print(summary(va_lm037))
  print("summary(va_lm038)")
  print(summary(va_lm038))
  print("summary(va_lm039)")
  print(summary(va_lm039))
  
  
  
  
  
  
  #### NExt
  
  e_lm023 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g*def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm030 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm031 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm032 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP_diff) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm033 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm034 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP*def_LP_diff) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm035 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm036 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm037 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm038 <- heavy::heavyLm(Employment_g_ann ~ I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm039 <- heavy::heavyLm(Employment_g_ann ~ def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  print("summary(e_lm023)")
  print(summary(e_lm023))
  print("summary(e_lm030)")
  print(summary(e_lm030))
  print("summary(e_lm031)")
  print(summary(e_lm031))
  print("summary(e_lm032)")
  print(summary(e_lm032))
  print("summary(e_lm033)")
  print(summary(e_lm033))
  print("summary(e_lm034)")
  print(summary(e_lm034))
  print("summary(e_lm035)")
  print(summary(e_lm035))
  print("summary(e_lm036)")
  print(summary(e_lm036))
  print("summary(e_lm037)")
  print(summary(e_lm037))
  print("summary(e_lm038)")
  print(summary(e_lm038))
  print("summary(e_lm039)")
  print(summary(e_lm039))
  
  
  
  
  #### NExt
  
  va_lm023 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g*def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm030 <- heavy::heavyLm(def_VA_g_ann ~ Sector_VA_g + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm031 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm032 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP_diff) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm033 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm034 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP*def_LP_diff) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm035 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm036 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm037 <- heavy::heavyLm(def_VA_g_ann ~ I(Sector_VA_g*def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm038 <- heavy::heavyLm(def_VA_g_ann ~ I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  va_lm039 <- heavy::heavyLm(def_VA_g_ann ~ def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                             data=df3, na.action = na.exclude, family=Cauchy())
  
  print("summary(va_lm023)")
  print(summary(va_lm023))
  print("summary(va_lm030)")
  print(summary(va_lm030))
  print("summary(va_lm031)")
  print(summary(va_lm031))
  print("summary(va_lm032)")
  print(summary(va_lm032))
  print("summary(va_lm033)")
  print(summary(va_lm033))
  print("summary(va_lm034)")
  print(summary(va_lm034))
  print("summary(va_lm035)")
  print(summary(va_lm035))
  print("summary(va_lm036)")
  print(summary(va_lm036))
  print("summary(va_lm037)")
  print(summary(va_lm037))
  print("summary(va_lm038)")
  print(summary(va_lm038))
  print("summary(va_lm039)")
  print(summary(va_lm039))
  
  
  #### NExt
  
  e_lm023 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g*def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm030 <- heavy::heavyLm(Employment_g_ann ~ Sector_Employment_g + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm031 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm032 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP_diff) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm033 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm034 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP*def_LP_diff) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm035 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm036 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm037 <- heavy::heavyLm(Employment_g_ann ~ I(Sector_Employment_g*def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm038 <- heavy::heavyLm(Employment_g_ann ~ I(def_LP*def_LP_diff*Firm.Age) + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  e_lm039 <- heavy::heavyLm(Employment_g_ann ~ def_LP*def_LP_diff*Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year), 
                            data=df3, na.action = na.exclude, family=Cauchy())
  
  print("summary(e_lm023)")
  print(summary(e_lm023))
  print("summary(e_lm030)")
  print(summary(e_lm030))
  print("summary(e_lm031)")
  print(summary(e_lm031))
  print("summary(e_lm032)")
  print(summary(e_lm032))
  print("summary(e_lm033)")
  print(summary(e_lm033))
  print("summary(e_lm034)")
  print(summary(e_lm034))
  print("summary(e_lm035)")
  print(summary(e_lm035))
  print("summary(e_lm036)")
  print(summary(e_lm036))
  print("summary(e_lm037)")
  print(summary(e_lm037))
  print("summary(e_lm038)")
  print(summary(e_lm038))
  print("summary(e_lm039)")
  print(summary(e_lm039))
  
}

# other old regressions. do not run.

if (F) {
  
  # Type and Size FE
  va_lm02 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size), 
                data=df3, na.action = na.exclude)
  va_lm02_save <- record_summary(va_lm02, "va_lm02", outfile="09_regressions.txt")
  
  # Type and Size and Sector FE
  va_lm03 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Sector.ISICR4), 
                data=df3, na.action = na.exclude)
  va_lm03_save <- record_summary(va_lm03, "va_lm03", outfile="09_regressions.txt")
  
  # Type and Size and Sector and Province FE
  va_lm04 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province), 
                data=df3, na.action = na.exclude)
  va_lm04_save <- record_summary(va_lm04, "va_lm04", outfile="09_regressions.txt")
}










# OLD lm and plm regressions with tests 
if (F) {
  
  va_lm05 <- plm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size), 
                 data=df3,
                 index=c("ID", "Year"),
                 model="within",                 #model="random"    #model="pooling"
                 na.action = na.exclude,
  )
  va_lm05_save <- record_summary(va_lm05, "va_lm05", outfile="09_regressions.txt")
  
  va_lm06 <- plm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size), 
                 data=df3,
                 index=c("ID", "Year"),
                 model="random",                 #model="random"    #model="pooling"
                 na.action = na.exclude,
  )
  va_lm06_save <- record_summary(va_lm06, "va_lm06", outfile="09_regressions.txt")
  phtest(va_lm05, va_lm06)
  
  va_lm07 <- plm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size), 
                 data=df3,
                 index=c("ID", "Year"),
                 model="pooling",                 #model="random"    #model="pooling"
                 na.action = na.exclude,
  )
  va_lm07_save <- record_summary(va_lm07, "va_lm07", outfile="09_regressions.txt")
  
  va_lm08 <- plm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff + Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Year), 
                 data=df3,
                 index=c("ID", "Year"),
                 model="within",                 #model="random"    #model="pooling"
                 na.action = na.exclude,
  )
  va_lm08_save <- record_summary(va_lm08, "va_lm08", outfile="09_regressions.txt")
  
  va_lm09 <- lm(def_VA_g_ann ~ Sector_VA_g + def_LP + def_LP_diff +  Firm.Age + factor(FirmType2) + factor(Firm.Size) + factor(Province) + factor(Year) + factor(Sector.ISICR4), 
                data=df3, na.action = na.exclude, family=Cauchy())
  va_lm09_save <- record_summary(va_lm09, "va_lm09", outfile="09_regressions_new.txt")
  
  summary_latex <- capture.output(stargazer(va_lm00, va_lm01, va_lm02, va_lm03, va_lm04, va_lm05, va_lm06, va_lm07, va_lm08))
  cat(paste("\n%", "VA models; comparison:", "\n\n"), file="09_regressions.txt", append=TRUE)
  cat(paste(summary_latex, collapse = "\n"), "\n\n\n", file="09_regressions.txt", append=TRUE)
  cat(paste("\n\n\n"), file="09_regressions.txt", append=TRUE)
  
  
  # I have severe doubts if these tests are reliable with these large numbers of observations
  # they basically think that everything is significant and sill reject any null
  # That is: fixed effects better (phtest), time fixed effects (plmtest, although pFtest comes out as 1.0), serial correlation (pbgtest), 
  #          no unit root (adf.test), heteroskedasticity (bptest)
  
  cat(paste(capture.output(phtest(va_lm05, va_lm06)), combine="\n"), file="09_regressions.txt", append=TRUE)
  cat(paste(capture.output(plmtest(va_lm07, effect="twoways", type="ghm")), combine="\n"), file="09_regressions.txt", append=TRUE)
  cat(paste(capture.output(pFtest(va_lm08, va_lm07)), combine="\n"), file="09_regressions.txt", append=TRUE)
  
  cat(paste(capture.output(pbgtest(va_lm05)), combine="\n"), file="09_regressions.txt", append=TRUE)                    # yes, there is serial correlation, but since the time series are short, this should be not a problem
  #cannot do crossectional correlation tests, needs 1TB memory. Probably not a concern because of large data size. Is it?
  #pcdtest(va_lm05, test = c("lm"))   # Breusch-Pagan LM
  #pcdtest(va_lm05, test = c("cd"))   # Pesaran CD
  cat(paste(capture.output(adf.test(na.omit(df3$def_VA_g_ann), k=2)), combine="\n"), file="09_regressions.txt", append=TRUE)
  cat(paste(capture.output(bptest(va_lm05, studentize=T)), combine="\n"), file="09_regressions.txt", append=TRUE)
  
  # Unfortunately the regressions fail the finite moment test for 2nd moment of residuals
  # First moment is certainly finite:
  #finity::finite_moment_test(va_lm09$residuals, 1., ignore_errors = T, psi=.5)
  #[,1]
  #[1,] 1714.954
  #[2,]    0.000
  ## -> finite
  # Second moment is not:
  finity::finite_moment_test(va_lm09$residuals, 2., ignore_errors = T, psi=1.)
  #[,1]
  #[1,] 1.665085e-05
  #[2,] 9.967442e-01
  ## -> not finite
  finity::finite_moment_test(va_lm09$residuals, 2.0, ignore_errors = T, psi=1.4)
  #[,1]
  #[1,] 1.665085e-05
  #[2,] 9.967442e-01
  ## -> not finite
  # Only becomes finite for particular choices of psi very close to k which degrades the power of the test:
  finity::finite_moment_test(va_lm09$residuals, 2.0, ignore_errors = T, psi=1.8)
  #[,1]
  #[1,] 4.464115e+02
  #[2,] 4.355998e-99
  ## -> finite??
}