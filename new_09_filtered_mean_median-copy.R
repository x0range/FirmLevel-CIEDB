set.seed(44)

library(pacman)
pacman::p_load(dplyr, plm, xtable, tseries, lmtest, stargazer, heavy, finity, quantreg) #, ggplot2, tidyr, plm


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

#### my code from here
set.seed(1000)
sam_ind <- sample(1:nrow(df3), 100000, replace = F) # ample 100,000 since it is not easy to handle 4 million obs. The results are qualitatively similar. 

# sampled emp
df3_emp_sam <- df3[sam_ind, c("Employment_g_ann", "Sector_Employment_g")]
df3_emp_sam  <- na.omit(df3_emp_sam)

#quantile regression from quantreg package
q_med <- rq(y ~ x, data=data.frame(y = df3_emp_sam$Employment_g_ann*100, x = df3_emp_sam$Sector_Employment_g*100))
pred_med_emp <- predict(q_med, newdata = data.frame(x = df3_emp_sam$Sector_Employment_g*100))

#sampled va
df3_va_sam <- df3[sam_ind, c("def_VA_g_ann", "Sector_VA_g")]
df3_va_sam  <- na.omit(df3_va_sam)

q_med <- rq(y ~ x, data=data.frame(y = df3_va_sam$def_VA_g_ann*100, x = df3_va_sam$Sector_VA_g*100))
pred_med_va <- predict(q_med, newdata = data.frame(x = df3_va_sam$Sector_VA_g*100))

## plot 
pdf(paste("filtered_mean_median.pdf", sep = ""), height = 4., width = 8)
par(mfrow = c(1, 2), mar = c(3, 3, 1, 1), mgp = c(1.7, .3, 0), tck = -.01, oma = c(0, 0, 2, 0), las = 1)

# mean filter (LOESS)
scatter.smooth(df3_emp_sam$Employment_g_ann*100 ~ df3_emp_sam$Sector_Employment_g*100, span = 2/3, degree = 2, pch = 16, cex = 0., yaxt = "n", xaxt = "n", main = expression("Employment Growth"),  xlab="Growth at Sectoral Level (%)", ylab="Growth at Firm Level (%)", axes=FALSE, lpars = list(col="black", lwd = 2, lty = 2), ylim = c(-1.2,0.5), cex.lab = 0.9) 

axis(side = 1, lwd = 0.3, cex.axis = .9, las = 1)
axis(side = 2, lwd = 0.3, cex.axis = .9, las = 1)

# medain filter 
lines(df3_emp_sam$Sector_Employment_g*100, pred_med_emp, lwd = 1)

legend("bottomright", legend = c("Mean filter", "Median filter"), lty = c(2,1), bty = "n", xpd = NA, cex = 1.1, ncol = 1,  pt.cex = 1.8)

scatter.smooth(df3_va_sam$def_VA_g_ann*100 ~ df3_va_sam$Sector_VA_g*100, span = 2/3, degree = 2, pch = 16, cex = 0., yaxt = "n", xaxt = "n", main = expression("Value-added Growth"),  xlab="Growth at Sectoral Level (%)", ylab="Growth at Firm Level (%)", axes=FALSE, lpars = list(col="black", lwd = 2, lty = 2), ylim = c(10,50), cex.lab = 0.9) 

axis(side = 1, lwd = 0.3, cex.axis = .9, las = 1)
axis(side = 2, lwd = 0.3, cex.axis = .9, las = 1)

lines(df3_va_sam$Sector_VA_g*100, pred_med_va, lwd = 1)

legend("bottomright", legend = c("Mean filter", "Median filter"), lty = c(2,1), bty = "n", xpd = NA, cex = 1.1, ncol = 1,  pt.cex = 1.8)

dev.off()
# 
# b_s2 <- df3 %>%
#   #group_by(Sector_Employment_g) %>%
#   filter(Employment_g_ann < quantile(Employment_g_ann, 0.9975, na.rm = T) & Employment_g_ann > quantile(Employment_g_ann, 0.0025, na.rm = T))%>%
#   filter(def_LP < quantile(def_LP, 0.9975, na.rm = T) & def_LP > quantile(def_LP, 0.0025, na.rm = T)) %>%
#   mutate(def_LP_bin = ntile(def_LP, 10)) %>%
#   group_by(def_LP_bin)%>%
#   summarise(Employment_g_ann_mean = mean(Employment_g_ann, na.rm = T),
#             Employment_g_ann_med = median(Employment_g_ann, na.rm = T),
#             n = n()) 
# 
# aa <- subset(b_s2, def_LP_bin == 10)
# 
# 
# pdf("mean.vs.med_LP.pdf", height=4, width=8)
# par(mfrow=c(1,2), mar=c(3, 3, 2, 2), mgp=c(1.5,0.3,0), tck=-.01, oma=c(0,0,1,0), las= 1)
# 
# plot(b_s2$def_LP_bin, b_s2$Employment_g_ann_mean, type = "l", xlab = "def_LP", ylab = "Mean_Employment_g_ann", main = "Mean")
# plot(b_s2$def_LP_bin, b_s2$Employment_g_ann_med, type = "l", xlab = "def_LP", ylab = "Median_Employment_g_ann", main = "Median")
# dev.off()

