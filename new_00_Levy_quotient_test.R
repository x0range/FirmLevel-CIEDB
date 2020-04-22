library(pacman)
pacman::p_load(dplyr,RColorBrewer,colorspace,stabledist,finity)



fun_plot_marginal <- function(df, cond_name="x", var_name="y", pdf_name="00_Levy_quotient_test", x_lab="", 
                              title="", c_names=NA, neg_cut=0.005, pov_cut=0.995, cut_num=1000, bin_num=100) {
  ### LP_change
  
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 4.8, width = 4)
  #par(mfrow=c(1,1), mar=c(3, 2.5, 1, 1), mgp=c(1.5,.3,0), tck=-.01, oma=c(0,0,2,0))
  
  var_ind <- match(var_name, colnames(df))
  cond_ind <- match(cond_name, colnames(df))
  
  df <- as.data.frame(df)
  names(df)
  print(colnames(df))
  print(cond_name)
  print(cond_ind)
  df$Cond <- df[, cond_ind] # conditional on this variable (categorical variable)
  df$Var <- df[, var_ind] # take this variable 
  
  #df$Var[!is.finite(df$Var)] <- NA
  
  df <- df %>%
    select(Cond, Var) %>%
    na.omit() %>%
    filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tails
    group_by(Cond) %>%
    filter(length(Var) > cut_num) # cut_num: the minimum # of obs for each class
  
  if (nrow(df)>cut_num) {
    df_info <- df %>% # for the min and maz value of the y and x axis
      group_by(Cond) %>%
      summarise(x_min = min(hist(Var, breaks = seq(min(Var), max(Var),l= bin_num+1), plot = F)$mids),
                x_max = max(hist(Var, breaks = seq(min(Var), max(Var),l= bin_num+1), plot = F)$mids),
                y_min = min(hist(Var, breaks = seq(min(Var), max(Var),l= bin_num+1), plot = F)$density[hist(Var, breaks = seq(min(Var), max(Var),l= bin_num+1), plot = F)$density > 0]),
                y_max = max(hist(Var, breaks = seq(min(Var), max(Var),l= bin_num+1), plot = F)$density)
      )
    
    x_min <- min(df_info$x_min)
    y_min <- min(df_info$y_min)
    x_max <- max(df_info$x_max)
    y_max <- max(df_info$y_max) 
    
    c_uni <- sort(unique(df$Cond))
    
    color_ind <- rainbow_hcl(length(c_names))
    
    plot(c(x_min, x_max), c(y_min, y_max), cex = 0,  log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = x_lab, ylab = "Log-Density", main = title, cex.main = 0.8)
    axis(side = 1, lwd = 0.3, cex.axis=0.9)
    axis(side = 2, lwd = 0.3, cex.axis=.9)
    
    q_25 <- function(n){ ## to use the pch argument in the plot function with more than 25 categories (sectors and provinces)
      apply(data.frame(n), 1, function(x) ifelse(x > 25, x-25,x)  )
    } 
    
    for(c in 1:length(c_uni)){
      print(c_uni[c])
      c_lp <- df$Var[df$Cond == c_uni[c]] # get each category
      print("Moment 1")
      print(finity::finite_moment_test(c_lp, 1.0))
      print("Moment 2")
      print(finity::finite_moment_test(c_lp, 2.0))
      print("Moment 3")
      print(finity::finite_moment_test(c_lp, 3.0))
      c_hist <- hist(c_lp, breaks = seq(min(c_lp), max(c_lp),l= bin_num+1), plot = F)   
      c_ind <- which(c_names%in%c_uni[c]) # ignore this (it is from another script where I keep the legend to be consistent across countries)
      points(c_hist$mids, c_hist$density, pch = q_25(c_ind), cex = 0.1, col = color_ind[c_ind])
      
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


perform_quotient_test <- function(diff_alpha, level_alpha, pdf_name) {
  diffs <- stabledist::rstable(100000, diff_alpha, 0., 0.1, 0.)
  levels <- stabledist::rstable(100000, level_alpha, 0.95, 0.15, 0.15)
  df <- data.frame("levels"=levels, "diffs"=diffs)
  df$quotients <- df$diffs / df$levels
  #df$type <- "Artificial"
  
  df <- melt(df) 
  df$variable <- as.character(df$variable)
  
  neg_cut <- 0.005
  pov_cut <- 0.995
  fun_plot_marginal(df=df,
                    cond_name = "variable", 
                    var_name = "value", 
                    pdf_name = pdf_name, 
                    x_lab = "", 
                    title = "", 
                    c_names = unique(df$variable), 
                    neg_cut = neg_cut, 
                    pov_cut = pov_cut, 
                    cut_num = 5000,
                    bin_num = 1000)
  
}

# main entry point
setwd("~/datalake/CIEDB_2009_2013/")

perform_quotient_test(1.05, 1.1, "00_Levy_quotient_test_01")
perform_quotient_test(0.9, 1.1, "00_Levy_quotient_test_02")
perform_quotient_test(1.1, 1.45, "00_Levy_quotient_test_03")

