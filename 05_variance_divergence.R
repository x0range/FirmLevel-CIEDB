if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(stabledist,StableEstim,parallel,ggplot2) # load packages


# functions

estimate_sd_by_n <- function(n, data, q) {
    # Function to compute mean and quantile of standard deviation estimates in subsamples of a particular size.
    # Accepts arguments:
    #   n (integer): Subsample size 
    #   data (numeric vector): Full sample
    #   q (double): Quantiles to return
    # Returns:
    #   numeric vetor of length 3: Mean, lower quantile (q), upper quantile (1-q) 
    
    print(paste("Done: ", n))
    
    # Parallel processing standard deviation computation in n_replications samples
    sd_estimates <- mclapply(as.list(1:n_replications),function(x){sd(sample(data,size=n))},mc.cores = 4)   
    sd_estimates <- unlist(sd_estimates)
    
    # Extract mean and quantiles
    sd_quantiles <- quantile(sd_estimates, c(q, 1-q))
    names(sd_quantiles) <- c("Lower", "Upper")
    sd_mean <- mean(sd_estimates)
    names(sd_mean) <- "Mean"
    
    # Return
    return(c(sd_mean, sd_quantiles))
}


# main entry point

# load data
load("~/CIEDB_temp/dataframe_including_FirmType2.Rda", verbose=T)           # provides df
data_series <- df$def_LP
data_series <- data_series[!is.na(data_series)]

# set range
N_range <- round(10**(seq(1,log10(length(data_series)/1.),length.out=30)))  # N range
n_replications <- 10000                                                     # number of replications

#fit parameters
fit <- StableEstim::McCullochParametersEstim(data_series)

# create artificial sample for comparison
artificial_data_series <- do.call(stabledist::rstable, as.list(c(length(data_series), fit, 0)))

# Monte Carlo simulation
# empirical data
result_df <- as.data.frame(t(sapply(N_range, estimate_sd_by_n, data=data_series, q=0.05)))
result_df$N <- N_range
# artificial data
artificial_result_df <- as.data.frame(t(sapply(N_range, estimate_sd_by_n, data=artificial_data_series, q=0.05)))
artificial_result_df$N <- N_range
 
#print(result_df)

# obtain theoretical divergence
sd_theoretical <- N_range**((2-fit[[1]])/(2*fit[[1]]))                      # fit[[1]] is \alpha
print(sd_theoretical)
# renormalize
sd_theoretical <- sd_theoretical * (result_df$Mean[[1]]/sd_theoretical[[1]])
print(sd_theoretical)

# combine df
result_df <- merge(x = result_df, y = artificial_result_df, by = "N")
result_df$sd_theoretical <- sd_theoretical

# save 
save(result_df, file="sd_divergence.Rda")
#load("sd_divergence.Rda", verbose=T)

# plots 
# paper style plots (small text)
plot_labels <- c("Data", "Artificial data", "Theoretical divergence")
ggplot(data=result_df) + 
    geom_line(aes(x=N, y=Mean.x, group=1, linetype=plot_labels[[1]], colour=plot_labels[[1]]), lwd=1) +             # Data mean
    geom_line(aes(x=N, y=Mean.y, group=2, linetype=plot_labels[[2]], colour=plot_labels[[2]]), lwd=1) +             # Artificial data mean
    geom_line(aes(x=N, y=sd_theoretical, group=3, linetype=plot_labels[[3]], colour=plot_labels[[3]]), lwd=1) +     # Theoretical divergence 
    geom_ribbon(aes(x=N, ymin=Lower.x, ymax=Upper.x), fill="#0000BB33") +                                           # Data quantiles
    geom_ribbon(aes(x=N, ymin=Lower.y, ymax=Upper.y), fill="#00BB0033") +                                           # Artificial data quantiles
    theme_bw() +                                                                                                    # bw theme
    theme(axis.text = element_text(colour = 1, size = 12), axis.title.x = element_text(size = 15, vjust=-.2), axis.title.y = element_text(size = 15, vjust=0.3)) + # text style
    theme(legend.title=element_blank(), legend.text=element_text(colour = 1, size = 12), legend.position=c(1, 0),   
                    legend.background = element_rect(linetype = 1, size = 0.5, colour = 1), legend.key.size = unit(0.5, "cm"), legend.key.width = unit(1.8, "cm"), 
                    legend.justification=c(1,0), aspect.ratio=1/1.41, panel.background = element_rect(colour = "black", size=0.5)) +  # legend style
    labs(x="N", y="Standard deviation") + scale_y_log10() + scale_x_log10() +                                       # axes scales and labels
    scale_linetype_manual(name="", values=c("dashed", "solid", "dotdash")) + scale_colour_manual(name="",values=c("green", "blue", "black"))    # line styles and colors
ggsave("sd_divergence_incl_artificial_data.pdf", width = 7.755, height = 5.5)                            

plot_labels <- c("Data", "Theoretical divergence")
ggplot(data=result_df) + 
    geom_line(aes(x=N, y=Mean.x, group=2, linetype=plot_labels[[1]], colour=plot_labels[[1]]), lwd=1) + 
    geom_line(aes(x=N, y=sd_theoretical, group=3, linetype=plot_labels[[2]], colour=plot_labels[[2]]), lwd=1) + 
    geom_ribbon(aes(x=N, ymin=Lower.x, ymax=Upper.x), fill="#0000BB33") +
    theme_bw() + 
    theme(axis.text = element_text(colour = 1, size = 12), axis.title.x = element_text(size = 15, vjust=-.2), axis.title.y = element_text(size = 15, vjust=0.3)) + 
    theme(legend.title=element_blank(), legend.text=element_text(colour = 1, size = 12), legend.position=c(1, 0), 
                    legend.background = element_rect(linetype = 1, size = 0.5, colour = 1), legend.key.size = unit(0.5, "cm"), legend.key.width = unit(1.8, "cm"), 
                    legend.justification=c(1,0), aspect.ratio=1/1.41, panel.background = element_rect(colour = "black", size=0.5)) +
    labs(x="N", y="Standard deviation") + scale_y_log10() + scale_x_log10() +
    scale_linetype_manual(name="", values=c("solid", "dotdash")) + scale_colour_manual(name="",values=c("blue", "black"))
ggsave("sd_divergence.pdf", width = 7.755, height = 5.5)

# presentation style plots (large text)
plot_labels <- c("Data", "Artificial data", "Theoretical divergence")
ggplot(data=result_df) + 
    geom_line(aes(x=N, y=Mean.x, group=1, linetype=plot_labels[[1]], colour=plot_labels[[1]]), lwd=1) + 
    geom_line(aes(x=N, y=Mean.y, group=2, linetype=plot_labels[[2]], colour=plot_labels[[2]]), lwd=1) + 
    geom_line(aes(x=N, y=sd_theoretical, group=3, linetype=plot_labels[[3]], colour=plot_labels[[3]]), lwd=1) + 
    geom_ribbon(aes(x=N, ymin=Lower.x, ymax=Upper.x), fill="#0000BB33") +
    geom_ribbon(aes(x=N, ymin=Lower.y, ymax=Upper.y), fill="#00BB0033") +
    theme_bw() + 
    theme(axis.text = element_text(colour = 1, size = 18), axis.title.x = element_text(size = 22, vjust=-.2), axis.title.y = element_text(size = 22, vjust=0.3)) + 
    theme(legend.title=element_blank(), legend.text=element_text(colour = 1, size = 18), legend.position=c(1, 0), 
                    legend.background = element_rect(linetype = 1, size = 0.5, colour = 1), legend.key.size = unit(0.5, "cm"), legend.key.width = unit(1.8, "cm"), 
                    legend.justification=c(1,0), aspect.ratio=1/1.41, panel.background = element_rect(colour = "black", size=0.5)) +
    labs(x="N", y="Standard deviation") + scale_y_log10() + scale_x_log10() +
    scale_linetype_manual(name="", values=c("dashed", "solid", "dotdash")) + scale_colour_manual(name="",values=c("green", "blue", "black"))
ggsave("sd_divergence_largetext_incl_artificial_data.pdf", width = 7.755, height = 5.5)

plot_labels <- c("Data", "Theoretical divergence")
ggplot(data=result_df) + 
    geom_line(aes(x=N, y=Mean.x, group=2, linetype=plot_labels[[1]], colour=plot_labels[[1]]), lwd=1) + 
    geom_line(aes(x=N, y=sd_theoretical, group=3, linetype=plot_labels[[2]], colour=plot_labels[[2]]), lwd=1) + 
    geom_ribbon(aes(x=N, ymin=Lower.x, ymax=Upper.x), fill="#0000BB33") +
    theme_bw() + 
    theme(axis.text = element_text(colour = 1, size = 18), axis.title.x = element_text(size = 22, vjust=-.2), axis.title.y = element_text(size = 22, vjust=0.3)) + 
    theme(legend.title=element_blank(), legend.text=element_text(colour = 1, size = 18), legend.position=c(1, 0), 
                    legend.background = element_rect(linetype = 1, size = 0.5, colour = 1), legend.key.size = unit(0.5, "cm"), legend.key.width = unit(1.8, "cm"), 
                    legend.justification=c(1,0), aspect.ratio=1/1.41, panel.background = element_rect(colour = "black", size=0.5)) +
    labs(x="N", y="Standard deviation") + scale_y_log10() + scale_x_log10() +
    scale_linetype_manual(name="", values=c("solid", "dotdash")) + scale_colour_manual(name="",values=c("blue", "black"))
ggsave("sd_divergence_largetext.pdf", width = 7.755, height = 5.5)
