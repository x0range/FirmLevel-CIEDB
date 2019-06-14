if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(ggplot2)

plot_by_var <- function(df33, var, var_label, x_var="Levy_alpha", x_var_description="", soofi_ID_cutoff=90, gradient_midpoint=1.05) {
    dfmerge <- df33[df33$Fit_Variable==var,]
    dfmerge$Levy_alpha[dfmerge$Levy_Soofi_ID<soofi_ID_cutoff]<-NA
    dfmerge$Levy_beta[dfmerge$Levy_Soofi_ID<soofi_ID_cutoff]<-NA
    dfmerge$Levy_gamma[dfmerge$Levy_Soofi_ID<soofi_ID_cutoff]<-NA
    dfmerge$Levy_delta[dfmerge$Levy_Soofi_ID<soofi_ID_cutoff]<-NA
    dfmerge <- dfmerge[c("Class", "Year", x_var)]

    if (nrow(dfmerge)>0) {          # Check for duplicates??

        #reshape(dat1, idvar = "Class", timevar = "Year", direction = "wide")
        
        base_size <- 9
        p <- ggplot(dfmerge, aes(x=Year, y=Class)) + geom_tile(aes_string(fill = x_var), colour = "white") + scale_fill_gradient(low = "red", high = "blue")
        p <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_continuous(breaks=c(1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013), labels=c(1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013), expand = c(0, 0)) +
             scale_y_discrete(expand = c(0, 0)) #+
             #scale_x_continuous(expand = c(0, 0)) 
             
             
             #opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"))
        p <- p + labs(title = paste(paste(var, "fit:"), x_var_description))   # title

        ## Save
        ggsave(paste(paste(paste("Heatmap", var_label, sep="_"), x_var, sep="_"), "pdf", sep="."))
    }
}

# main entry point

## Prepare data
load("China_fit_results_dataframe.Rda", verbose=T)
df3 <- df3[df3$Separator_Variable=="Province",]
setwd("./Figures/")

variable_Names <- c("Labor productivity", "Labor productivity growth", "Labor productivity log return", "Labor productivity (imputed) difference", "Labor productivity difference", "TFP", "Return on Capital", "Investment rate", "Labor productivity (imputed)")
variable_Labels <- c("def_LP_IO", "def_LP_IO_g", "def_LP_IO_lr", "def_LP_diff", "def_LP_IO_diff", "def_TFP_g", "def_RoC_G_FI", "def_FIAS_g", "def_LP")
x_vars <- c("Levy_alpha", "Levy_beta", "Levy_gamma", "Levy_delta")
x_var_descriptions <- c("Tail", "Skewness", "Scale", "Location")

for (i in 1:length(variable_Names)) {
    for (k in 1:length(x_vars)) {
        plot_by_var(df3, variable_Names[[i]], variable_Labels[[i]], x_var=x_vars[[k]], x_var_description=x_var_descriptions[[k]])
    }
}
