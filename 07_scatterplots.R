if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(ggplot2)

# plot: alpha vs Observations
#        alpha vs Soofi
#        alpha vs Soofi Diff

plot_by_var_and_sep <- function(df33, var, var_label, sep_var, y_var, color_var="Year", gradient_midpoint=1.05) {
    dfmerge <- df33[df33$Fit_Variable==var,]
    dfmerge <- dfmerge[dfmerge$Separator_Variable==sep_var,]
    dfmerge <- dfmerge[c("Class", "Year", "Levy_alpha", "Levy_Soofi_ID", "Soofi_Difference", "Observations")]
    
    if (nrow(dfmerge)>0) {          # Check for duplicates??

        p <- ggplot(dfmerge, aes_string(x="Levy_alpha" , y=y_var, color=color_var)) + geom_point(size=1) #+ scale_color_gradient(low = "green", high = "red")
        p <- p + theme(axis.line = element_line(color="black", size = 0.5)) + geom_vline(xintercept = 1)
        if (y_var=="Soofi_Difference") {
            p <- p + geom_hline(yintercept = 0)
        }
        p <- p + labs(title = paste(paste(paste(paste(var, "by"), sep_var), "Color:"), color_var))   # title

        ## Save
        ggsave(paste(paste(paste(paste(paste("Scatter", var_label, sep="_"), sep_var, sep="_"), y_var, sep="_"), color_var, sep="_"), "pdf", sep="."))
    }
}

# main entry point

## Prepare data
load("China_fit_results_dataframe.Rda", verbose=T)
#df3 <- df3[df3$Separator_Variable=="Province",]
setwd("./Figures/")

variable_Names <- c("Labor productivity", "Labor productivity growth", "Labor productivity log return", "Labor productivity (imputed) difference", "Labor productivity difference", "TFP", "Return on Capital", "Investment rate")
variable_Labels <- c("def_LP_IO", "def_LP_IO_g", "def_LP_IO_lr", "def_LP_diff", "def_LP_IO_diff", "def_TFP_g", "def_RoC_G_FI", "def_FIAS_g")
separator_Variables <- c("Province", "Sector", "Size", "FirmType", "Year")
y_Variables <- c("Levy_Soofi_ID", "Soofi_Difference", "Observations")
color_Variables <- c("Year", "Class")

for (i in 1:length(variable_Names)) {
    for (j in 1:length(separator_Variables)) {
        for (k in 1:length(y_Variables)) {
            for (h in 1:length(color_Variables)) {
                plot_by_var_and_sep(df3, variable_Names[[i]], variable_Labels[[i]], separator_Variables[[j]], y_Variables[k], color_var=color_Variables[h])
            }
        }
    }
}
