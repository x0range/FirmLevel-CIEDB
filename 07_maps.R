if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(maptools,rgeos,gpclib,foreign,sp,ggplot2,plyr,dplyr,knitr,rgdal)

plot_by_var_and_year <- function(df33, year, var, var_label, soofi_ID_cutoff=90, gradient_midpoint=1.05) {
    dfmerge <- df33[df33$Fit_Variable==var,]
    dfmerge <- dfmerge[dfmerge$Year==year,]

    if (nrow(dfmerge)>0 && length(dfmerge$id)==length(unique(dfmerge$id))) {

        dfmerge <- dfmerge[c("Class", "Levy_Soofi_ID", "Levy_alpha")]
        dfmerge$Levy_alpha[dfmerge$Levy_Soofi_ID<soofi_ID_cutoff]<-NA
        dfmerge$id <- dfmerge$Class
        dfmerge$Class <- NULL
        
        ChinaLevel1Data2 <- merge(ChinaLevel1Data, dfmerge, by = "id", all=T)
        ChinaLevel1 <- merge(ChinaLevel1dF, ChinaLevel1Data2, by = "id")

        ## Create the ggplot using standard approach
        g <- ggplot(ChinaLevel1, aes(x = long, y = lat, fill = Levy_alpha, group = group))
        g <- g + geom_polygon(color = "white", size = 0.2)      # internal borders
        g <- g + coord_equal()                                  # projection ratio
        g <- g + labs(title = paste(var, as.character(year)))   # title

        ## Add region names to plot
        ## Obtain coordinates for names as regional center
        Level1Centers <- ddply(ChinaLevel1dF, .(id), summarize, clat = mean(lat), clong = mean(long))
        ## Merge into data
        ChinaLevel1Data <- merge(ChinaLevel1Data, Level1Centers, all = TRUE)
        ## Remove Paracel Islands label (we do not have data anyway)
        ChinaLevel1Data$id[ChinaLevel1Data$id=="Paracel Islands"] <- NA
        ## Add region names
        g <- g + geom_text(data = ChinaLevel1Data , aes(x = jitter(clong, amount = 1), y = jitter(clat, amount = 1), 
                        label = id, size = 0.2, group = NULL, fill=NULL), size=3, show_guide = FALSE) 

        ## Theme
        g <- g + theme_bw() + 
                        theme(axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        legend.position = c(0.9, 0.2),
                        legend.text = element_text(size = rel(0.7)),
                        legend.background = element_rect(color = "gray50", size = 0.3, fill = "white"))
        ## Palette
        g <- g + scale_fill_gradient2(midpoint = gradient_midpoint, low = "green", high = "blue", mid="red")
        #print(g)

        ## Save
        ggsave(paste(paste(paste("Map", var_label, sep="_"), as.character(year), sep="_"), "pdf", sep="."), width=7, height=4.5)
    }
}

# main entry point

## Prepare data
load("China_fit_results_dataframe.Rda", verbose=T)
df3 <- df3[df3$Separator_Variable=="Province",]

## Prepare shapefile
# Follows this example https://datapleth.io/2015/10/17/china-base-map/
# Uses this shapefile https://github.com/sinanshi/visotmed/blob/master/data/CHN_adm
# Load shapefile
ChinaPolygonsLevel1 <- readOGR(dsn = ".", layer = "CHN_adm1")
## English names
ChinaPolygonsLevel1@data$NAME_1 <- as.character(ChinaPolygonsLevel1@data$NAME_1)
ChinaPolygonsLevel1@data[grep("Xinjiang Uygur", ChinaPolygonsLevel1@data$NAME_1),]$NAME_1 <- "Xinjiang"
ChinaPolygonsLevel1@data[grep("Nei Mongol", ChinaPolygonsLevel1@data$NAME_1),]$NAME_1 <- "Inner Mongolia"
ChinaPolygonsLevel1@data[grep("Xizang", ChinaPolygonsLevel1@data$NAME_1),]$NAME_1 <- "Tibet Autonomous Region"
ChinaPolygonsLevel1@data[grep("Ningxia Hui", ChinaPolygonsLevel1@data$NAME_1),]$NAME_1 <- "Ningxia"
ChinaPolygonsLevel1@data$NAME_1 <- as.factor(ChinaPolygonsLevel1@data$NAME_1)
# Convert the spacial polygon shapes to data frame
ChinaLevel1Data <- ChinaPolygonsLevel1@data
ChinaLevel1Data$id <- ChinaLevel1Data$NAME_1
# Fortify the data (polygon map as dataframe)
ChinaLevel1dF <- fortify(ChinaPolygonsLevel1, region = "NAME_1")

setwd("./Figures/")

variable_Names <- c("Labor productivity", "Labor productivity growth", "Labor productivity log return", "Labor productivity (imputed) difference", "Labor productivity difference", "TFP", "Return on Capital", "Investment rate")
variable_Labels <- c("def_LP_IO", "def_LP_IO_g", "def_LP_IO_lr", "def_LP_diff", "def_LP_IO_diff", "def_TFP_g", "def_RoC_G_FI", "def_FIAS_g")

for (i in 1:length(variable_Names)) {
    for (year in 1998:2013) {
        plot_by_var_and_year(df3, year, variable_Names[[i]], variable_Labels[[i]])
    }
}
