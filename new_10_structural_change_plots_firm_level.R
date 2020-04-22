library(pacman)
pacman::p_load(dplyr, tidyr, ggplot2, reshape2, ggrepel, zoo)


make_plot_df <- function(idf, operation="cumprod", firstyear=1998, remove_ambiguous_sectors40=T) {
  # remove empty column name
  colnames(idf)[colnames(idf)==""] <- "Other"
  if (operation=="cumprod") {
    idf <- idf + 1
    idf[idf>=2] <- NA
    idf[idf<=0.5] <- NA
  } else if (operation=="cumsum") {
    idf[idf>=0.5] <- NA
    idf[idf<=-0.5] <- NA
  } else {
    message(paste("Error: Operation unknown: ", operation))
    return(NA)
  }
  firstrow <- data.frame(t(replicate(ncol(idf), 1.0)))
  rownames(firstrow) <- c(firstyear)
  colnames(firstrow) <- colnames(idf)
  idf <- rbind(firstrow, idf)
  if (remove_ambiguous_sectors40) {
    for (i in 38:43) {
      coln <- as.character(i)
      idf[coln] <- NULL
    }
  }
  rdf <- apply(idf, 2, get(operation))
  return(rdf)
}

plot_time_dev <- function(df_a, df_b, var1, var2, plot_file_name="plot.pdf", columns_restriction=c(), show_legend=F, xlim=NA, ylim=NA, hiddenpoint=c(1,1),
                          legend_name="Sectors", legend_labels=NA, legend_horizontal_position=0.95) {
  columns_a <- colnames (df_a)
  columns_b <- colnames (df_b)
  plot_columns <- intersect(columns_a, columns_b)
  
  if (length(columns_restriction)>0) {
    plot_columns <- intersect(plot_columns, columns_restriction)
  }
  
  if (is.na(legend_labels)) {
    legend_labels=plot_columns
  }
  
  the_colors <- c("#e6194b", "#3cb44b", "#ffe119", "#0082c8", "#f58231", "#911eb4", "#46f0f0", "#f032e6", 
                  "#d2f53c", "#fabebe", "#008080", "#e6beff", "#aa6e28", "#fffac8", "#800000", "#aaffc3", 
                  "#808000", "#ffd8b1", "#000080", "#808080", "#000000", "#000000", "#000000", "#000000", 
                  "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", 
                  "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", 
                  "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", 
                  "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000")
  
  the_colors <- sort(the_colors[1:length(plot_columns)])
  
  color_df <- data.frame(Sectors=the_colors, the_labels=legend_labels)
  
  the_plot <- ggplot() + 
    labs(x=var1, y=var2) +
    theme_bw() +
    theme(axis.text = element_text(size=12), axis.title=element_text(size=14), legend.position = c(legend_horizontal_position, 0.5),
          legend.title=element_text(size=14), legend.text=element_text(size=12))
  
  if (show_legend) {
    the_plot <- the_plot + 
      geom_point(data = color_df, aes(x = c(hiddenpoint[[1]]), y = c(hiddenpoint[[2]]), color = Sectors)) + 
      scale_color_identity(name=legend_name, guide = 'legend', labels = color_df$the_labels)
  }
  
  #print(df_a)
  #print(df_b)
  for (i in 1:length(plot_columns)) {
    plot_data <- data.frame(cbind(df_a[,plot_columns[[i]]], df_b[,plot_columns[[i]]]))
    colnames(plot_data) <- c(var1, var2)
    plot_data$Year = rownames(df_a)         # would be the same as rownames(df_b)
    #print(plot_data)
    the_plot <- eval(the_plot +
                       geom_point(data=plot_data, aes(x=get(var1), y=get(var2)), color=the_colors[[i]]) +
                       geom_text_repel(data=plot_data, aes(x=get(var1), y=get(var2), label=Year)) +
                       geom_segment(data=plot_data, aes(x=get(var1),
                                                        y=get(var2),
                                                        xend=c(tail(get(var1), n=-1), NA), 
                                                        yend=c(tail(get(var2), n=-1), NA), 
                       ), color=the_colors[[i]]))
    
    
  }
  if (!is.na(xlim)) {
    the_plot <- the_plot +
      xlim(xlim[[1]], xlim[[2]])
  }
  if (!is.na(ylim)) {
    the_plot <- the_plot +
      ylim(ylim[[1]], ylim[[2]])
  }
  print(the_plot)
  ggsave(plot_file_name, plot=the_plot)
}


# main entry point

setwd("~/datalake/CIEDB_2009_2013/")

# relevant sector lists
big_ISIC_CIEDB = c("B", "C10-C12", "C13-C15", "C20", "C22", "C23", "C24", "C25", "C28", "C29", "C30", "D35")
big_ISIC_MACRO = c("A01", "B", "C10-C12", "C13-C15", "C20", "C24", "C26", "C28", "D35", "F", "G46", "H49", "K64", "L68", "O84", "P85")
big_GB2002 = c("06", "13", "17", "18", "19", "26", "30", "31", "32", "34", "35", "36", "37", "44")   # biggest in 2004 w/o 40, 41
big_Provinces = c("31", "21", "51", "25", "10", "20", "35", "45", "61", "05", "43", "41", "23", "30", "40", "15")   # biggest in 2004 w/o 40, 41
big_Province_labels = c("Hebei", "Beijing", "Heilongjiang", "Shanghai", "Jiangsu", "Anhui", "Shandong", "Tianjin", 
                        "Zhejiang", "Fujian", "Chongqing", "Hunan", "Hubei", "Henan", "Guangdong", "Sichuan")



# Part 1 micro data GB 2002 sectors

#load("09_data_panels_list.Rda", verbose=T) # consistent_panels, consistent_panel_years, consistent_panel_sizes, sectoral_panels, sectoral_growth
load("09_sectoral_change.Rda", verbose=T) # df_E_g, df_VA_g, df_LP_diff, df_LP_IO_diff, df_FA_g, df_E_s, df_VA_s

plot_df_E_g <- make_plot_df(df_E_g)
plot_df_VA_g <- make_plot_df(df_VA_g)
plot_df_LP_diff <- make_plot_df(df_LP_diff, "cumsum")
plot_df_LP_IO_diff <- make_plot_df(df_LP_IO_diff, "cumsum")
plot_df_FA_g <- make_plot_df(df_FA_g, "cumsum")

save(plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g, file = "10_sectoral_structural_change_plot_data.Rda")
load("10_sectoral_structural_change_plot_data.Rda", verbose=T) # plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g

plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_VA_g, var1="Employment share (1998=1.0)", var2="Value added share (1998=1.0)", 
              plot_file_name="Development_Employment_VA.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_diff, var1="Employment share (1998=1.0)", var2="Labour productivity (1998=1.0)", 
              plot_file_name="Development_Employment_LP.pdf")
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_df_LP_diff, var1="Value added share (1998=1.0)", var2="Labour prodictivity (1998=1.0)", 
              plot_file_name="Development_VA_LP.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_IO_diff, var1="Employment share (1998=1.0)", var2="Output per worker (1998=1.0)", 
              plot_file_name="Development_Employment_LP_IO.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_FA_g, var1="Employment share (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_Employment_Age.pdf")
plot_time_dev(df_a=plot_df_LP_diff, df_b=plot_df_FA_g, var1="Labour productivity (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_LP_Age.pdf")


plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_VA_g, var1="Employment share (1998=1.0)", var2="Value added share (1998=1.0)", 
              plot_file_name="Development_BG_Employment_VA.pdf", columns_restriction=big_GB2002, show_legend=T)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_diff, var1="Employment share (1998=1.0)", var2="Labour productivity (1998=1.0)", 
              plot_file_name="Development_BG_Employment_LP.pdf", columns_restriction=big_GB2002, show_legend=T)
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_df_LP_diff, var1="Value added share (1998=1.0)", var2="Labour prodictivity (1998=1.0)", 
              plot_file_name="Development_BG_VA_LP.pdf", columns_restriction=big_GB2002, show_legend=T)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_IO_diff, var1="Employment share (1998=1.0)", var2="Output per worker (1998=1.0)", 
              plot_file_name="Development_BG_Employment_LP_IO.pdf", columns_restriction=big_GB2002, show_legend=T)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_FA_g, var1="Employment share (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_BG_Employment_Age.pdf", columns_restriction=big_GB2002, show_legend=T)
plot_time_dev(df_a=plot_df_LP_diff, df_b=plot_df_FA_g, var1="Labour productivity (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_BG_LP_Age.pdf", columns_restriction=big_GB2002, show_legend=T)


# Part 2 micro data ISIC R4 sectors

#load("09_data_panels_list.Rda", verbose=T) # consistent_panels, consistent_panel_years, consistent_panel_sizes, sectoral_panels, sectoral_growth
load("09_ISICR4_sectoral_change.Rda", verbose=T) # df_E_g, df_VA_g, df_LP_diff, df_LP_IO_diff, df_FA_g, df_E_s, df_VA_s

plot_df_E_g <- make_plot_df(df_E_g)
plot_df_VA_g <- make_plot_df(df_VA_g)
plot_df_LP_diff <- make_plot_df(df_LP_diff, "cumsum")
plot_df_LP_IO_diff <- make_plot_df(df_LP_IO_diff, "cumsum")
plot_df_FA_g <- make_plot_df(df_FA_g, "cumsum")

save(plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g, file = "10_ISICR4_sectoral_structural_change_plot_data.Rda")
load("10_ISICR4_sectoral_structural_change_plot_data.Rda", verbose=T) # plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g

plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_VA_g, var1="Employment share (1998=1.0)", var2="Value added share (1998=1.0)", 
              plot_file_name="Development_ISICR4_Employment_VA.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_diff, var1="Employment share (1998=1.0)", var2="Labour productivity (1998=1.0)", 
              plot_file_name="Development_ISICR4_Employment_LP.pdf")
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_df_LP_diff, var1="Value added share (1998=1.0)", var2="Labour prodictivity (1998=1.0)", 
              plot_file_name="Development_ISICR4_VA_LP.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_IO_diff, var1="Employment share (1998=1.0)", var2="Output per worker (1998=1.0)", 
              plot_file_name="Development_ISICR4_Employment_LP_IO.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_FA_g, var1="Employment share (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_ISICR4_Employment_Age.pdf")
plot_time_dev(df_a=plot_df_LP_diff, df_b=plot_df_FA_g, var1="Labour productivity (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_ISICR4_LP_Age.pdf")


plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_VA_g, var1="Employment share (1998=1.0)", var2="Value added share (1998=1.0)", 
              plot_file_name="Development_ISICR4_BG_Employment_VA.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_diff, var1="Employment share (1998=1.0)", var2="Labour productivity (1998=1.0)", 
              plot_file_name="Development_ISICR4_BG_Employment_LP.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_df_LP_diff, var1="Value added share (1998=1.0)", var2="Labour prodictivity (1998=1.0)", 
              plot_file_name="Development_ISICR4_BG_VA_LP.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_IO_diff, var1="Employment share (1998=1.0)", var2="Output per worker (1998=1.0)", 
              plot_file_name="Development_ISICR4_BG_Employment_LP_IO.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_FA_g, var1="Employment share (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_ISICR4_BG_Employment_Age.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_LP_diff, df_b=plot_df_FA_g, var1="Labour productivity (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_ISICR4_BG_LP_Age.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)


df_E_g_ma <- zoo::rollapply(df_E_g, width=3, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=FALSE, fill=NA, align="center")
df_VA_g_ma <- zoo::rollapply(df_VA_g, width=3, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=FALSE, fill=NA, align="center")
rownames(df_E_g_ma) <- rownames(df_E_g)
rownames(df_VA_g_ma) <- rownames(df_VA_g)

# growth rates directly
plot_time_dev(df_a=df_E_g_ma, df_b=df_VA_g_ma, var1="Employment share growth", var2="Value added share growth", 
              plot_file_name="Development_ISICR4_BG_Employment_VA_Growth.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93,
              xlim=c(-0.15, 0.15), ylim=c(-0.1, 0.2))



# Part 3 macro data ISIC R4 sectors

# Second data series (macro)
load("08_macro_sectoral_change.Rda", verbose=T)  # MACRO_VA_share_wide, MACRO_E_share_wide, MACRO_VA_share_g_wide, MACRO_E_share_g_wide, MACRO_LP_wide

plot_MACRO_E_g <- make_plot_df(MACRO_VA_share_g_wide, firstyear=2000)
plot_MACRO_VA_g <- make_plot_df(MACRO_E_share_g_wide, firstyear=2000)
#plot_MACRO_LP_diff <- make_plot_df(MACRO_LP_diff_wide, "cumsum", firstyear=2000)

save(plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g, plot_MACRO_E_g, plot_MACRO_VA_g, file = "10_sectoral_structural_change_plot_data.Rda")
load("10_sectoral_structural_change_plot_data.Rda", verbose=T) # plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g, plot_MACRO_E_g, plot_MACRO_VA_g

plot_time_dev(df_a=MACRO_E_share_wide, df_b=MACRO_VA_share_wide, var1="Employment share", var2="Value added share", 
              plot_file_name="Development_MACRO_Employment_VA_absolute_shares.pdf")
plot_time_dev(df_a=plot_MACRO_E_g, df_b=plot_MACRO_VA_g, var1="Employment share (2000=1.0)", var2="Value added share (2000=1.0)", 
              plot_file_name="Development_MACRO_Employment_VA.pdf")
plot_time_dev(df_a=MACRO_E_share_wide, df_b=MACRO_LP_wide, var1="Employment share", var2="Labour productivity", 
              plot_file_name="Development_MACRO_Employment_LP_absolute_shares.pdf")
plot_time_dev(df_a=MACRO_VA_share_wide, df_b=MACRO_LP_wide, var1="Value added share", var2="Labour prodictivity", 
              plot_file_name="Development_MACRO_VA_LP_absolute_shares.pdf")

plot_time_dev(df_a=MACRO_E_share_wide, df_b=MACRO_VA_share_wide, var1="Employment share", var2="Value added share", 
              plot_file_name="Development_BG_MACRO_Employment_VA_absolute_shares.pdf", columns_restriction=big_ISIC_MACRO, 
              show_legend=T, xlim=c(0, 0.1), ylim=c(0, 0.1))
plot_time_dev(df_a=plot_MACRO_E_g, df_b=plot_MACRO_VA_g, var1="Employment share (2000=1.0)", var2="Value added share (2000=1.0)", 
              plot_file_name="Development_BG_MACRO_Employment_VA.pdf", columns_restriction=big_ISIC_MACRO, show_legend=T)
plot_time_dev(df_a=MACRO_E_share_wide, df_b=MACRO_LP_wide, var1="Employment share", var2="Labour productivity", 
              plot_file_name="Development_BG_MACRO_Employment_LP_absolute_shares.pdf", columns_restriction=big_ISIC_MACRO, 
              show_legend=T, xlim=c(0, 0.1)#, ylim=c(0, 0.2)
              )
plot_time_dev(df_a=MACRO_VA_share_wide, df_b=MACRO_LP_wide, var1="Value added share", var2="Labour prodictivity", 
              plot_file_name="Development_BG_MACRO_VA_LP_absolute_shares.pdf", columns_restriction=big_ISIC_MACRO, 
              show_legend=T, xlim=c(0, 0.1))

plot_time_dev(df_a=MACRO_E_share_wide, df_b=MACRO_VA_share_wide, var1="Employment share", var2="Value added share", 
              plot_file_name="Development_IND_MACRO_Employment_VA_absolute_shares.pdf", columns_restriction=big_ISIC_CIEDB, 
              show_legend=T, xlim=c(0, 0.05), ylim=c(0, 0.07), legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_MACRO_E_g, df_b=plot_MACRO_VA_g, var1="Employment share (2000=1.0)", var2="Value added share (2000=1.0)", 
              plot_file_name="Development_IND_MACRO_Employment_VA.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=MACRO_E_share_wide, df_b=MACRO_LP_wide, var1="Employment share", var2="Labour productivity", 
              plot_file_name="Development_IND_MACRO_Employment_LP_absolute_shares.pdf", columns_restriction=big_ISIC_CIEDB, 
              show_legend=T, legend_horizontal_position=0.93, xlim=c(0, 0.05)#, ylim=c(0, 0.2)
)
plot_time_dev(df_a=MACRO_VA_share_wide, df_b=MACRO_LP_wide, var1="Value added share", var2="Labour prodictivity", 
              plot_file_name="Development_IND_MACRO_VA_LP_absolute_shares.pdf", columns_restriction=big_ISIC_CIEDB, 
              show_legend=T, legend_horizontal_position=0.93, xlim=c(0, 0.06))


# Growth rates directly
MACRO_E_share_g_ma <- zoo::rollapply(MACRO_E_share_g_wide, width=5, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=FALSE, fill=NA, align="center")
MACRO_VA_share_g_ma <- zoo::rollapply(MACRO_VA_share_g_wide, width=5, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=FALSE, fill=NA, align="center")
rownames(MACRO_E_share_g_ma) <- rownames(MACRO_E_share_g_wide)
rownames(MACRO_VA_share_g_ma) <- rownames(MACRO_VA_share_g_wide)

plot_time_dev(df_a=MACRO_E_share_g_ma, df_b=MACRO_VA_share_g_ma, var1="Employment share growth", var2="Value added share growth", 
              plot_file_name="Development_IND_MACRO_Employment_VA_Growth.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93,
              xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2))
plot_time_dev(df_a=MACRO_E_share_g_ma, df_b=MACRO_VA_share_g_ma, var1="Employment share growth", var2="Value added share growth", 
              plot_file_name="Development_BG_MACRO_Employment_VA_Growth.pdf", columns_restriction=big_ISIC_MACRO, show_legend=T, legend_horizontal_position=0.93,
              xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2))


# Part 4 concistency check plots
# CIEDB ISIC R4 sectoral accounts are still in df_E_s, df_VA_s, plot_df_E_g, plot_df_VA_g, plot_df_LP_diff

plot_time_dev(df_a=df_E_s, df_b=MACRO_E_share_wide, var1="Employment share CIEDB", var2="Employment share WIOD", 
              plot_file_name="Development__consistency_check_Employment_absolute_shares.pdf", columns_restriction=big_ISIC_CIEDB, 
              show_legend=T, hiddenpoint=c(df_E_s[1,"B"], MACRO_E_share_wide[1,"B"]), legend_horizontal_position=0.93)
plot_time_dev(df_a=df_VA_s, df_b=MACRO_VA_share_wide, var1="Value added share CIEDB", var2="Value added share WIOD", 
              plot_file_name="Development__consistency_check_VA_absolute_shares.pdf", columns_restriction=big_ISIC_CIEDB, 
              show_legend=T, hiddenpoint=c(df_VA_s[1,"B"], MACRO_VA_share_wide[1,"B"]), legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_MACRO_E_g, var1="Employment share CIEDB (1998=1.0)", var2="Employment share WIOD (2000=1.0)", 
              plot_file_name="Development__consistency_check_Employment.pdf")#, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_MACRO_VA_g, var1="Value added share CIEDB (1998=1.0)", var2="Value added share WIOD (2000=1.0)", 
              plot_file_name="Development__consistency_check_VA.pdf")#, legend_horizontal_position=0.93)
plot_time_dev(df_a=df_LP_diff, df_b=MACRO_LP_wide, var1="Labor productivity CIEDB (1998=1.0)", var2="Labor productivity WIOD", 
              plot_file_name="Development__consistency_check_VA.pdf", columns_restriction=big_ISIC_CIEDB, 
              show_legend=T, hiddenpoint=c(df_LP_diff[1,"B"], MACRO_LP_wide[1,"B"]), legend_horizontal_position=0.93)





# Part 5 micro data Zhejiang Province only ISIC R4 sectors
plot_df_VA_g_national <- plot_df_VA_g
plot_df_E_g_national <- plot_df_E_g

#load("09_data_panels_list.Rda", verbose=T) # consistent_panels, consistent_panel_years, consistent_panel_sizes, sectoral_panels, sectoral_growth
load("09_largestProvince_ISICR4_sectoral_change.Rda", verbose=T) # df_E_g, df_VA_g, df_LP_diff, df_LP_IO_diff, df_FA_g, df_E_s, df_VA_s

plot_df_E_g <- make_plot_df(df_E_g)
plot_df_VA_g <- make_plot_df(df_VA_g)
plot_df_LP_diff <- make_plot_df(df_LP_diff, "cumsum")
plot_df_LP_IO_diff <- make_plot_df(df_LP_IO_diff, "cumsum")
plot_df_FA_g <- make_plot_df(df_FA_g, "cumsum")

save(plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g, file = "10_largestProvince_sectoral_structural_change_plot_data.Rda")
load("10_largestProvince_sectoral_structural_change_plot_data.Rda", verbose=T) # plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g

plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_VA_g, var1="Employment share (1998=1.0)", var2="Value added share (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_Employment_VA.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_diff, var1="Employment share (1998=1.0)", var2="Labour productivity (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_Employment_LP.pdf")
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_df_LP_diff, var1="Value added share (1998=1.0)", var2="Labour prodictivity (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_VA_LP.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_IO_diff, var1="Employment share (1998=1.0)", var2="Output per worker (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_Employment_LP_IO.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_FA_g, var1="Employment share (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_Employment_Age.pdf")
plot_time_dev(df_a=plot_df_LP_diff, df_b=plot_df_FA_g, var1="Labour productivity (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_LP_Age.pdf")


plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_VA_g, var1="Employment share (1998=1.0)", var2="Value added share (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_BG_Employment_VA.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_diff, var1="Employment share (1998=1.0)", var2="Labour productivity (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_BG_Employment_LP.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_df_LP_diff, var1="Value added share (1998=1.0)", var2="Labour prodictivity (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_BG_VA_LP.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_IO_diff, var1="Employment share (1998=1.0)", var2="Output per worker (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_BG_Employment_LP_IO.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_FA_g, var1="Employment share (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_BG_Employment_Age.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_LP_diff, df_b=plot_df_FA_g, var1="Labour productivity (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_BG_LP_Age.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)


# Zhejiang vs national
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_E_g_national, var1="Employment share Zhejiang (1998=1.0)", var2="Employment share national (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_BG_Employment.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_df_VA_g_national, var1="Value added share Zhejiang (1998=1.0)", var2="Value added share national (1998=1.0)", 
              plot_file_name="Development_ZhejiangOnly_ISICR4_BG_VA.pdf", columns_restriction=big_ISIC_CIEDB, show_legend=T, legend_horizontal_position=0.93)



# Part 6 micro data by Provinces, not sectors

#load("09_data_panels_list.Rda", verbose=T) # consistent_panels, consistent_panel_years, consistent_panel_sizes, sectoral_panels, sectoral_growth
load("09_Province_sectoral_change.Rda", verbose=T) # df_E_g, df_VA_g, df_LP_diff, df_LP_IO_diff, df_FA_g, df_E_s, df_VA_s

plot_df_E_g <- make_plot_df(df_E_g, remove_ambiguous_sectors40=F)
plot_df_VA_g <- make_plot_df(df_VA_g, remove_ambiguous_sectors40=F)
plot_df_LP_diff <- make_plot_df(df_LP_diff, "cumsum", remove_ambiguous_sectors40=F)
plot_df_LP_IO_diff <- make_plot_df(df_LP_IO_diff, "cumsum", remove_ambiguous_sectors40=F)
plot_df_FA_g <- make_plot_df(df_FA_g, "cumsum", remove_ambiguous_sectors40=F)

save(plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g, file = "10_Provinces_sectoral_structural_change_plot_data.Rda")
load("10_Provinces_sectoral_structural_change_plot_data.Rda", verbose=T) # plot_df_E_g, plot_df_VA_g, plot_df_LP_diff, plot_df_LP_IO_diff, plot_df_FA_g

plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_VA_g, var1="Employment share (1998=1.0)", var2="Value added share (1998=1.0)", 
              plot_file_name="Development_Provinces_Employment_VA.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_diff, var1="Employment share (1998=1.0)", var2="Labour productivity (1998=1.0)", 
              plot_file_name="Development_Provinces_Employment_LP.pdf")
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_df_LP_diff, var1="Value added share (1998=1.0)", var2="Labour prodictivity (1998=1.0)", 
              plot_file_name="Development_Provinces_VA_LP.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_IO_diff, var1="Employment share (1998=1.0)", var2="Output per worker (1998=1.0)", 
              plot_file_name="Development_Provinces_Employment_LP_IO.pdf")
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_FA_g, var1="Employment share (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_Provinces_Employment_Age.pdf")
plot_time_dev(df_a=plot_df_LP_diff, df_b=plot_df_FA_g, var1="Labour productivity (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_Provinces_LP_Age.pdf")



plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_VA_g, var1="Employment share (1998=1.0)", var2="Value added share (1998=1.0)", 
              plot_file_name="Development_Provinces_BG_Employment_VA.pdf", columns_restriction=big_Provinces, show_legend=T,
              legend_name="Provinces", legend_labels=big_Province_labels, legend_horizontal_position=0.9)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_diff, var1="Employment share (1998=1.0)", var2="Labour productivity (1998=1.0)", 
              plot_file_name="Development_Provinces_BG_Employment_LP.pdf", columns_restriction=big_Provinces, show_legend=T,
              legend_name="Provinces", legend_labels=big_Province_labels, legend_horizontal_position=0.9)
plot_time_dev(df_a=plot_df_VA_g, df_b=plot_df_LP_diff, var1="Value added share (1998=1.0)", var2="Labour prodictivity (1998=1.0)", 
              plot_file_name="Development_Provinces_BG_VA_LP.pdf", columns_restriction=big_Provinces, show_legend=T,
              legend_name="Provinces", legend_labels=big_Province_labels, legend_horizontal_position=0.9)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_LP_IO_diff, var1="Employment share (1998=1.0)", var2="Output per worker (1998=1.0)", 
              plot_file_name="Development_Provinces_BG_Employment_LP_IO.pdf", columns_restriction=big_Provinces, show_legend=T,
              legend_name="Provinces", legend_labels=big_Province_labels, legend_horizontal_position=0.9)
plot_time_dev(df_a=plot_df_E_g, df_b=plot_df_FA_g, var1="Employment share (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_Provinces_BG_Employment_Age.pdf", columns_restriction=big_Provinces, show_legend=T,
              legend_name="Provinces", legend_labels=big_Province_labels, legend_horizontal_position=0.9)
plot_time_dev(df_a=plot_df_LP_diff, df_b=plot_df_FA_g, var1="Labour productivity (1998=1.0)", var2="Avg. firm age (1998=1.0)", 
              plot_file_name="Development_Provinces_BG_LP_Age.pdf", columns_restriction=big_Provinces, show_legend=T,
              legend_name="Provinces", legend_labels=big_Province_labels, legend_horizontal_position=0.9)

