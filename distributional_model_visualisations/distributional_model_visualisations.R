if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr, ggplot2, StableEstim, lmomco, cowplot)

create_data_series_AEP <- function(i, j, plot_values) {
    reverse_order_idx <- 1+4-j  #j=1 -> rio=4, j=2 -> rio=3, j=3 -> rio=2, j=4 -> rio=1, 
    AEP_param <- list(para=c(xi, sigma, heta, kappa), type="aep4")
    AEP_param$para[[reverse_order_idx]] <- plot_values[[i]]
    dat_next <- pdfaep4(xs, AEP_param)
    return(dat_next)
}

create_data_series_Levy <- function(i, j, plot_values) {
    Levy_param <- list(xi, sigma, heta, kappa)
    Levy_param[[j]] <- plot_values[[i]]
    arg.list <- append(Levy_param, list(xs), 0)
    dat_next <- do.call(dstable, arg.list)
    return(dat_next)
}

create_plot <- function(j, disttype="Levy") {
    plot_labels <- all_plot_labels[[j]]
    
    plot_next <- ggplot() + labs(x="x", y="p(x)") + scale_y_log10(limits=c(0.005, 1.))

    dat_list <- list()
    for (i in 1:3) {
        if (disttype == "AEP") {
            dat_list[[i]] <- create_data_series_AEP(i, j, all_plot_values[[j]])
        } else if (disttype == "Levy") {
            dat_list[[i]] <- create_data_series_Levy(i, j, all_plot_values[[j]])
        } else {
            quit(-99)
        }
    }
    plot_next <- plot_next + geom_line(aes(xs, dat_list[[1]], group=1, linetype=plot_labels[[1]], colour=plot_labels[[1]]),size=1.0)
    plot_next <- plot_next + geom_line(aes(xs, dat_list[[2]], group=2, linetype=plot_labels[[2]], colour=plot_labels[[2]]),size=1.0)
    plot_next <- plot_next + geom_line(aes(xs, dat_list[[3]], group=3, linetype=plot_labels[[3]], colour=plot_labels[[3]]),size=1.0)

    plot_next <- plot_next + scale_linetype_manual(name="", values=c("solid","dashed", "dotdash")) + scale_colour_manual(name="",values=plot_colors) + theme_bw() + theme(axis.text = element_text(colour = 1, size = 12), axis.title.x = element_text(size = 15, vjust=-.2), axis.title.y = element_text(size = 15, vjust=0.3)) + theme(legend.title=element_blank(), legend.text=element_text(colour = 1, size = 12), legend.position=c(0, 1), legend.background = element_rect(linetype = 1, size = 0.5, colour = 1), legend.key.size = unit(0.5, "cm"), legend.key.width = unit(1.8, "cm"), legend.justification=c(0,1), aspect.ratio=1/1.41, panel.background = element_rect(colour = "black", size=0.5)) + annotate(geom = 'text', label = plot_paramtext[[j]], x=Inf, y=Inf, hjust = 1.2, vjust = 2)
    
    return(plot_next)
}


pdfOutputFileName <- "AEP_variation.pdf"

xi <- 0
sigma <- 1
heta <- 0.6
kappa <- 0.5
xs <- seq(-10,10,length.out=300)
ys <- seq(0.001, 2, length.out=length(xs))

xi_plot_labels <- c("xi=-3", "xi=0", "xi=3")
sigma_plot_labels <- c("sigma=0.5", "sigma=1.0", "sigma=2.0")
heta_plot_labels <- c("h=0.6", "h=1.0", "h=1.7")
kappa_plot_labels <- c("kappa=0.5", "kappa=1.0", "kappa=2.0")

xi_plot_values <- c(-3, 0, 3)
sigma_plot_values <- c(0.5, 1, 2)
heta_plot_values <- c(0.6, 1, 1.7)
kappa_plot_values <- c(0.5, 1, 2)

all_plot_labels <- list(kappa_plot_labels, heta_plot_labels, sigma_plot_labels, xi_plot_labels)
all_plot_values <- list(kappa_plot_values, heta_plot_values, sigma_plot_values, xi_plot_values)
plot_paramtext <- list("xi=0, sigma=1, h=0.6", "xi=0, sigma=1, kappa=0.5", "xi=0, h=0.6, kappa=0.5", "sigma=1, h=0.6, kappa=0.5")

plot_ltys <- c(1, 2, 4)
plot_colors <- c("royalblue1", "blue3", "black")

plot_list <- list()
for (j in 1:4) {
    plot_list[[j]] <- create_plot(j, "AEP")
}

combined_figure <- plot_grid(plotlist=plot_list, align = "h", ncol = 2)
save_plot(pdfOutputFileName, combined_figure, ncol = 2, nrow = 2, base_aspect_ratio = 1.41)
dev.off()


pdfOutputFileName <- "Levy_variation.pdf"

xi <- 1.1
sigma <- 0.5
heta <- 1
kappa <- 0
xs <- seq(-10,10,length.out=300)
ys <- seq(0.001, 2, length.out=length(xs))

xi_plot_labels <- c("alpha=0.4", "alpha=1.1", "alpha=2.0")
sigma_plot_labels <- c("beta=-0.5", "beta=0.0", "beta=0.5")
heta_plot_labels <- c("gamma=0.5", "gamma=1.0", "gamma=2.0")
kappa_plot_labels <- c("delta=-3", "delta=0", "delta=3")

xi_plot_values <- c(0.4, 1.1, 2.0)
sigma_plot_values <- c(-0.5, 0, 0.5)
heta_plot_values <- c(0.5, 1, 2)
kappa_plot_values <- c(-3, 0, 3)

all_plot_labels <- list(xi_plot_labels, sigma_plot_labels, heta_plot_labels, kappa_plot_labels)
all_plot_values <- list(xi_plot_values, sigma_plot_values, heta_plot_values, kappa_plot_values)
plot_paramtext <- list("beta=0.5, gamma=1, delta=0", "alpha=1.1, gamma=1, delta=0", "alpha=1.1, beta=0.5, delta=0", "alpha=1.1, beta=0.5, gamma=1")

plot_ltys <- c(1, 2, 4)
plot_colors <- c("royalblue1", "blue3", "black")

plot_list <- list()
for (j in 1:4) {
    plot_list[[j]] <- create_plot(j, "Levy")
}

combined_figure <- plot_grid(plotlist=plot_list, align = "h", ncol = 2)
save_plot(pdfOutputFileName, combined_figure, ncol = 2, nrow = 2, base_aspect_ratio = 1.41)
dev.off()

quit(-1)
