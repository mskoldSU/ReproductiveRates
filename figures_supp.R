library(cowplot)
source("load_data.R")

# Fig width and height
w <- 7
h <- 7

# Locations for MCMC output

waterbuck.mcmc <- "mcmc/Waterbuck.Rdata"
giraffe.mcmc <- "mcmc/Giraffe.Rdata"
zebra.mcmc <- "mcmc/Zebra.Rdata"
kudu.mcmc <- "mcmc/Kudu.Rdata"
wildebeest.mcmc <- "mcmc/Blue Wildebeest.Rdata"
sable.mcmc <- "mcmc/Sable.Rdata"
impala.mcmc <- "mcmc/Impala.Rdata"

##
## Parameter posterior plots
##

plot.pars <- function(mcmc.file, species, pars = c("b0", "b1", "b2", "s", "sigma"), 
                      labels = c("beta[0]", "beta[1]", "beta[2]", "s", "sigma")){
    # A gridded plot of parameter posterior distributions
    #
    # args:
    #   mcmc.file:  file location of MCMC output (an .Rdata-file containing
    #       two rjags mcarray objects named out_exp for exponential model
    #       and out_log for logistic model)
    #   species: character string of species name for main title
    #   pars: names of parameters to be plotted (as in mcarray)
    #   labels: names of parameters for plot titles
    #
    load(mcmc.file)
    plotlist <- list()
    N <- length(out_exp[[pars[1]]])
    for (i in pars){
        plotlist[[i]] <- data.frame(value = c(as.matrix(out_exp[[i]]), 
                                              as.matrix(out_log[[i]])),
                                    model = c(rep("exponential", N), 
                                              rep("logistic", N))) %>% 
            ggplot(aes(color=model, x=value)) +
            geom_line(stat="density", size = 1) +
            xlab(parse(text = labels[which(pars ==i)])) +
            ylab("") + 
            theme(plot.margin = unit(c(6, 0, 6, 0), "pt"),
                  legend.position = "none")+
            panel_border()
    }
    dummy <- data.frame(x = c(1, 1), model = c("exponential", "logistic"))
    legend <- get_legend(ggplot(dummy, aes(x = x, color = model)) + 
                             geom_line(stat="density", size = 1))
    p <- plot_grid(plotlist = plotlist, legend)
    title <- ggdraw() + 
        draw_label(paste(species, "posterior densities"), fontface='bold')
    plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
}

#
# Save parameter posterior density figs
#

ggsave("Figs_supp/waterbuck_par_fig.pdf", 
       plot.pars(waterbuck.mcmc, "Waterbuck"), 
       width = w, height = h)
ggsave("Figs_supp/giraffe_par_fig.pdf", 
       plot.pars(giraffe.mcmc, "Giraffe"), 
       width = w, height = h)
ggsave("Figs_supp/wildebeest_par_fig.pdf", 
       plot.pars(wildebeest.mcmc, "Blue Wildebeest"), 
       width = w, height = h)
ggsave("Figs_supp/zebra_par_fig.pdf", 
       plot.pars(zebra.mcmc, "Burchell's zebra"), 
       width = w, height = h)
ggsave("Figs_supp/impala_par_fig.pdf", 
       plot.pars(impala.mcmc, "Impala"), 
       width = w, height = h)
ggsave("Figs_supp/kudu_par_fig.pdf", 
       plot.pars(kudu.mcmc, "Greater kudu"), 
       width = w, height = h)
ggsave("Figs_supp/sable_par_fig.pdf", 
       plot.pars(sable.mcmc, "Sable antelope"), 
       width = w, height = h)


##
## Population posterior plots
##

pop.quantiles <- function(N, quantiles, years = 1977:1997, chain = 1){
    # Computes a data.frame of posterior quantiles from simulations of a 
    # population process
    #
    # args:
    #   N:  a t*d*n*c array, where t is the length of the process, d number of districts, n number of 
    #       simulations and c number of chains
    #   quantiles: a vector of quantiles to be computed (at least two)
    #   years: calendar years corresponding to the first component of N
    #   chain: which of the c chains to be used
    # returns:
    #   A data-frame with a column for each quantile and a row for each process step
    # 
    district.names <- c("South", "Central", "North", "FarNorth")
    qN <- t(apply(N[, 1, , chain], 1, quantile, probs = quantiles, na.rm = TRUE)) %>% 
        as.data.frame() %>% 
        setNames(paste("q", quantiles, sep = "")) %>% 
        mutate(year = years, district = district.names[1])
    for (i in 2:length(district.names)){
        qN <-rbind(qN,
                   t(apply(N[, i, , chain], 1, quantile, probs = quantiles, na.rm = TRUE)) %>% 
                       as.data.frame %>% 
                       setNames(paste("q", quantiles, sep = "")) %>% 
                       mutate(year = years, district = district.names[i]))
    }
    return(qN)
}


plot.pop <- function(mcmc.file, species, census){
    # A gridded plot of parameter posterior distributions
    #
    # args:
    #   mcmc.file:  file location of MCMC output (an .Rdata-file containing
    #       two rjags mcarray objects named out_exp for exponential model
    #       and out_log for logistic model, should contain log-population index
    #       named logN)
    #   species: character string of species name for main title
    #   census: data containing actual counts
    #
    load(mcmc.file)
    bind_rows(
        filter(census, Species == species) %>% 
            filter(year > 1976, year < 1998) %>% 
            gather(district, count, South:FarNorth) %>%
            left_join(pop.quantiles(exp(out_exp$logN), c(.05, .25, .5, .75, .95)),
                      by = c("year", "district")) %>% 
            mutate(model = "exponential"),
        filter(census, Species == species) %>% 
            filter(year > 1976, year < 1998) %>% 
            gather(district, count, South:FarNorth) %>%
            left_join(pop.quantiles(exp(out_log$logN), c(.05, .25, .5, .75, .95)),
                      by = c("year", "district")) %>% 
            mutate(model = "logistic")
    ) %>%
        ggplot(aes(x = year)) + 
        geom_line(aes(y = q0.5, color = district), size = 1) +
        geom_point(aes(y = count, color = district), size = 2) +
        geom_ribbon(aes(ymax = q0.95, ymin = q0.05, fill = district), alpha = .2) +
        geom_ribbon(aes(ymax = q0.75, ymin = q0.25, fill = district), alpha = .5) +    
        facet_grid(~ model) +
        labs(y = paste(species, "abundance index"))+
        panel_border() +
        ggtitle(paste(species, "posterior abundance"))+ theme(legend.position="top")
    
}

ggsave("Figs_supp/waterbuck_pop_fig.pdf",plot.pop(waterbuck.mcmc, "Waterbuck", census) , width = w, height = h)
ggsave("Figs_supp/giraffe_pop_fig.pdf",plot.pop(giraffe.mcmc, "Giraffe", census) , width = w, height = h)
ggsave("Figs_supp/wildebeest_pop_fig.pdf",plot.pop(wildebeest.mcmc, "Blue Wildebeest", census) , width = w, height = h)
ggsave("Figs_supp/zebra_pop_fig.pdf",plot.pop(zebra.mcmc, "Zebra", census) , width = w, height = h)
ggsave("Figs_supp/impala_pop_fig.pdf",plot.pop(impala.mcmc, "Impala", census) , width = w, height = h)
ggsave("Figs_supp/kudu_pop_fig.pdf",plot.pop(kudu.mcmc, "Kudu", census) , width = w, height = h)
ggsave("Figs_supp/sable_pop_fig.pdf",plot.pop(sable.mcmc, "Sable", census) , width = w, height = h)
