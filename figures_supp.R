waterbuck.mcmc <- "mcmc/Waterbuck2017-01-20.Rdata"
giraffe.mcmc <- "mcmc/Giraffe2017-01-20.Rdata"
zebra.mcmc <- "mcmc/Zebra2017-01-19.Rdata"
kudu.mcmc <- "mcmc/Kudu2017-01-20.Rdata"
wildebeest.mcmc <- "mcmc/Blue Wildebeest2017-01-19.Rdata"
sable.mcmc <- "mcmc/Sable2017-01-20.Rdata"
impala.mcmc <- "mcmc/Impala2017-01-20.Rdata"
source("load.R")
w <- 7
h <- 7

##
## Parameter posterior plots
##

plot.pars <- function(mcmc.file, species, pars = c("b0", "b1", "b2", "sig.err", "sig.pro"), 
                      labels = c("beta[0]", "beta[1]", "beta[2]", "sigma", "s")){
    load(mcmc.file)
    plotlist <- list()
    N <- length(out_exp[[pars[1]]])
    for (i in pars){
        plotlist[[i]] <- data.frame(value = c(as.matrix(out_exp[[i]]), 
                                              as.matrix(out_log[[i]])),
                                    model = c(rep("exponential", N), 
                                              rep("logistic", N))) %>% 
            ggplot(aes(color=model,x=value)) +
            geom_line(stat="density", size = 1) +
            xlab(parse(text = labels[which(pars ==i)])) +
            ylab("") + 
            theme(plot.margin = unit(c(6,0,6,0), "pt"),
                  legend.position = "none")+
            panel_border()
        
    }
    dummy <- data.frame(x = c(1, 1), model = c("exponential", "logistic"))
    legend <- get_legend(ggplot(dummy, aes(x = x, color = model)) + geom_line(stat="density", size = 1))
    p <- plot_grid(plotlist = plotlist, legend)
    title <- ggdraw() + draw_label(paste(species, "posterior densities"), fontface='bold')
    plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
}

ggsave("Figs/waterbuck_par_fig.pdf",plot.pars(waterbuck.mcmc, "Waterbuck") , width = w, height = h)
ggsave("Figs/giraffe_par_fig.pdf",plot.pars(giraffe.mcmc, "Giraffe") , width = w, height = h)
ggsave("Figs/wildebeest_par_fig.pdf",plot.pars(wildebeest.mcmc, "Blue Wildebeest") , width = w, height = h)
ggsave("Figs/zebra_par_fig.pdf",plot.pars(zebra.mcmc, "Burchell's zebra") , width = w, height = h)
ggsave("Figs/impala_par_fig.pdf",plot.pars(impala.mcmc, "Impala") , width = w, height = h)
ggsave("Figs/kudu_par_fig.pdf",plot.pars(kudu.mcmc, "Greater kudu") , width = w, height = h)
ggsave("Figs/sable_par_fig.pdf",plot.pars(sable.mcmc, "Sable antelope") , width = w, height = h)


##
## Population posterior plots
##

plot.pop <- function(mcmc.file, name, census){
    load(mcmc.file)
    bind_rows(
        filter(census, Species == name) %>% 
            filter(year > 1976, year < 1998) %>% 
            gather(district, count, South:FarNorth) %>%
            left_join(pop.quantiles(exp(out_exp$logN), c(.05, .25, .5, .75, .95)),
                      by = c("year", "district")) %>% 
            mutate(model = "exponential"),
        filter(census, Species == name) %>% 
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
        labs(y = paste(name, "abundance index"))+
        panel_border() +
        ggtitle(paste(name, "posterior abundance"))+ theme(legend.position="top")
    
}

ggsave("Figs/waterbuck_pop_fig.pdf",plot.pop(waterbuck.mcmc, "Waterbuck", census) , width = w, height = h)
ggsave("Figs/giraffe_pop_fig.pdf",plot.pop(giraffe.mcmc, "Giraffe", census) , width = w, height = h)
ggsave("Figs/wildebeest_pop_fig.pdf",plot.pop(wildebeest.mcmc, "Blue Wildebeest", census) , width = w, height = h)
ggsave("Figs/zebra_pop_fig.pdf",plot.pop(zebra.mcmc, "Zebra", census) , width = w, height = h)
ggsave("Figs/impala_pop_fig.pdf",plot.pop(impala.mcmc, "Impala", census) , width = w, height = h)
ggsave("Figs/kudu_pop_fig.pdf",plot.pop(kudu.mcmc, "Kudu", census) , width = w, height = h)
ggsave("Figs/sable_pop_fig.pdf",plot.pop(sable.mcmc, "Sable", census) , width = w, height = h)
