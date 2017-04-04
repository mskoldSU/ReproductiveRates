library(tidyverse)
library(cowplot)
## url <- "http://dataknp.sanparks.org/sanparks/metacat?action=read&qformat=sanparks&sessionid=0&docid=judithk.815.1"
## census <- read_tsv(url)
census <- read_tsv("data/judithk.815.1-815.1")
dryrain <- read_csv("data/dryrain.csv", 
                    col_names = c("year", "South", "Central", "North", "FarNorth"),
                    skip = 1)

waterbuck.mcmc <- "mcmc/Waterbuck2017-01-20.Rdata"
giraffe.mcmc <- "mcmc/Giraffe2017-01-20.Rdata"
zebra.mcmc <- "mcmc/Zebra2017-01-19.Rdata"
kudu.mcmc <- "mcmc/Kudu2017-01-20.Rdata"
wildebeest.mcmc <- "mcmc/Blue Wildebeest2017-01-19.Rdata"
sable.mcmc <- "mcmc/Sable2017-01-20.Rdata"
impala.mcmc <- "mcmc/Impala2017-01-20.Rdata"

w <- 7
h <- 7

##
## Compares logistic and exponential functions for lambda = 1.5
##

ilogit <- function(x){1 / (1 + exp(-x))}

logist <- function(x, lambda){lambda * ilogit(x * lambda / (lambda - 1) - log(lambda - 1))}

logistic_vs_exp_fig <- ggplot(data.frame(x = c(log(0.5), log(2))), aes(x)) +
    stat_function(fun = logist, geom = "line", aes(colour = "logistic"),
                  args = list(lambda = 1.5), size = 1) +
    stat_function(fun = exp, geom = "line", aes(colour = "exponential"), 
                  size = 1) +
    scale_colour_manual("", values = c("red", "blue")) + 
    background_grid(major = "xy", minor = "xy") +
    ylab(expression(lambda)) +
    xlab(expression(x)) +
    ylim(c(0,2)) +
    geom_abline(slope = 1, intercept = 1, linetype = 3, size = 1) +
    theme(legend.position = c(.15, .9))+
    panel_border()
ggsave("Figs/logistic_vs_exp_fig.pdf", logistic_vs_exp_fig, width = w, height = h)
rm(logistic_vs_exp_fig)

##
## Population counts and dry season rainfall
##

animal_counts_fig <- census %>% 
    filter((Species == "Waterbuck") | (Species == "Giraffe")) %>% 
    filter(year > 1976, year < 1998) %>% 
    select(-TOTAL) %>% 
    gather(Region, Count, South:FarNorth) %>% 
    ggplot(aes(x = year, y = Count, color = Region)) +
    geom_point(show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    facet_grid(. ~ Species, scales = "free_y") +
    xlab("") +
    ylab("animal count") +
    panel_border()

rainfall_fig <- dryrain %>% 
    filter(year > 1976, year < 1998) %>%     
    gather(Region, Rainfall, South:FarNorth) %>%     
    ggplot(aes(x = year, y = Rainfall, fill = Region, width = 0.7)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_bar(stat = "identity", position = "dodge", 
             color = "black", show.legend = FALSE) +
    xlim(1976.5, 1994.5) +
    ylim(0, 200) +
    xlab("") +
    ylab("rainfall (mm)") +
    theme(legend.position = "top")+
    panel_border()

counts_and_rain_fig <- plot_grid(rainfall_fig, animal_counts_fig, 
                                 ncol = 1, align = "v", labels = c("a", "b"), 
                                 rel_heights = c(1, 1))
ggsave("Figs/counts_and_rain_fig.pdf", counts_and_rain_fig, width = w, height = h)

##
## Plot posterior distribution of variance parameters for Waterbuck data
##

load(waterbuck.mcmc)

plot.data <- bind_rows(data.frame(s = out_exp$sig.err[, , 2],
                                  sigma = out_exp$sig.pro[, , 2],
                                  model = "exponential"),
                       data.frame(s = out_log$sig.err[, , 2],
                                  sigma = out_log$sig.pro[, , 2],
                                  model = "logistic"))
rm(out_log, out_exp)
# Joint posterior density
sd_joint_fig <-  ggplot(plot.data, aes(x = s, y = sigma)) +
    geom_density2d(h = .04, aes(color = model)) +
    facet_grid(model ~ .) +
    xlab(expression(s)) +
    ylab(expression(sigma)) + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, .22)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, .33)) +
    geom_hline(yintercept = 0) +
    theme(legend.position="none")+
    panel_border()

# Marginal posterior densities
sd_marginal_fig <- gather(plot.data, parameter, value, s:sigma) %>% 
    ggplot(aes(x = value, color = model)) +
    stat_density(adjust = 2,
                 geom="line", position="identity", size = 1) +
    facet_grid(parameter ~ ., scales = "free_y", labeller = label_parsed) +
    xlab("") +
    geom_vline(xintercept = 0) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, .33)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position=c(.3, .9))+
    panel_border()

rm(plot.data)

sd_fig <- plot_grid(sd_marginal_fig, sd_joint_fig, 
                    labels = c("a", "b"), 
                    ncol = 2, rel_widths = c(1.3, 1))
ggsave("Figs/sd_fig.pdf", sd_fig, width = w, height = h)

##
## Waterbuck and Giraffe Central district plots
##

pop.quantiles <- function(N, quantiles, years = 1977:1997, chain = 1){
    # Computes a data.frame of posterior quantiles from simulations of a 
    # population process
    #
    # args:
    #   N:  a t*d*n*c array, where t is the length of the process, d number of districts, n number of 
    #       simulations and c number of chains
    #   quantiles: a vector of quantiles to be computed (more than one)
    # returns:
    #   A data-frame with a column for each quantile and a row for each process step
    # 
    district.names <- c("South", "Central", "North", "FarNorth")
    qN <- t(apply(N[, 1, , chain], 1, quantile, probs = quantiles, na.rm = TRUE)) %>% 
        as.data.frame() %>% 
        setNames(paste("q", quantiles, sep = "")) %>% 
        mutate(year = years, district = district.names[1])
    for (i in 2:4){
        qN <-rbind(qN,
                   t(apply(N[, i, , chain], 1, quantile, probs = quantiles, na.rm = TRUE)) %>% 
                       as.data.frame %>% 
                       setNames(paste("q", quantiles, sep = "")) %>% 
                       mutate(year = years, district = district.names[i]))
    }
    return(qN)
}

# Waterbuck
load(waterbuck.mcmc)
waterbuck_central_fig <- bind_rows(
    filter(census, Species == "Waterbuck") %>% 
        filter(year > 1976, year < 1998) %>% 
        gather(district, count, South:FarNorth) %>%
        left_join(pop.quantiles(exp(out_exp$logN), c(.05, .25, .5, .75, .95)),
                  by = c("year", "district")) %>% 
        mutate(model = "exponential"),
    filter(census, Species == "Waterbuck") %>% 
        filter(year > 1976, year < 1998) %>% 
        gather(district, count, South:FarNorth) %>%
        left_join(pop.quantiles(exp(out_log$logN), c(.05, .25, .5, .75, .95)),
                  by = c("year", "district")) %>% 
        mutate(model = "logistic")
) %>%  filter(district == "Central") %>% 
    ggplot(aes(x = year)) + 
    geom_line(aes(y = q0.5), size = 1, color = "blue") +
    geom_point(aes(y = count), size = 2) +
    geom_ribbon(aes(ymax = q0.95, ymin = q0.05), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymax = q0.75, ymin = q0.25), alpha = .5, fill = "blue") +    
    facet_grid(~ model) +
    labs(y = "Waterbuck abundance")+
    panel_border()
rm(out_log, out_exp)

# Giraffe
load(giraffe.mcmc)
giraffe_central_fig <- bind_rows(
    filter(census, Species == "Giraffe") %>% 
        filter(year > 1976, year < 1998) %>% 
        gather(district, count, South:FarNorth) %>%
        left_join(pop.quantiles(exp(out_exp$logN), c(.05, .25, .5, .75, .95)),
                  by = c("year", "district")) %>% 
        mutate(model = "exponential"),
    filter(census, Species == "Giraffe") %>% 
        filter(year > 1976, year < 1998) %>% 
        gather(district, count, South:FarNorth) %>%
        left_join(pop.quantiles(exp(out_log$logN), c(.05, .25, .5, .75, .95)),
                  by = c("year", "district")) %>% 
        mutate(model = "logistic")
) %>%  filter(district == "Central") %>% 
    ggplot(aes(x = year)) + 
    geom_line(aes(y = q0.5), size = 1, color = "blue") +
    geom_point(aes(y = count), size = 2) +
    geom_ribbon(aes(ymax = q0.95, ymin = q0.05), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymax = q0.75, ymin = q0.25), alpha = .5, fill = "blue") +    
    facet_grid(~ model) +
    labs(y = "Giraffe abundance")+
    panel_border()
rm(out_log, out_exp)

waterbuck_giraffe_central_fig <- plot_grid(waterbuck_central_fig,
                                           giraffe_central_fig, 
                                           labels = c("a", "b"), 
                                           ncol = 1)
ggsave("Figs/waterbuck_giraffe_central_fig.pdf", waterbuck_giraffe_central_fig, width = w, height = h)

##
## Posterior Waterbuck rate of increase years
##

load(waterbuck.mcmc)
waterbuck_rates_fig <- bind_rows(
    data.frame(lambda = exp(out_exp$logN[9, 2, , 1] - out_exp$logN[8, 2, , 1]),
               model = "exponential", par = "lambda[1985]"),
    data.frame(lambda = exp(out_log$logN[9, 2, , 1] - out_log$logN[8, 2, , 1]),
               model = "logistic", par = "lambda[1985]"),
    data.frame(lambda = exp(out_exp$logN[10, 2, , 1] - out_exp$logN[9, 2, , 1]),
               model = "exponential", par = "lambda[1986]"),    
    data.frame(lambda = exp(out_log$logN[10, 2, , 1] - out_log$logN[9, 2, , 1]),
               model = "logistic", par = "lambda[1986]")
) %>% 
    ggplot(aes(x = lambda, color = model)) +
    xlab("") +
    theme(legend.position = c(.15, .88)) +
    stat_density(adjust = 2,
                 geom="line", position="identity", size = 1) +
    geom_vline(xintercept = 1.5, lty = 3) +
    ylab("") +
    facet_grid(par ~ ., labeller = label_parsed)+
    panel_border()
rm(out_exp, out_log)

ggsave("Figs/waterbuck_rates_fig.pdf", waterbuck_rates_fig, width = w, height = h)

##
## Posterior rate of increase as function of rainfall
##

process.noise <- function(mcmc.out, rain){
    # Simulates the process part of the population model at equilibrium 
    # (i.e. without density dependence)
    #
    # args:
    #   mcmc.out:   A JAGS object containing draws of parameters b0,b2,sig.pro
    #   rain:   rain covariate
    #
    # Returns matrix of process noise
    N <- length(mcmc.out$b0[1, , 1])
    e <- t(outer(out_exp$b0[1, , 1], rain^0) +
               outer(out_exp$b2[1, , 1], rain) +
               outer(out_exp$sig.pro[1, , 1] * rnorm(N), rain^0))
    return(e)
}
rain <- 0:300 # Range of rainfall values
load(waterbuck.mcmc)

rate_vs_rain_fig <- bind_rows(
    logist(process.noise(out_log, rain), lambda = 1.5) %>% 
        apply(., 1, quantile, probs = c(0.25, 0.5, 0.75)) %>% 
        t %>% 
        as.data.frame %>% 
        mutate(model = "logistic", rain = rain),
    exp(process.noise(out_exp, rain)) %>% 
        apply(., 1, quantile, probs = c(0.25, 0.5, 0.75)) %>% 
        t %>% 
        as.data.frame %>% 
        mutate(model = "exponential", rain = rain)) %>% 
    ggplot(aes(x = rain)) +
    geom_ribbon(aes(ymax = `75%`, ymin = `25%`, fill = model), alpha = .2) +
    geom_line(aes(y = `50%`, color = model), size = 1) +
    ylab("Rate of increase") +
    xlab("Rainfall") +
    geom_histogram(data = gather(dryrain, Region, Rainfall, South:FarNorth), 
                   aes(x = Rainfall, weight = 0.02), 
                   fill = "white", colour = "black") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 300)) + 
    scale_y_continuous(expand = c(0, 0)) +
    geom_hline(yintercept = 1.5, linetype = 3) +
    theme(legend.position = c(.15, .88))+
    panel_border()
rm(rain, out_exp, out_log)

ggsave("Figs/rate_vs_rain_fig.pdf", rate_vs_rain_fig, width = w, height = h)

##
## Waterbuck prediction, high/low precipitation
##

predict.Central.Waterbuck <- function(mcmc.out, T, rain, logybar, link, ...){ 
    # Predicts Central district Waterbuck population
    #
    # args: 
    #   mcmc.out: A JAGS object containing draws of parameters 
    #      logN,b0,b2,sig.pro
    #   T: number of timesteps to be predicted
    #   rain: dry season rainfall
    #   logybar: mean log population size used in MCMC
    #   link: exponential or logistic
    n.sim <- dim(mcmc.out$logN)[3]
    Npred <- matrix(ncol = n.sim, nrow = T)
    Npred[1,] <- exp(mcmc.out$logN[20 ,2 , , 1])
    b0 <- mcmc.out$b0[, , 1]
    b1 <- mcmc.out$b1[, , 1]
    b2 <- mcmc.out$b2[, , 1]
    sig.pro <- mcmc.out$sig.pro[, , 1]
    for (i in 2:T){
        e<-rnorm(n.sim, 0, sd = sig.pro)
        Npred[i,] <- Npred[i-1, ] * 
            link(b1 * (log(Npred[i-1, ]) - logybar) + b0 + b2 * rain[i] + e)
    }
    return(Npred)
}

load(waterbuck.mcmc)

logybar <- census %>% 
    filter(Species == "Waterbuck", year > 1976, year < 1998) %>% 
    select(South, Central, North, FarNorth) %>%
    log %>% 
    unlist %>% 
    mean(., na.rm = TRUE)

high_rain <- dryrain %>% 
    filter(year %in% 1981:1987) %>% # Period of high precipitation
    .$Central

low_rain <- dryrain %>% 
    filter(year %in% 1988:1994) %>% # Period of low precipitation
    .$Central

logist1.5 <- function(x) logist(x, lambda = 1.5)

high_prec_fig <- bind_rows(
    predict.Central.Waterbuck(out_log, 7, high_rain, logybar, logist1.5) %>% 
        apply(., 1, quantile, probs = c(.05, .25,.5 , .75, .95)) %>% 
        t %>% 
        as.data.frame %>% 
        mutate(year = 1996:2002, model = "logistic"),
    predict.Central.Waterbuck(out_exp, 7, high_rain, logybar, exp) %>% 
        apply(., 1, quantile, probs = c(.05, .25,.5 , .75, .95)) %>% 
        t %>% 
        as.data.frame %>% 
        mutate(year = 1996:2002, model = "exponential")) %>% 
    ggplot(aes(x=year)) + 
    geom_ribbon(aes(ymax = `95%`, ymin = `5%`), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymax = `75%`, ymin = `25%`), alpha = .5, fill = "blue") +  
    geom_line(aes(y = `50%`), color = "blue") +
    facet_grid(~ model) +
    labs(y="Waterbuck abundance index")+
    panel_border()


low_prec_fig <- bind_rows(
    predict.Central.Waterbuck(out_log, 7, low_rain, logybar, logist1.5) %>% 
        apply(., 1, quantile, probs = c(.05, .25,.5 , .75, .95)) %>% 
        t %>% 
        as.data.frame %>% 
        mutate(year = 1996:2002, model = "logistic"),
    predict.Central.Waterbuck(out_exp, 7, low_rain, logybar, exp) %>% 
        apply(., 1, quantile, probs = c(.05, .25,.5 , .75, .95)) %>% 
        t %>% 
        as.data.frame %>% 
        mutate(year = 1996:2002, model = "exponential")) %>% 
    ggplot(aes(x=year)) + 
    geom_ribbon(aes(ymax = `95%`, ymin = `5%`), alpha = .2, fill = "blue") +
    geom_ribbon(aes(ymax = `75%`, ymin = `25%`), alpha = .5, fill = "blue") +  
    geom_line(aes(y = `50%`), color = "blue") +
    facet_grid(~ model) +
    labs(y="Waterbuck abundance index") +
    panel_border()

high_low_prec_fig <- plot_grid(high_prec_fig,
                               low_prec_fig, 
                               labels = c("a", "b"), 
                               ncol = 1)
ggsave("Figs/high_low_prec_fig.pdf", high_low_prec_fig, width = w, height = h)