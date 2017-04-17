# Runs mcmc for all species

set.seed(1)
library(rjags)
library(tidyverse)

census <- read_tsv("data/judithk.815.1-815.1")
dryrain <- read_csv("data/dryrain.csv", 
                    col_names = c("year", "South", "Central", "North", "FarNorth"),
                    skip = 1)

exponential.model <- "model
{
# Likelihood
for (i in 1:4){
    # Set initial value to fist observation
    logN[start.idx[i], i] <- y[start.idx[i], i]
    for (t in (start.idx[i] + 1):end.idx[i]){
        y[t, i] ~ dnorm(logN[t, i], tau.err)
        logN[t, i] <- logN[t - 1, i]+e[t, i]
        e[t, i] ~ dnorm(b0.tilde + b1 * (logN[t - 1, i] - y.bar) + b2 * rain[t - 1, i], tau.pro)
    }
}
# Priors
b0.tilde ~ dnorm(0, 0.000001)
b0 <- b0.tilde - b1 * y.bar
b1 ~ dnorm(0, 0.000001)
b2 ~ dnorm(0, 0.000001)
tau.err <- 1 / (s * s) # Observation error precision
s ~ dunif(0, 1000)
tau.pro <- 1 / (sigma * sigma) # Process noise precision
sigma ~ dunif(0, 1000)
}"

logistic.model <- "model
{
# Likelihood
for (i in 1:4){
    # Set initial value to fist observation
    logN[start.idx[i], i] <- y[start.idx[i], i]
    for (t in (start.idx[i] + 1):end.idx[i]){
        y[t, i] ~ dnorm(logN[t, i], tau.err)
        logN[t, i] <- logN[t - 1, i] + log(r) - log(exp(-r * e[t,i] / (r - 1)) * (r - 1) + 1)
        e[t,i] ~ dnorm(b0.tilde + b1 * (logN[t - 1, i] - y.bar) + b2 * rain[t - 1,i], tau.pro)
    }
}
# Priors
b0.tilde ~ dnorm(0, 0.000001)
b0 <- b0.tilde - b1 * y.bar
b1 ~ dnorm(0, 0.000001)
b2 ~ dnorm(0, 0.000001)
tau.err <- 1 / (s * s) # Observation error precision
s ~ dunif(0, 1000)
tau.pro <- 1 / (sigma * sigma) # Process noise precision
sigma ~ dunif(0, 1000)
}
"
write(exponential.model, file = "exponential.jag")
write(logistic.model, file = "logistic.jag")

rain <- dryrain %>% 
    filter(year > 1976, year < 1998) %>% 
    select(South, Central, North, FarNorth)

# MCMC iterations and thinning
N <- 10000000
thin <- 1000

# Run MCMC for all species
for (species in c("Blue Wildebeest", "Zebra", "Impala", "Giraffe", "Kudu", "Waterbuck", "Warthog", "Sable")){
    y <- census %>% 
        filter(Species == species, year > 1976, year < 1998) %>% 
        select(South, Central, North, FarNorth) %>%
        log()
    start.idx <- apply(y, 2, function(x){min(which(!is.na(x)))})
    end.idx <- apply(y, 2, function(x){max(which(!is.na(x)))})
    jags.data <- list(y = y, 
                      rain = rain, 
                      start.idx = start.idx, 
                      end.idx = end.idx,
                      r = 1.5,
                      y.bar = mean(unlist(y), na.rm = TRUE))
    jags_exp <- jags.model("exponential.jag",
                           data = jags.data, 
                           n.chains = 2, n.adapt = 10000)
    update(jags_exp, 1000000)
    jags_log <- jags.model("logistic.jag",
                           data = jags.data, 
                           n.chains = 2, n.adapt = 10000)
    update(jags_log, 1000000)
    
    par <- c("b0", "b1", "b2", "logN", "s", "sigma")
    out_exp <- jags.samples(jags_exp, par, N, thin = thin)
    out_log <- jags.samples(jags_log, par, N, thin = thin)
    save(out_log, out_exp, file = paste("mcmc/", species,".Rdata", sep = ""))
}

