# Runs mcmc for all species

set.seed(1)
library(rjags)
source("load_data.R")

# JAGS model code
exponential.model <- "model
{
# Likelihood
for (i in 1:4){
    # Set initial value to fist observation
    logN[start.idx[i], i] <- y[start.idx[i], i]
    for (t in (start.idx[i] + 1):end.idx[i]){
        y[t, i] ~ dnorm(logN[t, i], tau.err)
        logN[t, i] <- logN[t - 1, i] + e[t, i]
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

# JAGS model code
logistic.model <- "model
{
# Likelihood
for (i in 1:4){
    # Set initial value to fist observation
    logN[start.idx[i], i] <- y[start.idx[i], i]
    for (t in (start.idx[i] + 1):end.idx[i]){
        y[t, i] ~ dnorm(logN[t, i], tau.err)
        logN[t, i] <- logN[t - 1, i] + log(lambda) - log(exp(-lambda * e[t,i] / (lambda - 1)) * (lambda - 1) + 1)
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
    
    # JAGS data
    jags.data <- list(y = y, # log-population count
                      rain = rain, # dry-season rainfall, all districts 1977-1997
                      start.idx = apply(y, 2, function(x){min(which(!is.na(x)))}), # year of first observed value
                      
                      end.idx = apply(y, 2, function(x){max(which(!is.na(x)))}), #year of last observed value
                      lambda = 1.5, #  maximal reproductive rate (lambda_max, only used in logistic model)
                      y.bar = mean(unlist(y), na.rm = TRUE)) # average observed log-count
    
    # Construct model objects and run burn-in period
    jags_exp <- jags.model("exponential.jag",
                           data = jags.data, 
                           n.chains = 2, n.adapt = 10000)
    update(jags_exp, 1000000)
    jags_log <- jags.model("logistic.jag",
                           data = jags.data, 
                           n.chains = 2, n.adapt = 10000)
    update(jags_log, 1000000)
    
    # Parameters to monitor
    par <- c("b0", "b1", "b2", "logN", "s", "sigma")
    
    # Main MCMC
    out_exp <- jags.samples(jags_exp, par, N, thin = thin)
    out_log <- jags.samples(jags_log, par, N, thin = thin)
    
    save(out_log, out_exp, file = paste("mcmc/", species,".Rdata", sep = ""))
}

