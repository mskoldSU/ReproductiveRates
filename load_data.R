# Loads data

library(tidyverse)

census <- read_tsv("data/judithk.815.1-815.1")
## url <- "http://dataknp.sanparks.org/sanparks/metacat?action=read&qformat=sanparks&sessionid=0&docid=judithk.815.1"
## census <- read_tsv(url)

dryrain <- read_csv("data/dryrain.csv", 
                    col_names = c("year", "South", "Central", "North", "FarNorth"),
                    skip = 1)