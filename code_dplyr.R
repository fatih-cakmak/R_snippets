## dplyr snippets
## libraries
library(magrittr)
library(tidyr)
library(dplyr)
library(ggvis)
##

##
flights <- read.csv("C:/Users/SAMSUNG/Dropbox/myWork/R/dplyr-tutorial/flights.csv")
hourly_delay <- flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(date, hour) %>% 
  summarise(
    delay = mean(dep_delay),
    n = n()
  ) %>% 
  filter(n > 10)
hourly_delay
##

## dplyr also works with remote datasources
## it can generate SQL code to communicate with databases
## In R, 1L means "1", 1 means "1.0"
## check:
##  browseVignettes(package = "dplyr")
##  stackoverflow: ...tagged/dplyr?sort=frequent






