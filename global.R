# Attach packages
library(tidyverse)
library(broom)
library(foreign)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(ineq)
library(IC2)
library(raster)
library(leaflet)

#########################################################3
# Part I. Create a Database
# We would like you to create a database with 30,000 observations. We attach a STATA do file that does this and a STATA ado file which you will need. The data you will create has regions, weights, 200 income variables, and 200 tax variables.
#########################################################

# Important note: It was not clear from the instructions what each element of this
# database "means". For example, there are 30,000 "observations".
# For the purposes of this exercise, I am assuming that an observation is a 
# place (town, municipality, etc.), and that each income variable refers to 
# an individual person. Ie, I'm assuming that there are 30,000 "places" and 200
# "people" per place.

# Use a cached file if necessary, to speed up app runtime
if('prepared_data.RData' %in% dir()){
  load('prepared_data.RData')
} else {
  # Lines beginning with "#stata:" indicate the stata code being translated
  
  # Replicate `Test data prep.do`
  # Set a random number order for reproducibility
  #stata: set seed 458267
  set.seed(458267) 
  
  # Create a dataframe with 30,000 observations
  #stata: set obs 30000
  n <- 30000
  df <-
    data_frame(id = 1:n,
               #stata: gen regions = int(runiform()*9)
               region = sample(0:9, size = n, replace = TRUE)) %>%
    mutate(region = as.integer(region)) %>%
    #stata: replace region = 1 if inrange(region,0,3)
    #stata: replace region = 2 if inrange(region,4,5)
    mutate(region = ifelse(between(region, 0, 3), 1,
                           ifelse(between(region, 4, 5), 2,
                                  region))) %>%
    #stata: gen x =runiform()
    mutate(x = runif(n = n, min = 0, max = 1)) %>%
    #stata: sort x
    arrange(x) %>%
    #stata: drop x
    dplyr::select(-x) %>%
    #stata: gen weight = uniform()*15000
    mutate(weight = runif(n = n) * 15000)
  
  #stata: replace weight =10000*runiform() if weight<1000
  df <- df %>%
    mutate(weight = ifelse(weight < 1000,
                           10000 * runif(n = length(which(df$weight < 1000))),
                           weight))
  # Income per capita
  #stata: forval z=1/200{
  #stata:   gen income_`z' = region*runiform()*4000 + rnormal()*200
  #stata: }
  for (z in 1:200){
    # Define the name of the new variable
    variable_name <- paste0('income_', z)
    # Create values to populate the variable
    values <- df$region * 
      runif(n = nrow(df)) * 
      4000 +
      rnorm(n = nrow(df)) * 
      200
    # Populate
    df[,variable_name] <- values
  }
  
  
  #########################################################
  # Part II. Estimate standard poverty and inequality indicators
  # Once the data is created, we ask that you calculate the standard Foster–Greer–Thorbecke (FGT) poverty indicators (poverty headcount, poverty gap, and poverty gap squared). Please do this using three poverty lines {3000, 2000, 5500} for each of the income concepts. In addition, please calculate the Gini and Theil indices for each income concept. Attached please find a STATA do-file that does this. Please generate a Stata log file and an R output file with the results so that we can corroborate that you obtain the same results using both programs.
  #########################################################
  
  # This section attempts to reproduce the results from proc_incs.do
  
  #stata: local peso weight
  #stata: local lines 3000 2000 5500
  lines <- c(3000, 2000, 5500)
  
  # Make data "long" instead of wide for easier analysis
  # See https://www.jstatsoft.org/article/view/v059i10/v59i10.pdf for justification
  dfl <- df %>%
    gather(person_id, value, income_1:income_200) %>%
    mutate(person_id = parse_number(person_id))
  
  # Arrange by poverty
  dfl <- dfl %>% arrange(value) 
  
  # Condition to ensure that incomes are above zero.
  dfl$value <- ifelse(dfl$value < 0, 0, dfl$value)
  
  # POVERTY HEADCOUNT RATE
  # Create variables for whether someone is "poor" at each of 
  # the 3 poverty lines
  dfl <- dfl %>%
    mutate(poor_3000 = value < 3000,
           poor_2000 = value < 2000,
           poor_5000 = value < 5000)
  
  
  #########################################################
  # Part III. R Shiny visualization
  # Please present your results using Shiny. You can choose how and what to show.
  #########################################################
  
  # Get map of Panama
  pan <- getData(name = 'GADM', country = 'PAN', level = 1)
  
  # Keep only the 6 southernmost regions
  keep <- order(coordinates(pan))[1:6]
  pan <- pan[keep,]
  
  # Give them a region number
  pan$region <- 1:6
  
  save(df, dfl, pan,
       file = 'prepared_data.RData')
}
