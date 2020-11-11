
################################################################################
# Script for quantifying the risk of yellow fever case introductions in Asia 
################################################################################

#install.packages(c("epiflows", "readxl", "dplyr", "tidyr", "tibble", "ggplot2",
#                  "cowplot", "grid", "gridExtra", "ggpubr", "ggsci", "ggthemes", "reshape2"))

################################################################################
# Preamble
################################################################################

# Load packages 
library(epiflows)
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)
library(ggpubr)
library(ggsci)
library(ggthemes)
library(reshape2)


# Set working directory 
# setwd("~/Desktop/Manuscript/YF_in_Asia-master")


# Data  
asia_flows <- read.csv("DATA/asia_flows.country.csv")
locations <- read.csv("DATA/asia_ec_locations_airport.csv")
locations_country <- locations[c(1:101),-1 ]
airport_flows <- read.csv("DATA/asia_flows_airport.csv")[ , c(2:4)]


# source scripts
source("R/FUN_YF_introductions.R")


################################################################################
# Tidy data and create a flow for each endemic country 
# detailing passenger movement 
################################################################################

# Identifying variable 

Endemic_countries <- c("Angola", "Benin", "Burkina Faso", "Burundi", "Cameroon", 
                       "Central African Republic", "Chad", "Congo", "Ivory Coast (Cote d'Ivoire)", 
                       "Democratic Republic of the Congo", "Equatorial Guinea", "Eritrea", "Ethiopia", 
                       "Gabon", "Gambia", "Ghana", "Guinea", "Guinea Bissau", "Kenya", "Liberia", 
                       "Mali", "Mauritania", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
                       "Senegal", "Sierra Leone", "Somalia", "South Sudan", "Sudan", "Tanzania", "Togo", 
                       "Uganda", "Zambia", "Argentina", "Bolivia", "Brazil", "Colombia", "Ecuador", 
                       "French Guiana", "Guyana", "Panama", "Paraguay", "Peru", "Suriname", 
                       "Trinidad and Tobago", "Venezuela")

matrix_country_flows <- sapply(1:48, function(i){ tidy_epiflows (asia_flows, Endemic_countries[i] ) } )  # Creates matrix of flows

list_country_flows <- split(matrix_country_flows, col(matrix_country_flows)) # convert to list 

flows_df <- lapply(1:48, function(i) {as.data.frame(list_country_flows[[i]] )})  # convert to dataframes within list 

flows_df <- lapply(1:48, function(i) {setNames(flows_df[[i]] , c("from", "to", "n" ) ) })  # rename columnns 
                           
            

################################################################################
# Estimate the number of introductions for each endemic country 
################################################################################


# epiflow object for each endemic country

list_epiflows   <-  lapply(1:48, function(i) {make_epiflows (
   flows = flows_df [[i]],
   locations = locations_country   ,
   pop_size = "location_population",
   duration_stay = "length_of_stay",
   num_cases = "num_cases_time_window",
   first_date = "first_date_cases",
   last_date = "last_date_cases" ) } )     

# Extract duration of stay 

duration_stay <- locations_country[c(1:48), 6]


# Estimate the number of introductions into Asian countries from each endemic country 

list_country_epiflow_results  <- lapply(1:48, function(i) {estimate_risk_spread(
                                   list_epiflows[[i]],
                                   location_code = Endemic_countries[i],
                                   r_incubation = incubation,
                                   r_infectious = infectious,
                                   n_sim = 10000,
                                   return_all_simulations = FALSE,
                                   avg_length_stay_days  = duration_stay[i] ) } )




# Sao Tome and Principe only had flights to China so epiflows returned all 10,000 simulations. Take mean of these.

list_country_epiflow_results[[26]] <- aggregate(list_country_epiflow_results[[26]], 
                                                by = list(list_country_epiflow_results[[26]]$mean_cases, 
                                                          list_country_epiflow_results[[26]]$lower_limit_95CI, 
                                                          list_country_epiflow_results[[26]]$upper_limit_95CI), 
                                                mean)[4:6]

# Select Asian countries with an upper 95% confidence interval exceeding one introduction 

countries_at_risk <- select_at_risk(list_country_epiflow_results)

countries_at_risk[20, 1] <- c("UAE") # rename United Arab Emirates


countries_at_risk <- rename(countries_at_risk, "Country" = Group.1)



################################################################################
# Estimate the number of introductions from endemic countries into Asian 
# airports that are within countries found to be at risk of at least one 
# yellow fever introduction
################################################################################

# drop Asian countries from locations 

locations_airport <- locations[-c(49:101), -1]

# Tidy data and create a flow for each endemic country detailing passanger movement 

matrix_airport_flows <- sapply(1:48, function(i){ tidy_epiflows (airport_flows,Endemic_countries[i] ) } )  # Creates matrix of flows

list_airport_flows <- split(matrix_airport_flows, col(matrix_airport_flows)) # convert to list 

airport_flows_df <- lapply(1:48, function(i) {as.data.frame(list_airport_flows[[i]] )})  # convert to dataframes within list 

airport_flows_df <- lapply(1:48, function(i) {setNames(airport_flows_df[[i]] , c("from","to", "n" ) ) })  # rename columnns 



# Create an epiflow object for each endemic country


list_epiflows_airport   <-  lapply(1:48, function(i) {make_epiflows (
  flows = airport_flows_df [[i]],
  locations = locations_airport    ,
  pop_size = "location_population",
  duration_stay = "length_of_stay",
  num_cases = "num_cases_time_window",
  first_date = "first_date_cases",
  last_date = "last_date_cases" ) } )     




# Estimate the number of introductions into Asian countries from each endemic country 

list_airport_epiflow_results  <- lapply(1:48, function(i) {estimate_risk_spread(
  list_epiflows_airport[[i]],
  location_code = Endemic_countries[i],
  r_incubation = incubation,
  r_infectious = infectious,
  n_sim = 10000,
  return_all_simulations = FALSE,
  avg_length_stay_days  = duration_stay[i] ) } )



# Sao Tome and Principe only had flights to China so epiflows returned all 10,000 simulations. Take mean of these.

list_airport_epiflow_results[[26]] <- aggregate(list_airport_epiflow_results[[26]], 
                                                by = list(list_airport_epiflow_results[[26]]$mean_cases, 
                                                          list_airport_epiflow_results[[26]]$lower_limit_95CI, 
                                                          list_airport_epiflow_results[[26]]$upper_limit_95CI), 
                                                mean)[4:6]

# Select Asian countries with an upper 95% confidence interval exceeding one introduction 

airports_at_risk <- select_at_risk(list_airport_epiflow_results)
airports_at_risk <- rename(airports_at_risk, "Airport" = Group.1)


Asian_countries <- c("India", "United Arab Emirates", "Bahrain", "Lebanon","Thailand" , "India",  
                     "China","India","Saudi Arabia","Qatar",  "United Arab Emirates","Hong Kong", 
                     "South Korea", "Turkey", "Saudi Arabia", "Malaysia","Kuwait","Oman", 
                     "Saudi Arabia","Philippines", "Japan", "China","China","Saudi Arabia","Singapore")

# Add column identifying the country for each airport at risk 

airports_risk <- cbind(airports_at_risk,Asian_countries )

airport_country <- airports_risk[, c(1,5)]


# Reorder by country by changing the factor levels 

airports_risk$Airport <- factor(airports_risk$Airport, levels = airports_risk$Airport [order(airports_risk$Asian_countries)])

# Of countries at risk, divide by region in Asia (SEA: South and East Asia, ME: Middle East and West Asia)
SEA <- c("China", "Hong Kong", "India", "Japan", "Malaysia",  "Philippines", "Singapore", "South Korea", "Thailand" )
ME <- c("Bahrain", "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia", "Turkey", "United Arab Emirates")

#Filter risk by region 
SEA_airport<-  filter(airports_risk , `Asian_countries` %in% SEA )
ME_airport <-  filter(airports_risk , `Asian_countries` %in% ME  )

################################################################################
# Figure 1: Mean and 95% confidence intervals of the total predicted number of 
# introductions of yellow fever into A) Airports in South and East Asia and
# B) Airports in West Asia and the Middle East. 
################################################################################

airport_plots <- plot_airport(SEA_airport,ME_airport)

ggsave(plot=airport_plots, filename="Figure.1.jpg",height = 18, width = 13, units = "cm", dpi = 600)



