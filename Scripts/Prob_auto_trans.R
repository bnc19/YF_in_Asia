################################################################################
# Script for estimating the probability of autochthonous transmission          
# given the introduction of at least one yellow fever case 
################################################################################



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
setwd("/Users/bethancracknelldaniels/Desktop/MSc project readings/YF in Asia Project")

# Data

airports_risk <- read.csv("DATA/airports_risk.csv", stringsAsFactors=FALSE)[, -1]
avg_R0_estimates <- read.csv("DATA/avg_R0_estimates.csv", stringsAsFactors=FALSE)[, -1]
min_R0_estimates <- read.csv("DATA/min_R0_estimates.csv", stringsAsFactors=FALSE)[, -1]
max_R0_estiamtes <- read.csv("DATA/max_R0_estimates.csv",stringsAsFactors=FALSE)[, -1]
avg_temp_var <- read.csv("DATA/temp_variables_mean.csv", stringsAsFactors=FALSE)[, -1]


# source scripts
source("R/FUN_prob_auto_trans.R")



################################################################################

# Estimate probability of transmission for all airports at all three temperatures 

list_R0_estimates <- list (avg_R0_estimates,min_R0_estimates,  max_R0_estiamtes)


list_prob_transmission_all_temp <- lapply(1:3, function(j) {sapply(1:29, function(i){
  
  
  prob_auto(n = 10000,
               R0_VH = list_R0_estimates[[j]]$R0_VH_mean[i], 
               mean_R0_HV = list_R0_estimates[[j]]$R0_HV_mean[i], 
               sd_R0_HV = list_R0_estimates[[j]]$R0_HV_sd[i], 
               seeds  = list_R0_estimates[[j]]$seeds[i], 
               sd_seeds = list_R0_estimates[[j]]$sd_seeds[i]) 
  
}) })


all_cities <- avg_R0_estimates$city 

tidy_results <- lapply(1:3, function(i){   #tidy results 
  tidy_auto(list_prob_transmission_all_temp[[i]], avg_R0_estimates$Airport, all_cities)
} ) 



# add column identifying temperature 
tidy_results[[1]]$temp <- rep("Avg", 29)

tidy_results[[2]]$temp <- rep("Min", 29)

tidy_results[[3]]$temp <- rep("Max", 29)


auto_probabilities <- bind_rows(tidy_results)

arrange_auto_probabilities <- arrange(auto_probabilities, city)

# seperate
auto_list_cities <- auto_probabilities %>%
  group_by(city) %>% 
  nest() %>% 
  select(data) 


# list of all city plots 


list_auto_plots <- lapply(1:29, function(i){
  plot_auto_cities(auto_list_cities[[2]][[i]], all_cities [i])
})



################################################################################
# Figure 8: Mean and 95% confidence intervals of the predicted probabilities of 
# autochthonous transmission in Asian Cities, given the independent 
# introduction of at least one infectious individual.
################################################################################


all_auto_plots <- do.call("grid.arrange", c(list_auto_plots, ncol=5))


all_auto_plots_anot <- annotate_figure(all_auto_plots, 
                                       left = text_grob("Probability of autochthonous transmission", 
                                       rot = 90, size = 22, vjust = 0.3, hjust = 0.3), 
                                       bottom = text_grob("Temperature", size = 22))


ggsave(plot=all_auto_plots_anot , filename="Figure_8.png", height = 17, width = 12)



################################################################################
# Sensitivity analyses of population immunity and dispersion parameter 
################################################################################

# select cities with probablities over 50%

R0_risk <- rbind(avg_R0_estimates[c(7, 8,12 ), ], max_R0_estiamtes[c(26, 27), ])

# values for sensitivity analyses 
immunity <- c(0:100)/100
k_sen <- c(0.01, 0.1, 0.5, 1)


# estimate the probabilities of auto transmission across range of values 

list_prob_imm_het <- lapply(1:5, function(k) {sapply(1:101, function(i) {sapply(1:4, function(j) { 
  
  prob_auto(n = 10000,
            R0_VH = R0_risk[k, 2], 
            mean_R0_HV = R0_risk[k, 4], 
            sd_R0_HV = R0_risk[k, 7], 
            k = k_sen[j],
            seeds = R0_risk[k, 8], 
            sd_seeds = R0_risk[k, 9],
            immunity = immunity[i])
} ) } ) } )



# tidy data 
format_list_imm <- lapply(1:5, function(i) {tidy_data(list_prob_imm_het[[i]] , immunity )} )


list_immunity_k <- lapply(1:5, function(i) {format_immunity_k_data(format_list_imm[[i]])} )



leg <- add_legend()

immunity_k_plots <- lapply(1:5, function(i){
  plot_immunity_k(list_immunity_k[[i]], R0_risk$city[i])
})



################################################################################
# Figure 9: Probability of autochthonous transmission across increasing levels  
# of population immunity at A) the average temperature in Dubai, Mumbai and 
# Guangzhou and B) the maximum temperature in Beijing and Shanghai. 
################################################################################

immunity_k_grid <- plot_grid(immunity_k_plots[[3]], immunity_k_plots[[4]], 
                             immunity_k_plots[[1]], immunity_k_plots[[5]],
                             immunity_k_plots[[2]], 
                             leg, ncol = 2 , labels = c("A", "B") , label_size  = 24) 



immunity_k_annot  <- annotate_figure(immunity_k_grid , 
                                     left = text_grob("Probability of autochthonous transmission", 
                                     rot = 90, size = 22, vjust = 0.3, hjust = 0.3), 
                                     bottom = text_grob("Temperature", size = 22))

                        


ggsave(plot=immunity_k_annot , filename="Figure_9.png", height = 16, width = 12)




################################################################################
# Sensitivity analyses on mosquito traits #
################################################################################

#estimate R0 VH values for range of competencies

cities <- list( "DXB", "BOM", "CAN", "BEY", "JED", "BKK")

competency <- (0:100) / 100

R0_HV_comp <- lapply(1:6, function(i){est_R0_VH_comp(cities[i], 10000, avg_temp_var, competency)})



### Estimate R0 VH values for range of mosquitos per person ###

mosq_per_person <- c(0.05, 0.13, 0.6, 0.85, 1.4)



R0_HV_mosq  <- lapply(1:6, function(j){sapply(1:5, function(i)
  {est_R0_HV_mean_mosq(cities[j], 10000, avg_temp_var, mosq_per_person[i]) }) } )


df_R0_HV_mosq  <- lapply(R0_HV_mosq , function(x) tidy_data(x, mosq_per_person))


### Estimate the probabilities of auto transmission ###

number_seeds_cd <- list_R0_estimates[[1]][c(12,7,8,5,17,6), ] # number of seeds for selected cities 

# estimate probabilities 
list_prob_com_den <- lapply(1:6, function(k) {sapply(1:101, function(i) {sapply(1:5, function(j) { prob_auto(
  n = 10000,
  R0_VH = R0_HV_comp[[k]][i],
  mean_R0_HV = df_R0_HV_mosq[[k]] [j, 2], 
  sd_R0_HV= df_R0_HV_mosq [[k]] [j, 5],
  seeds = number_seeds_cd [k, 8], 
  sd_seeds = number_seeds_cd [k, 9]) } ) } ) } ) 
  

# tidy data 

format_list <- lapply(1:6, function(i) {tidy_data(list_prob_com_den[[i]] , competency )} )

list_comp_dens <- lapply(1:6, function(i) {tidy_and_name_comp_den(format_list[[i]])} )


 # plot titles 
Titles <- c("Dubai", "Mumbai", "Guangzhou", "Beirut", "Jeddah", "Bangkok" ) 

comp_dens_plots <- lapply(1:6, function(i) {plot_comp_den(list_comp_dens[[i]] , Titles[i] )} )

legend <- comp_legend()
 

################################################################################
# Figure 10: Sensitivity analysis of the impact of mosquito density and 
# competence on the probability of autochthonous transmission at the average 
# temperature in Dubai, Beirut, Jeddah, Mumbai, Guangzhou and Bangkok. 
################################################################################

all_comp_dens_plots <- plot_grid(comp_dens_plots[[1]], comp_dens_plots[[2]], comp_dens_plots[[4]],
                                 comp_dens_plots[[3]], comp_dens_plots[[5]], comp_dens_plots[[6]],
                                 ncol = 2 , labels = c("A", "B") ,label_size  = 24)


all_comp_dens_anot <- annotate_figure(all_comp_dens_plots , legend, 
                                      left = text_grob("Probability of autochthonous transmission", 
                                      rot = 90, size = 22, vjust = 0.3, hjust = 0.3), 
                                      bottom = text_grob("Effective transmission rate from mosquito to human", size = 22))



ggsave(plot=all_comp_dens_anot , filename="Figure_10.png", height = 15, width =12 )
  
