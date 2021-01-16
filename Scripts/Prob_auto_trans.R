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
# setwd("/Users/bethancracknelldaniels/Desktop/Manuscript/YF_in_Asia-master")

# Data

airports_risk <- read.csv("DATA/airports_risk.csv", stringsAsFactors=FALSE)[, -1]
avg_temp_var <-read.csv("DATA/AA_temp_variables_average.csv", stringsAsFactors = FALSE)[, -1]
max_temp_var <- read.csv("DATA/AL_temp_variables_max.csv", stringsAsFactors = FALSE)[, -1]
AL_avg_R0_estimates <- read.csv("DATA/AL_avg_R0_estimates.csv", stringsAsFactors=FALSE)[, -1]
AL_min_R0_estimates <- read.csv("DATA/AL_min_R0_estimates.csv", stringsAsFactors=FALSE)[, -1]
AL_max_R0_estimates <- read.csv("DATA/AL_max_R0_estimates.csv", stringsAsFactors=FALSE)[, -1]
AA_avg_R0_estimates <- read.csv("DATA/AA_avg_R0_estimates.csv", stringsAsFactors=FALSE)[, -1]
AA_min_R0_estimates <- read.csv("DATA/AA_min_R0_estimates.csv", stringsAsFactors=FALSE)[, -1]
AA_max_R0_estimates <- read.csv("DATA/AA_max_R0_estimates.csv", stringsAsFactors=FALSE)[, -1]
# source scripts
source("R/FUN_prob_auto_trans.R")



####################################################################################

# Estimate AA probability of transmission for all airports at all three temperatures 
####################################################################################

AA_list_R0_estimates <- list (AA_avg_R0_estimates,AA_min_R0_estimates,  AA_max_R0_estimates)


AA_list_prob_transmission_all_temp <- lapply(1:3, function(j) {sapply(1:11, function(i){
  
  
  prob_auto(n = 10000,
               R0_VH = AA_list_R0_estimates[[j]]$R0_VH_mean[i], 
               mean_R0_HV = AA_list_R0_estimates[[j]]$R0_HV_mean[i], 
               sd_R0_HV = AA_list_R0_estimates[[j]]$R0_HV_sd[i], 
               seeds  = AA_list_R0_estimates[[j]]$seeds[i], 
               sd_seeds = AA_list_R0_estimates[[j]]$sd_seeds[i]) 
  
}) })


AA_all_cities <- sort(AA_avg_R0_estimates$City)

AA_tidy_results <- lapply(1:3, function(i){   #tidy results 
  tidy_auto(AA_list_prob_transmission_all_temp[[i]], AA_avg_R0_estimates$Airport, AA_all_cities)
} ) 



# add column identifying temperature 
AA_tidy_results[[1]]$temp <- rep("Avg", 11)

AA_tidy_results[[2]]$temp <- rep("Min", 11)

AA_tidy_results[[3]]$temp <- rep("Max", 11)


AA_auto_probabilities <- bind_rows(AA_tidy_results)

  AA_arrange_auto_probabilities <- arrange(AA_auto_probabilities, city)

# seperate
AA_auto_list_cities <- AA_auto_probabilities %>%
  group_by(city) %>% 
  nest() %>% 
  select(data)  
 
AA_auto_list_cities <- AA_auto_list_cities[order(AA_auto_list_cities$city),]

# list of all city plots 


AA_list_auto_plots <- lapply(1:11, function(i){
  plot_auto_cities(AA_auto_list_cities[[2]][[i]], AA_all_cities [i])
})

AA_all_auto_plots <- do.call("grid.arrange", c(AA_list_auto_plots, ncol=5))






####################################################################################

# Estimate AL probability of transmission for all airports at all three temperatures 
####################################################################################

AL_list_R0_estimates <- list (AL_avg_R0_estimates,AL_min_R0_estimates,  AL_max_R0_estimates)


AL_list_prob_transmission_all_temp <- lapply(1:3, function(j) {sapply(1:13, function(i){
  
  
  prob_auto(n = 10000,
            R0_VH = AL_list_R0_estimates[[j]]$R0_VH_mean[i], 
            mean_R0_HV = AL_list_R0_estimates[[j]]$R0_HV_mean[i], 
            sd_R0_HV = AL_list_R0_estimates[[j]]$R0_HV_sd[i], 
            seeds  = AL_list_R0_estimates[[j]]$seeds[i], 
            sd_seeds = AL_list_R0_estimates[[j]]$sd_seeds[i]) 
  
}) })


AL_all_cities <- sort(AL_avg_R0_estimates$City)

AL_tidy_results <- lapply(1:3, function(i){   #tidy results 
  tidy_auto(AL_list_prob_transmission_all_temp[[i]], AL_avg_R0_estimates$Airport, AL_all_cities)
} ) 



# add column identifying temperature 
AL_tidy_results[[1]]$temp <- rep("Avg", 13)

AL_tidy_results[[2]]$temp <- rep("Min", 13)

AL_tidy_results[[3]]$temp <- rep("Max", 13)


AL_auto_probabilities <- bind_rows(AL_tidy_results)

AL_arrange_auto_probabilities <- arrange(AL_auto_probabilities, city)

# seperate
AL_auto_list_cities <- AL_auto_probabilities %>%
  group_by(city) %>% 
  nest() %>% 
  select(data)  

AL_auto_list_cities <- AL_auto_list_cities[order(AL_auto_list_cities$city),]

# list of all city plots 


AL_list_auto_plots <- lapply(1:13, function(i){
  plot_auto_cities(AL_auto_list_cities[[2]][[i]], AL_all_cities [i])
})



################################################################################
# Figure 2: Mean and 95% confidence intervals of the predicted probabilities of 
# autochthonous transmission in Asian Cities, given the independent 
# introduction of at least one infectious individual.
################################################################################


AL_all_auto_plots <- do.call("grid.arrange", c(AL_list_auto_plots, ncol=5))

all_auto_plots <- plot_grid(AA_all_auto_plots,AL_all_auto_plots, ncol = 1, labels = c("A", "B"))

all_auto_plots_anot <- annotate_figure(all_auto_plots, 
                                          left = text_grob("Probability of autochthonous transmission", 
                                                           rot = 90, size = 10, vjust = 0.3, hjust = 0.3), 
                                          bottom = text_grob("Temperature", size = 10))


ggsave(plot=all_auto_plots_anot, filename="Figure.jpg", height = 18, width = 15, units = "cm", dpi = 600)








################################################################################
# Sensitivity analyses of population immunity and dispersion parameter 
################################################################################

# select cities with probabilities over 50%

R0_risk <- rbind(AA_avg_R0_estimates[c(2, 3,8 ), ], AL_max_R0_estimates[c(2,3,4), ])

# values for sensitivity analyses 
immunity <- c(0:100)/100
k_sen <- c(0.01, 0.1, 0.5, 1)


# estimate the probabilities of auto transmission across range of values 

list_prob_imm_het <- lapply(1:6, function(k) {sapply(1:101, function(i) {sapply(1:4, function(j) { 
  
  prob_auto(n = 10000,
            R0_VH = R0_risk[k, 2], 
            mean_R0_HV = R0_risk[k, 3], 
            sd_R0_HV = R0_risk[k, 6], 
            k = k_sen[j],
            seeds = R0_risk[k, 7], 
            sd_seeds = R0_risk[k, 10],
            immunity = immunity[i])
} ) } ) } )



# tidy data 
format_list_imm <- lapply(1:6, function(i) {tidy_data(list_prob_imm_het[[i]] , immunity )} )



list_immunity_k <- lapply(1:6, function(i) {format_immunity_k_data(format_list_imm[[i]])} )


leg <- add_legend()

immunity_k_plots <- lapply(1:6, function(i){
  plot_immunity_k(list_immunity_k[[i]], R0_risk$City[i])
})



################################################################################
# Figure 3: Probability of autochthonous transmission across increasing levels  
# of population immunity at A) the average temperature in Dubai, Mumbai and 
# Guangzhou and B) the maximum temperature in Beijing and Shanghai. 
################################################################################

immunity_k_grid <- plot_grid(immunity_k_plots[[3]], immunity_k_plots[[4]], 
                             immunity_k_plots[[1]], immunity_k_plots[[5]],
                             immunity_k_plots[[2]], immunity_k_plots[[6]],
                             leg, ncol = 2 , labels = c("A", "B") , label_size  = 14) 



immunity_k_annot  <- annotate_figure(immunity_k_grid , 
                                     left = text_grob("Probability of autochthonous transmission", 
                                     rot = 90, size = 10, vjust = 0.1, hjust = 0.3), 
                                     bottom = text_grob("Proportion of the population assumed immune to yellow fever", 
                                                        size = 10, vjust = -10))

                        


ggsave(plot=immunity_k_annot , filename="Figure_3.jpg",height = 22, width = 13, units = "cm", dpi = 600)


################################################################################
# Sensitivity analyses on mosquito traits #
################################################################################

#estimate R0 VH values for range of competencies

cities <- R0_risk$Airport

competency <- (0:100) / 100

R0_VH_comp_avg <- lapply(1:3, function(i){est_R0_VH_comp(cities[i], 10000, avg_temp_var, competency)})

R0_VH_comp_max <- lapply(4:6, function(i){est_R0_VH_comp(cities[i], 10000, max_temp_var, competency)})

R0_VH_comp <- c(R0_VH_comp_avg, R0_VH_comp_max)

### Estimate R0 VH values for range of mosquitoes per person ###

mosq_per_person <- c(0.1,0.45,0.85,2,4,5)


# calculate for 3 cities with AA population at average temp

R0_HV_mosq_avg  <- lapply(1:3, function(j){sapply(1:6, function(i)
  {est_R0_HV_mean_mosq(cities[j], 10000, avg_temp_var, mosq_per_person[i]) }) } )

# calculate for 3 cities with AL population at max temp

R0_HV_mosq_max  <- lapply(4:6, function(j){sapply(1:6, function(i)
{est_R0_HV_mean_mosq(cities[j], 10000, max_temp_var, mosq_per_person[i]) }) } )

R0_HV_mosq <- c(R0_HV_mosq_avg, R0_HV_mosq_max)



df_R0_HV_mosq  <- lapply(R0_HV_mosq , function(x) tidy_data(x, mosq_per_person))


### Estimate the probabilities of auto transmission ###

number_seeds_cd <- R0_risk[,c(7,10)] # number of seeds for selected cities 

# estimate probabilities 
list_prob_com_den <- lapply(1:6, function(k) {sapply(1:101, function(i) {sapply(1:6, function(j) { prob_auto(
  n = 10000,
  R0_VH = R0_VH_comp[[k]][i],
  mean_R0_HV = df_R0_HV_mosq[[k]] [j, 2], 
  sd_R0_HV= df_R0_HV_mosq [[k]] [j, 5],
  seeds = number_seeds_cd [k, 1], 
  sd_seeds = number_seeds_cd [k, 2]) } ) } ) } ) 
  

# tidy data 

format_list <- lapply(1:6, function(i) {tidy_data(list_prob_com_den[[i]] , competency )} )

list_comp_dens <- lapply(1:6, function(i) {tidy_and_name_comp_den(format_list[[i]])} )


 # plot titles 
Titles <- c("Bangkok","Hong Kong", "Mumbai", "Beijing", "Beirut" ,"Guangzhou") 

comp_dens_plots <- lapply(1:6, function(i) {plot_comp_den(list_comp_dens[[i]] , Titles[i] )} )

legend <- comp_legend()
 

################################################################################
# Supplementary figure 2: Sensitivity analysis of the impact of mosquito density and 
# competence on the probability of autochthonous transmission at the average 
# temperature in Dubai, Beirut, Jeddah, Mumbai, Guangzhou and Bangkok. 
################################################################################

all_comp_dens_plots <- plot_grid(comp_dens_plots[[1]], comp_dens_plots[[4]], comp_dens_plots[[2]],
                                 comp_dens_plots[[5]], comp_dens_plots[[3]], comp_dens_plots[[6]],
                                 ncol = 2 , labels = c("A", "B") ,label_size  = 24)


all_comp_dens_anot <- annotate_figure(all_comp_dens_plots , legend, 
                                      left = text_grob("Probability of autochthonous transmission", 
                                      rot = 90, size = 22, vjust = 0.3, hjust = 0.3), 
                                      bottom = text_grob("Effective transmission rate from mosquito to human", size = 22))



ggsave(plot=all_comp_dens_anot , filename="Sup_figure_2.jpg", height = 15, width =12 )
  
  



