################################################################################
# Script for estimating temperature dependent R0 values for locations in 
# Asia at risk of yellow fever introductions 
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

airport_introductions <- read.csv("DATA/airports_risk.csv", stringsAsFactors=FALSE)[, -1]
temp_params <- read.csv("DATA/temp_params.csv", stringsAsFactors=FALSE)[, -1]
temp_params<- column_to_rownames(temp_params, "Variable")
temperature <- read.csv("DATA/prov_asia_temp.csv", stringsAsFactors=FALSE)[, -1]


# Identifying variables 
airport_country <- airport_introductions[, c(1,2)]
SEA <- c("China", "Hong Kong", "India", "Japan", "Malaysia", "Pakistan", "Philippines", "Singapore", "South Korea", "Thailand" )
ME <- c("Bahrain", "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia", "Turkey", "United Arab Emirates", "Jordan")
ME_code <- filter(airport_country, Asian_countries %in% ME ) $Airport
SEA_code <- filter(airport_country, Asian_countries %in% SEA ) $Airport


# source scripts
source("R/FUN_temp_R0.R")



################################################################################
# Estimate temperature dependent mosquito traits for each location at the 
# mean, min and max temperatures.
################################################################################
  
# average
  temp_variables_average <- temp_param(temperature$temp_mean)
  temp_variables_average$prop_Surv <- exp(-temp_variables_average$EIP / temp_variables_average$lf )
  
# min 
  temp_variables_min <- temp_param(temperature$temp_min)
  temp_variables_min $prop_Surv <- exp(-temp_variables_min $EIP / temp_variables_min $lf )
  
# max
  temp_variables_max <- temp_param(temperature$temp_max)
  temp_variables_max $prop_Surv <- exp(-temp_variables_max $EIP / temp_variables_max $lf )
  
  
# Other parameters to estimate R0 not sensitive to temperature 
  
mosq_person_SEA <- 0.85
mosq_person_ME <- 0.13
vec_human_transmission <- 0.25


# Estiamte R0_VH and R0_HV for all locations at the average, minimum and maximum temperature 


avg_R0_VH <- sapply(airport_introductions $Airport, function(x){est_R0_VH(x, temp_variables_average) }) 

avg_R0_HV<- sapply(airport_introductions$Airport, function(x){est_R0_HV_mean(x, 10000, temp_variables_average) }) 


min_R0_VH <- sapply(airport_introductions $Airport, function(x){est_R0_VH(x,  temp_variables_min) }) 

min_R0_HV<- sapply(airport_introductions $Airport, function(x){est_R0_HV_mean(x, 10000, temp_variables_min) }) 


max_R0_VH <- sapply(airport_introductions $Airport, function(x){est_R0_VH(x, temp_variables_max) }) 

max_R0_HV<- sapply(airport_introductions $Airport, function(x){est_R0_HV_mean(x, 10000, temp_variables_max) }) 



# Estiamte R0 for all locations 


R0_estimates_avg_temp <- sapply(airport_introductions $Airport, function(x){est_R0(x, 10000, temp_variables_average) })  #apply function to all airport codes 
R0_estimates_min_temp <- sapply(airport_introductions $Airport, function(x){est_R0(x, 10000, temp_variables_min) })
R0_estimates_max_temp <- sapply(airport_introductions $Airport, function(x){est_R0(x, 10000, temp_variables_max) })


avg_R0 <- tidy_R0(R0_estimates_avg_temp)
min_R0 <- tidy_R0(R0_estimates_min_temp)
max_R0 <- tidy_R0(R0_estimates_max_temp)  

list_R0 <-list(avg_R0, min_R0, max_R0  )

# Plot R0 estimates 

SEA_R0_estimates <- lapply(1:3, function(i){plot_SEA_R0(list_R0[[i]], SEA)} )
ME_R0_estimates <- lapply(1:3, function(i){plot_ME_R0(list_R0[[i]], ME)} )
       
################################################################################
# Figure 7: Mean (point) and 95% confidence intervals (bars) of temperature 
# dependent R0 estimates for cities predicted to be at risk of yellow fever 
# introduction in A) South and East Asia and B) West Asia and the Middle East. 
################################################################################

avg_R0_plots <- plot_grid(SEA_R0_estimates[[1]] , ME_R0_estimates[[1]] , ncol = 1, align = "h",labels = c("A", "B"),  label_size = 20)

avg_R0_plots_annot <- annotate_figure(avg_R0_plots, left = text_grob(" ", rot = 90, size = 18), 
                                   bottom = text_grob("City", size = 18, vjust = 0))

ggsave(plot=avg_R0_plots_annot, filename="figure_7.png", height = 11, width = 11)


################################################################################
# Sensitivity analysis on the impact of temperature on R0 estimates 
################################################################################


temp_range <- c(-13:44)  # temp range of locations in Asia 

temp_sens <- temp_param_sens(temp_range) # estimate parameter values for temp range


all_R0_estimates_sens<- sapply(temp_range, function(x){est_R0_sen(x, 10000, temp_sen) }) # estimate R0 for temp range

all_R0_estimates_sens[is.na(all_R0_estimates_sens)] <- 0  #set any NA to 0 


R0_sensitivity <- tidy_data(all_R0_estimates_sens, temp_range) # tidy data
R0_sensitivity<- rename(R0_sensitivity, Temperature = Variable )
R0_sensitivity$Temperature <- as.numeric(R0_sensitivity$Temperature )


################################################################################
# Appendix C: Distribution of R0 across the range of temperatures at 
# the at-risk cities in Asia. 
################################################################################

sensitivity_R0_plot <- ggplot(R0_sensitivity) + 
  geom_ribbon(aes(x=Temperature,ymin=lower_CI,ymax=upper_CI), color = "black", fill = "transparent", size = 0.2) +
  geom_line(aes(x=Temperature,y= mean), size = 0.6) +
  theme(legend.position="none") + 
  xlab("Temperature (°C)") + 
  ylab("R0") + 
  theme_hc(base_size = 24) + 
  scale_y_continuous(limits=c(0,7),breaks=seq(0,7,1)) + 
  scale_x_continuous(limits = c(-10,45),breaks=seq(-10,45,5)) + 
  theme(axis.title.y = element_text(vjust = 0.5, hjust=0.3, angle = 0))



ggsave(plot=sensitivity_R0_plot, filename="Appendix_C.png", height = 9, width = 11)


################################################################################
# Appendix D: Temperature dependent R0 estimates for South and East Asia at 
# A) the minimum temperature and B) maximum temperature
################################################################################

R0_plots_min_max<- plot_grid(SEA_R0_estimates[[2]] , SEA_R0_estimates[[3]] ,
                          ME_R0_estimates[[2]] , ME_R0_estimates[[3]] ,
                          ncol = 2, align = "h", labels = c("A", "C", "B", "D"),  label_size = 20)


R0_plots_min_max_annot <- annotate_figure(R0_plots_min_max,
                                    bottom = text_grob("City", size = 18, vjust = 0))


ggsave(plot=R0_plots_min_max_annot , filename="Appendix_D.png", height = 15, width = 24)

