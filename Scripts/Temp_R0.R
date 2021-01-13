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
library(EnvStats)


# Set working directory 
# setwd("/Users/bethancracknelldaniels/Desktop/MSc project readings/YF in Asia Project")

# Data 
airports_risk <- read.csv("DATA/airports_risk.csv", stringsAsFactors=FALSE)[, -1]
temp_params <- read.csv("DATA/temp_params.csv", stringsAsFactors=FALSE)[, -1]
temp_params<- column_to_rownames(temp_params, "Variable")
temperature <- read.csv("DATA/prov_asia_temp.csv", stringsAsFactors=FALSE)[, -1]
mosq_per_person <- read.csv("DATA/mosq_per_person.csv", col.names = c("City", "AA", "AL"))
mean_survival_aegypti <- read.csv("DATA/mean_survival_aegypti.csv")[, -1]
mean_survival_albopictus <- read.csv("DATA/mean_survival_albopictus.csv")[, -1]

source("R/FUN_temp_R0.R")



################################################################################
# Estimate temperature dependent Ae aegypti (AA) traits for each location at the 
# mean, min and max temperatures.
################################################################################
  
# average

AA_temp_variables_average <- temp_param(temperature$temp_mean)[,-5]

AA_temp_variables_average$lf <-  get_lifespan(mean_survival_aegypti, temperature$temp_mean)
  
AA_temp_variables_average$prop_Surv <- exp(-AA_temp_variables_average$EIP / AA_temp_variables_average$lf )

# min  

AA_temp_variables_min <- temp_param(temperature$temp_min)[,-5]

AA_temp_variables_min$lf <-  get_lifespan(mean_survival_aegypti, temperature$temp_min)
AA_temp_variables_min $prop_Surv <- exp(-AA_temp_variables_min $EIP / AA_temp_variables_min $lf )
  
# max

AA_temp_variables_max <- temp_param(temperature$temp_max)[,-5]
AA_temp_variables_max$lf <-  get_lifespan(mean_survival_aegypti, temperature$temp_max)
AA_temp_variables_max $prop_Surv <- exp(-AA_temp_variables_max $EIP / AA_temp_variables_max $lf )
  

# Select cities with AA population


AA_temp_variables_average <- select_cities(mosq_per_person[, -3], airports_risk, AA_temp_variables_average)


AA_temp_variables_min <- select_cities(mosq_per_person[, -3], airports_risk, AA_temp_variables_min)


AA_temp_variables_max <- select_cities(mosq_per_person[, -3], airports_risk, AA_temp_variables_max)


# Estimate R0_VH and R0_HV for all locations at the average, minimum and maximum temperature 


AA_avg_R0_VH <- sapply(1:11, function(x){est_R0_VH(AA_temp_variables_average[x,], B_VH = 0.24) }) 


AA_avg_R0_HV<- sapply(1:11, function(x){est_R0_HV_mean(10000, AA_temp_variables_average[x,], B_HV = 0.8) }) 


AA_min_R0_VH <- sapply(1:11, function(x){est_R0_VH(AA_temp_variables_min[x,], B_VH = 0.24) }) 

AA_min_R0_HV<- sapply(1:11, function(x){est_R0_HV_mean(10000, AA_temp_variables_min[x,], B_HV = 0.8) }) 


AA_max_R0_VH <- sapply(1:11, function(x){est_R0_VH(AA_temp_variables_max[x,], B_VH = 0.24) }) 

AA_max_R0_HV<- sapply(1:11, function(x){est_R0_HV_mean(10000, AA_temp_variables_max[x,], B_HV = 0.8) }) 



# Estimate R0 for all locations 


AA_R0_estimates_avg_temp <- sapply(1:11, function(x){est_R0(10000, AA_temp_variables_average[x,], B_HV = 0.8, B_VH = 0.24) })  #apply function to all airport codes 
AA_R0_estimates_min_temp <- sapply(1:11, function(x){est_R0(10000, AA_temp_variables_min[x,], B_HV = 0.8, B_VH = 0.24) })
AA_R0_estimates_max_temp <- sapply(1:11, function(x){est_R0(10000, AA_temp_variables_max[x,], B_HV = 0.8, B_VH = 0.24) })


AA_avg_R0 <- tidy_R0(AA_R0_estimates_avg_temp)
AA_min_R0 <- tidy_R0(AA_R0_estimates_min_temp)
AA_max_R0 <- tidy_R0(AA_R0_estimates_max_temp)  

AA_list_R0 <-list(AA_avg_R0, AA_min_R0, AA_max_R0  )

AA_all_R0_values <- sort_temp(temperature = temperature, list_R0 = AA_list_R0,airports_risk = airports_risk)



# Colours for graph

AA_graph_colours <- c("#006A40CC", "#FF9966", "#75B41ECC", "#FF0000", "#0099FF" ,"#8AB8CFCC", "#007E7FCC" ,
                   "#D95F02", "#000000" , "#F2990CCC", "#660000")


# Plot R0 estimates 

AA_R0_all_temp_plot <- ggplot(AA_all_R0_values , aes(y = mean, x = temperature)) +
  geom_point(size = 3, aes(colour= City, shape = `Temperature (degree Celsius)`)) + 
  geom_errorbar(aes(ymin = lower_CI, 
                    ymax = upper_CI, 
                    colour= City), width = .25)  +
  xlab("") + 
  ylab("R0") + 
  theme_hc(base_size = 18) + 
  scale_y_continuous(limits=c(0,6),breaks=seq(0,6,1))+ 
  scale_x_continuous(limits=c(-15,45),breaks=seq(-15,45,5))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5), 
        axis.title.y = element_text(vjust = 0.5, hjust=0.3, angle = 0))+ 
  scale_color_manual( values = c(AA_graph_colours)) +
  theme(legend.position="right")+ 
  guides(shape = FALSE)  





################################################################################
# Estimate temperature dependent Ae albopictus (AL) traits for each location at the 
# mean, min and max temperatures.
################################################################################

# average

AL_temp_variables_average <- temp_param(temperature$temp_mean)[,-3]

AL_temp_variables_average$lf <-  get_lifespan(mean_survival_albopictus, temperature$temp_mean)

AL_temp_variables_average$EIP <- AL_temp_variables_average$EIP*1.2

AL_temp_variables_average$prop_Surv <- exp(-(AL_temp_variables_average$EIP) / AL_temp_variables_average$lf )

# min  

AL_temp_variables_min <- temp_param(temperature$temp_min)[,-3]

AL_temp_variables_min$lf <-  get_lifespan(mean_survival_albopictus, temperature$temp_min)

AL_temp_variables_min$EIP <- AL_temp_variables_min$EIP*1.2
AL_temp_variables_min $prop_Surv <- exp(-(AL_temp_variables_min $EIP) / AL_temp_variables_min $lf )

# max

AL_temp_variables_max <- temp_param(temperature$temp_max)[,-3]
AL_temp_variables_max$lf <-  get_lifespan(mean_survival_albopictus, temperature$temp_max)

AL_temp_variables_max$EIP <- AL_temp_variables_max$EIP*1.2
AL_temp_variables_max $prop_Surv <- exp(-(AL_temp_variables_max $EIP) / AL_temp_variables_max $lf )


# Select cities with AL population


AL_temp_variables_average <- select_cities(mosq_per_person[, -2], airports_risk, AL_temp_variables_average)


AL_temp_variables_min <- select_cities(mosq_per_person[, -2], airports_risk, AL_temp_variables_min)


AL_temp_variables_max <- select_cities(mosq_per_person[, -2], airports_risk, AL_temp_variables_max)



# Estimate R0_VH and R0_HV for all locations at the average, minimum and maximum temperature 


AL_avg_R0_VH <- sapply(1:13, function(x){est_R0_VH(AL_temp_variables_average[x,], B_VH =0.13) }) 

AL_avg_R0_HV <- sapply(1:13, function(x){est_R0_HV_mean(10000, AL_temp_variables_average[x,], B_HV = 0.26) }) 


AL_min_R0_VH <- sapply(1:13, function(x){est_R0_VH(AL_temp_variables_min[x,], B_VH =0.13) }) 

AL_min_R0_HV<- sapply(1:13, function(x){est_R0_HV_mean(10000, AL_temp_variables_min[x,], B_HV = 0.26) }) 


AL_max_R0_VH <- sapply(1:13, function(x){est_R0_VH(AL_temp_variables_max[x,], B_VH =0.13) }) 

AL_max_R0_HV<- sapply(1:13, function(x){est_R0_HV_mean(10000, AL_temp_variables_max[x,], B_HV = 0.26) }) 



# Estimate R0 for all locations 


AL_R0_estimates_avg_temp <- sapply(1:13, function(x){est_R0(10000, AL_temp_variables_average[x,], B_VH =0.13,B_HV = 0.26 ) })  #apply function to all airport codes 
AL_R0_estimates_min_temp <- sapply(1:13, function(x){est_R0(10000, AL_temp_variables_min[x,], B_VH =0.13,B_HV = 0.26 ) })
AL_R0_estimates_max_temp <- sapply(1:13, function(x){est_R0(10000, AL_temp_variables_max[x,], B_VH =0.13,B_HV = 0.26 ) })



AL_avg_R0 <- tidy_R0(AL_R0_estimates_avg_temp)
AL_min_R0 <- tidy_R0(AL_R0_estimates_min_temp)
AL_max_R0 <- tidy_R0(AL_R0_estimates_max_temp)  

AL_list_R0 <-list(AL_avg_R0, AL_min_R0, AL_max_R0  )

AL_all_R0_values <- sort_temp(temperature, AL_list_R0, airports_risk)

AL_graph_colours <- c("#FF9966", "#5A5895CC" ,  "#CC0000" , "#E7298A" , "#75B41ECC", "#0099FF" ,"#8AB8CFCC",
                      "#D95F02", "#F2990CCC","#0099cc", "#660099", "#660000", "#FF99cc")

  
  


# Plot R0 

AL_R0_all_temp_plot <- ggplot(AL_all_R0_values , aes(y = mean, x = temperature)) +
  geom_point(size = 3, aes(colour= City, shape = `Temperature (degree Celsius)`)) + 
  geom_errorbar(aes(ymin = lower_CI, 
                    ymax = upper_CI, 
                    colour= City), width = .25)  +
  xlab("") + 
  ylab("R0") + 
  theme_hc(base_size = 18) + 
  scale_y_continuous(limits=c(0,6),breaks=seq(0,6,1))+ 
  scale_x_continuous(limits=c(-15,45),breaks=seq(-15,45,5))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5), 
        axis.title.y = element_text(vjust = 0.5, hjust=0.3, angle = 0))+ 
  scale_color_manual( values = c(AL_graph_colours)) +
  theme(legend.position="right")+ 
  guides(shape = FALSE)  



################################################################################
# Supplementary figure 1: Mean (point) and 95% confidence intervals(bars) of 
# temperature dependent R0 estimates in cities predicted to be at risk of yellow 
# fever introduction in A) South and East Asia and B) West Asia and the Middle
# East. 
################################################################################

all_R0_plots <- plot_grid(AA_R0_all_temp_plot , AL_R0_all_temp_plot, ncol = 1, align = "h",labels = c("A", "B"),  label_size = 20)



all_R0_plots_annot <- annotate_figure(all_R0_plots, left = text_grob(" ", rot = 90, size = 18), 
                                         bottom = text_grob("Temperature (degree Celsius)", size = 18, vjust = 0))

ggsave(plot=all_R0_plots_annot, filename="Sup_figure_1.jpg", height = 12, width = 15)

