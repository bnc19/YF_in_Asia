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
# setwd("/Users/bethancracknelldaniels/Desktop/MSc project readings/YF in Asia Project")

# Data 
airports_risk <- read.csv("DATA/airports_risk.csv", stringsAsFactors=FALSE)[, -1]
temp_params <- read.csv("DATA/temp_params.csv", stringsAsFactors=FALSE)[, -1]
temp_params<- column_to_rownames(temp_params, "Variable")
temperature <- read.csv("DATA/prov_asia_temp.csv", stringsAsFactors=FALSE)[, -1]


# Identifying variables 
SEA <- c("China", "Hong Kong", "India", "Japan", "Malaysia","Philippines", "Singapore", "South Korea", "Thailand" )
ME <- c("Bahrain", "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia", "Turkey", "United Arab Emirates")
ME_code <- filter(airports_risk, Asian_countries %in% ME ) $Airport
SEA_code <- filter(airports_risk, Asian_countries %in% SEA ) $Airport


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


# Estimate R0_VH and R0_HV for all locations at the average, minimum and maximum temperature 


avg_R0_VH <- sapply(airports_risk $Airport, function(x){est_R0_VH(x, temp_variables_average) }) 

avg_R0_HV<- sapply(airports_risk$Airport, function(x){est_R0_HV_mean(x, 10000, temp_variables_average) }) 


min_R0_VH <- sapply(airports_risk $Airport, function(x){est_R0_VH(x,  temp_variables_min) }) 

min_R0_HV<- sapply(airports_risk $Airport, function(x){est_R0_HV_mean(x, 10000, temp_variables_min) }) 


max_R0_VH <- sapply(airports_risk $Airport, function(x){est_R0_VH(x, temp_variables_max) }) 

max_R0_HV<- sapply(airports_risk $Airport, function(x){est_R0_HV_mean(x, 10000, temp_variables_max) }) 



# Estimate R0 for all locations 


R0_estimates_avg_temp <- sapply(airports_risk $Airport, function(x){est_R0(x, 10000, temp_variables_average) })  #apply function to all airport codes 
R0_estimates_min_temp <- sapply(airports_risk $Airport, function(x){est_R0(x, 10000, temp_variables_min) })
R0_estimates_max_temp <- sapply(airports_risk $Airport, function(x){est_R0(x, 10000, temp_variables_max) })


avg_R0 <- tidy_R0(R0_estimates_avg_temp)
min_R0 <- tidy_R0(R0_estimates_min_temp)
max_R0 <- tidy_R0(R0_estimates_max_temp)  

list_R0 <-list(avg_R0, min_R0, max_R0  )

all_R0_values <- sort_temp(temperature, list_R0, airports_risk)

# Filter by region 

ME_R0_all_temp <- filter(all_R0_values, Airport %in% ME_code)
SEA_R0_all_temp <- filter(all_R0_values, Airport %in% SEA_code)


# Colours for graph

graph_colours <- c("#006A40CC", "#FF9966", "#75B41ECC", "#FF0000", "#0099FF" ,"#8AB8CFCC", "#007E7FCC" ,
                   "#D95F02", "#000000" , "#F2990CCC", "#5A5895CC" ,"#E7298A" ,"#0099cc", "#660099", 
                   "#660000", "#FF99cc")


# Plot R0 estimates be region 

ME_R0_all_temp_plot <- ggplot(ME_R0_all_temp , aes(y = mean, x = temperature)) +
  geom_point(size = 3, aes(colour= City, shape = `Temperature (degree Celsius)`)) + 
  geom_errorbar(aes(ymin = lower_CI, 
                    ymax = upper_CI, 
                    colour= City), width = .25)  +
  xlab("") + 
  ylab("R0") + 
  theme_hc(base_size = 18) + 
  scale_y_continuous(limits=c(0,12.5),breaks=seq(0,12.5,1))+ 
  scale_x_continuous(limits=c(-15,45),breaks=seq(-15,45,5))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5), 
        axis.title.y = element_text(vjust = 0.5, hjust=0.3, angle = 0))+ 
  scale_color_manual( values = c(graph_colours)) +
  theme(legend.position="right")+ 
  guides(shape = FALSE)  


SEA_R0_all_tem<- ggplot(SEA_R0_all_temp , aes(y = mean, x = temperature)) +
  geom_point(size = 3, aes(colour= City, shape = `Temperature (degree Celsius)`)) + 
  geom_errorbar(aes(ymin = lower_CI, 
                    ymax = upper_CI, 
                    colour= City), width = .25)  +
  xlab("") + 
  ylab("R0") + 
  theme_hc(base_size = 18) + 
  scale_y_continuous(limits=c(0,13),breaks=seq(0,12.5,1))+ 
  scale_x_continuous(limits=c(-15,45),breaks=seq(-15,45,5))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5), 
        axis.title.y = element_text(vjust = 0.5, hjust=0.3, angle = 0))+ 
  scale_color_manual( values = c(graph_colours)) +
  theme(legend.position="right") + 
  guides(shape = FALSE)  



# Get legend 
l <- ggplot(SEA_R0_all_temp , aes(y = mean, x = temperature)) +
  geom_point(size = 3, aes(colour= City, shape = `Temperature (degree Celsius)`)) + 
  geom_errorbar(aes(ymin = lower_CI, 
                    ymax = upper_CI, 
                    colour= City), width = .25)  +
  xlab("") + 
  ylab("R0") + 
  theme_hc(base_size = 18) + 
  scale_y_continuous(limits=c(0,13),breaks=seq(0,12.5,1))+ 
  scale_x_continuous(limits=c(-15,45),breaks=seq(-15,45,5))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5), 
        axis.title.y = element_text(vjust = 0.5, hjust=0.3, angle = 0))+ 
  scale_color_manual( values = c(graph_colours)) + 
  guides(col = FALSE)  

lege <- get_legend(l)



################################################################################
# Supplementary figure 1: Mean (point) and 95% confidence intervals(bars) of 
# temperature dependent R0 estimates in cities predicted to be at risk of yellow 
# fever introduction in A) South and East Asia and B) West Asia and the Middle
# East. 
################################################################################

all_R0_plots <- plot_grid( ME_R0_all_temp_plot ,SEA_R0_all_tem , ncol = 1, align = "h",labels = c("A", "B"),  label_size = 20)

plots_legend <- plot_grid (all_R0_plots, lege, ncol = 1, rel_heights  = c(3,.1))


all_R0_plots_annot <- annotate_figure(plots_legend, left = text_grob(" ", rot = 90, size = 18), 
                                      bottom = text_grob("Temperature (degree Celsius)", size = 18, vjust = 0))

ggsave(plot=plots_legend, filename="Sup_figure_1.jpg", height = 12, width = 15)


