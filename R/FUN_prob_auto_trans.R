################################################################################
################################################################################
# Functions for estimating the probability of autochthonous transmission given 
# the introduction of at least one yellow fever case 
################################################################################
################################################################################



################################################################################
# Functions to estimate the probabality of autochthonous results
################################################################################

prob_auto<- function(n, mean_R0_HV , sd_R0_HV,R0_VH,  seeds, sd_seeds, k = 0.1, immunity = 0){
  
  # n = number of draws from distributions
  
  # mean_R0_HV = the mean number of infectious vectors produced per infectious human, calculated for each location 
  
  # sd_R0_HV= the sd for the number of vectors produced per infectious human, calculated for each location 
  
  # R0_VH = the mean number of humans produced per infectious vector, calculated for each location 
  
  # s = the number of independent seeds at each locations
  
  # sd_seeds = the sd for the number of independent seeds at each locations
  
  # k = the dispersion parameter 
  
  # immunity = proportion of the population assumed to be immune to yellow fever
  
  
  
  R0_HV <- rnorm(n, mean_R0_HV, sd_R0_HV)  # draw from R0_HV distribution 
  R0_HV <-  ifelse(R0_HV <0 , 0, R0_HV)    # no negative values
  
  gv_0  <- dnbinom(0, mu =  R0_HV, size = k)   # probability of an infectious human producing 0 infectious vectors  
  # extinction at 0 generations
  not_gv_0 <- 1 - gv_0                         #probability of an infectious human producing at least one infectious vector 
  

  R0_VH <-  ifelse(R0_VH <0 , 0, R0_VH)    # no negative values
  
  Reff <-  R0_VH * (1 - (immunity) )
  gh_0  <- dnbinom(0, mu =  Reff, size = k)   # probability of an infectious vector producing 0 infectious humans
  # extinction at 0 generations
  
  
  prob_no_trans <- gv_0  +  not_gv_0 *  gh_0   # probability of no H -> V transmission and 
  # probability of H -> V transmission but no V -> H transmission 
  
  
  s <- rnorm(n , seeds, sd_seeds)
  
  prob_auto_trans <-  1 - (prob_no_trans ^s )   # probability of not going extinct, at least one infectious human produced 
  # given the introduction of multiple independent infectious humans (s)
  
  
  mean_auto <- mean(prob_auto_trans,na.rm = T)            # calculate the mean and 95% limits of the probability of auto transmission 
  quant <- quantile(prob_auto_trans, c(0.025, 0.975),na.rm = T)
  
  out <- data.frame(mean = mean_auto,         #  data frame of the results 
                    lower_CI = quant[1],
                    upper_CI = quant[2])
  
  return(out)
  
} # estimate probabilities


tidy_data <- function(x, name_col) {
  
  x [is.na(x )] <- 0
  write.csv(x, file = "x")
  read <- column_to_rownames(read.csv("x"), "X")
  
  colnames(read) <- name_col #name columns temperature   
  df <- as.data.frame(t(read))  # transpose 
  
  output <- df %>%  
    rownames_to_column(var = "Variable")
  
  
  
  return(output)
  
}   # tidy data


tidy_auto <- function (prob_transmission, airport_code, city) {
  

  tidy <- tidy_data(prob_transmission, airport_code) # reformat 
  tidy <- rename(tidy, Airport = Variable)
  
  tidy$mean <- round(tidy$mean, 3) # round data
  tidy$lower_CI <- round(tidy$lower_CI, 3)
  tidy$upper_CI <- round(tidy$upper_CI, 3)
  
  out <- cbind(city, tidy) # add city identifier 
  
  
  out$city <- as.character(out$city )


  return(out)
  
} # format data



################################################################################
# Funciton to plot the probability of autochthonous transmission for all cities 
################################################################################

plot_auto_cities <- function(x, city){
  
  x$color <- ifelse(x$upper_CI >= 0.5, 'red', 'black')  # if upper 95% CI is above 0.5 code as red 
  
  ggplot(x  , aes(y = mean, x = temp)) +   # plot
    geom_point(size = 2, color =x$color ) +
    geom_errorbar(aes(ymin = lower_CI ,
                      ymax = upper_CI), color = x$color,  width = .25) +
    ggtitle(city) + 
    ylab("") + 
    xlab("") + 
    theme_hc(base_size = 16) +
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+ 
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5), 
          axis.title.y = element_text(vjust = 0.5, hjust=0.3, angle = 0))  }



################################################################################
# Funtion to format the results of the immunity and dispersion parameter 
# sensitivity analyses 
################################################################################

format_immunity_k_data <- function(x) {
  tidy <- rename(x,"Immunity" = Variable )   
  
  tidy$Immunity <- as.numeric(tidy$Immunity ) 
  
  add_sufix <- (rep(k_sen, 3)) # identify k 
  
  index <- order(add_sufix)
  suffix <- add_sufix[index]  
    
  temp <- rep(c("mean", "lower", "upper"), 4) # identify output 
  colnames_temp <-paste(temp, suffix, sep="_")  
  
  colnames(tidy)[-1] <- colnames_temp
  
  out <- pivot_longer(tidy, cols = - `Immunity`,  
                      names_to = "dispersion parameter (k)",
                      values_to = c("Mean", "Lower", "Upper"),
                      names_prefix = c("mean_", "lower_", "upper_"))
  
  return(out)
}



################################################################################
# Funcitons to plot the immunity and sensitivity analyses  
################################################################################

plot_immunity_k <- function(x, city) {
plots <- ggplot(x) + 
  geom_ribbon(aes(x=Immunity,ymin=Lower,ymax=Upper, color = `dispersion parameter (k)`, 
                  group = `dispersion parameter (k)`, fill = `dispersion parameter (k)`),  size = 0.2, alpha = .2, show.legend = FALSE) +
  geom_line(aes(x=Immunity,y= Mean, color = `dispersion parameter (k)`, 
                group = `dispersion parameter (k)`), size = 0.6, show.legend = FALSE )+
  xlab("") + 
  ylab("") + 
  theme_hc(base_size = 18) + 
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) + 
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) +
  ggtitle(city) + 
  geom_segment(x = 0, xend = 5, y = 0.5, yend = 0.5, size = 0.5, linetype = 3) +
  geom_segment(x = 0.8, xend = 0.8, y = 0, yend = 5, size = 0.5, linetype = 3) 
  
 
  
  return(plots)
  
} # plot 
  
} # plot 

add_legend <- function() {
l <- ggplot(list_immunity_k[[1]]) + 
  geom_ribbon(aes(x=Immunity,ymin=Lower,ymax=Upper, color = `dispersion parameter (k)`, 
                  group = `dispersion parameter (k)`, fill = `dispersion parameter (k)`),  
              size = 0.2, alpha = .2, show.legend = FALSE) +
  geom_line(aes(x=Immunity,y= Mean, color = `dispersion parameter (k)`, 
                group = `dispersion parameter (k)`), size = 0.6, show.legend = T )+
  xlab("") + 
  ylab("") + 
  theme_hc(base_size = 24) + 
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) + 
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) +
  ggtitle("city") + 
  geom_segment(x = 0, xend = 5, y = 0.5, yend = 0.5, size = 0.5, linetype = 3) + 
  theme(legend.position = c(.60, .80), legend.text = element_text( size = 24))

leg <- get_legend(l)

return(leg)
} # legend



################################################################################
# Funcitons for mosquito density and competency analysis 
################################################################################

est_R0_VH_comp <- function(airport.code, n, temp_variables, comp) {
  
  TV <- temp_variables #temp variable can be mean, min, max 
  
  biting_rate <- TV$a[TV$temperature.airport  == airport.code] # select location specific mosq / person (SEA or Middle East)
  
  
  B_VH <- comp # effective transmission from vector to human set at 0.25 following "Global Risk and Elimination of Yellow Fever Epidemics"
  
  
  lf <- TV$lf[TV$temperature.airport == airport.code]  # select airport temp specific longevity 
  
  
  R0 <- biting_rate * lf* B_VH  # multiply all parameters 
  
  return(R0)
}  # estimate R0_VH with varied competency 

infectious <- function(n) {  rnorm(n, 4.5, 1.5/1.96) } # randomly draw from the infectious period distribution

human_vec_transmission <- function(n) {rnorm(n, 0.4, 0.1)} # randomly draw from the infectious the effective human vector transmission rate  

est_R0_HV_mosq <- function(airport.code, n,  temp_variables, mosq_per_person) { 
  
  TV <- temp_variables  #temp variable can be mean, min, max 
  
  mosq_person <- mosq_per_person
  biting_rate <- TV$a[TV$temperature.airport == airport.code]  # select airport temp specific biting rate 
  
  
  
  B_HV <- human_vec_transmission(n)  # draw from distribution of effectve human to vector transmission 
  
  
  prob_surv <- TV$prop_Surv[TV$temperature.airport == airport.code]  # select airport temp specific prob surv of EIP 
  
  DI <- infectious(n)  # draw from distribution for infectiousness 
  
  R0_HV <- mosq_person * biting_rate * prob_surv * B_HV * DI  # multiply all parameters 
  
  return(R0_HV)
} #estimate R0 HV with varied density

est_R0_HV_mean_mosq <- function(airport.code, n, temp_variables, mosq_per_person){
  
  TV <- temp_variables 
  
  airport.code <- airport.code
  
  n <- n
  
  mosq <- mosq_per_person
  
  
  R0_HV <- est_R0_HV_mosq(airport.code, n, TV, mosq) # estimate R0_HV
  
  
  mean_HV <- mean(R0_HV, na.rm = TRUE)   # calculate mean, 95 CI, quantile of R0_VH
  quant_HV <- quantile(R0_HV, c(.025, .975), na.rm = TRUE)
  sd_HV <- sd(R0_HV)
  
  
  out_HV <- data.frame( mean = mean_HV, 
                        lower_CI = quant_HV[1],
                        upper_CI = quant_HV[2], 
                        sd = sd_HV) 
  
  return(out_HV)} #estimate mean of HV from sampling 

tidy_and_name_comp_den <- function(x) {
  tidy <- rename(x,"Competency" = Variable )
  
  
  add_sufix <- (rep(mosq_per_person, 3))
  
  index <- order(add_sufix)
  suffix <- add_sufix[index]  
  
  temp <- rep(c("mean", "lower", "upper"), 5)
  colnames_temp <-paste(temp, suffix, sep="_")  
  
  colnames(tidy)[-1] <- colnames_temp
  
  
  out <- pivot_longer(tidy, cols = - `Competency`,
                      names_to = "Female mosquitos per person",
                      values_to = c("Mean", "Lower", "Upper"),
                      names_prefix = c("mean_", "lower_", "upper_"))
  
 out$Competency <- as.numeric(out$Competency )
  
  return(out)
} #  tidy and name mosquito sensitivity analysis output 


################################################################################
# Functions to plot mosquito sensitivity analysis 
################################################################################

plot_comp_den <- function(x, cities) {
  
  plot <- ggplot(x) + 
    geom_ribbon(aes(x=Competency,ymin=Lower,ymax=Upper, color = `Female mosquitos per person`, group = `Female mosquitos per person`, 
                    fill = `Female mosquitos per person`),  size = 0.2, alpha = .1, show.legend = FALSE) +
    geom_line(aes(x=Competency,y= Mean, color = `Female mosquitos per person`, group = `Female mosquitos per person`), size = 0.6, show.legend = F )+
    xlab("") + 
    ylab("") + 
    theme_hc(base_size = 18) + 
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) + 
    scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) +
    ggtitle(cities) + 
    geom_segment(x = 0, xend = 5, y = 0.5, yend = 0.5, size = 0.5, linetype = 3) + 
    geom_segment(x = 0.25, xend = 0.25, y = 0, yend = 5, size = 0.5, linetype = 1, color = "grey")
} # plot 

comp_legend <- function() {
  plot <- ggplot(list_comp_dens[[1]] ) + 
    geom_ribbon(aes(x=Competency,ymin=Lower,ymax=Upper, color = `Female mosquitos per person`, group = `Female mosquitos per person`, 
                    fill = `Female mosquitos per person`),  size = 0.2, alpha = .1, show.legend = T) +
    geom_line(aes(x=Competency,y= Mean, color = `Female mosquitos per person`, group = `Female mosquitos per person`), size = 0.6, show.legend = F )+
    xlab("") + 
    ylab("") + 
    theme_hc(base_size = 18) + 
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) + 
    scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) +
    ggtitle(cities) + 
    geom_segment(x = 0, xend = 5, y = 0.5, yend = 0.5, size = 0.5, linetype = 3) + 
    geom_segment(x = 0.25, xend = 0.25, y = 0, yend = 5, size = 0.5,  color = "grey")
  
  leg <- get_legend(plot)
  
  return(leg)
} # legend


