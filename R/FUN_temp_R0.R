################################################################################
################################################################################
# Functions for estimating temperature dependent R0 values for locations in 
# Asia at risk of yellow fever introductions 
################################################################################
################################################################################



################################################################################
# Functions to estimate temp suitability 
################################################################################

briere <-  function(Temp, parm){                   # function for biting rate and PDR 
  out <-  parm[3]*Temp*(Temp-parm[1])*(parm[2]-Temp)^0.5
  return(out)
}

quad <- function(Temp, parm){    # function for mortality rate
  out <- -parm[3]*(Temp-parm[1])*(parm[2] -Temp)
  return(out)
}



################################################################################
# Function for estimating biting rate, lifespan and extrinsic incubaiton
# period from temperature 
################################################################################

temp_param <- function(Temp) {
  
  a <- briere(Temp, parm = c(
    temp_params[1,1], 
    temp_params[2,1], 
    temp_params[3,1]))  #biting rate
  
  lf <- quad(Temp, parm = c(
    temp_params[1,2], 
    temp_params[2,2], 
    temp_params[3,2])) # mortality 
  
  lf <- ifelse( lf<=0, 1, lf ) # restrict to positive 
  
  
  PDR <- briere(Temp, parm = c(
    temp_params[1,3], 
    temp_params[2,3], 
    temp_params[3,3])) #parasite development 
  
  PDR[PDR<0] = 0.001
  EIP <- 1/PDR  #restict to positive 
  
  
  
  temp_variables <- data.frame(temperature$airport, temperature$country, a, lf, EIP)

  
  return(temp_variables)
}



################################################################################
# Functions to estimate R0 
################################################################################

infectious <- function(n) {  rnorm(n, 4.5, 1.5/1.96) }  # randomly draw from the infectious period distribution

human_vec_transmission <- function(n) {rnorm(n, 0.4, 0.1)} # randomly draw from the infectious the effective human vector transmission rate
 
est_R0_HV <- function(airport.code, n,  temp_variables) {   # estimate R0_HV
  
  TV <- temp_variables  #temp variable can be mean, min, max 
  
  
  mosq_person <- ifelse(airport.code %in% ME_code, mosq_person_ME, mosq_person_SEA) # select location specific mosq / person (SEA or Middle East)
  
  biting_rate <- TV$a[TV$temperature.airport == airport.code]  # select airport temp specific biting rate 
  
  
  
  B_HV <- human_vec_transmission(n)  # draw from distribution of effectve human to vector transmission 
  
  
  prob_surv <- TV$prop_Surv[TV$temperature.airport == airport.code]  # select airport temp specific prob surv of EIP 
  
  DI <- infectious(n)  # draw from distribution for infectiousness 
  
  
  R0_HV <- mosq_person * biting_rate * prob_surv * B_HV * DI  # multiply all parameters 
  
  return(R0_HV)
}  # estimate R0 HV

est_R0_VH <- function(airport.code,temp_variables) { # estimate R0_VH
  
  TV <- temp_variables #temp variable can be mean, min, max 
  
  
  biting_rate <- TV$a[TV$temperature.airport == airport.code] # select location specific mosq / person (SEA or Middle East)
  
  
  B_VH <- 0.25  # effective transmission from vector to human set at 0.25 following "Global Risk and Elimination of Yellow Fever Epidemics"
  
  
  lf <- TV$lf[TV$temperature.airport == airport.code]  # select airport temp specific longevity 
  
  
  R0 <- biting_rate * lf* B_VH  # multiply all parameters 
  

  return(R0)
} # estimate R0 VH

est_R0_HV_mean <- function(airport.code, n, temp_variables){ #estimate the mean and 95 CI of R0_HV
  
  TV <- temp_variables
  airport.code <- airport.code
  n <- n

  
  
  R0_HV <- est_R0_HV(airport.code, n, TV) # estimate R0_HV
  
  
  mean_HV <- mean(R0_HV, na.rm = TRUE)   # calculate mean, 95 CI, quantile of R0_VH
  quant_HV <- quantile(R0_HV, c(.025, .975), na.rm = TRUE)
  sd_HV <- sd(R0_HV)
  
  
  out_HV <- data.frame( 
                        mean = mean_HV, 
                        lower_CI = quant_HV[1],
                        upper_CI = quant_HV[2], 
                        sd = sd_HV) 
  
  return(out_HV)} #estimate mean of HV from sampling 

est_R0 <- function(airport.code, n, temp_variables) { # estimate mena and 95% CI R0
  
  TV <- temp_variables
  
  airport.code <- airport.code
  
  n <- n
  
  
  R0_VH <- est_R0_VH(airport.code,  TV)  # estimate R0_VH
  
  R0_HV <- est_R0_HV(airport.code, n, TV) # estimate R0_HV
  
  
  R0 <- R0_VH * R0_HV  # multiply together 
  
  
  mean <- mean(R0, na.rm = TRUE)   # calculate mean, 95 CI, quantile 
  quant <- quantile(R0, c(.025, .975), na.rm = TRUE)
  sd <- sd(R0)
  
  out <- data.frame( mean = mean, 
                     lower_CI = quant[1],
                     upper_CI = quant[2], 
                     sd = sd)
  #return data frame of variables 
  
  return(out)
  
  
}  # estimate R0 

tidy_R0 <- function(x, col_names) {
  
  x [is.na(x )] <- 0
  write.csv(x, file = "x")
  read <- column_to_rownames(read.csv("x"), "X")
  
  df <- as.data.frame(t(read))  # transpose 
  
  output <- df %>%  
    rownames_to_column(var = "Airport")
  
  
  return(output)
  
} # tidy results 



################################################################################
# Function to plot R0 estimates 
################################################################################

plot_SEA_R0 <- function(R0_estimates, SEA) {
  
  x <- left_join(R0_estimates, airport_country)
  
  region_est <- x %>% 
    filter(Asian_countries %in% SEA ) %>% 
    arrange(Asian_countries, Airport)
  
  
  #rename as cities 
  
  cities <- c("Guangzhou", "Beijing", "Shanghai", "Hong Kong", "Ahmedabad", "Mumbai", "New Delhi", "Chennai", "Hyderabad", "Tokyo", "Kuala Lumpur","Karachi", "Manila", "Singapore", "Seoul", "Bangkok") 
  
  
  region_est$city <- cities
  
  # reorder by country by changing the factor levels 
  
  region_est$city <- factor(region_est$city , levels = region_est$city  [order(region_est$Asian_countries)])
  
  
  rename_est <- rename(region_est, "South and East Asian Countries"  = Asian_countries)
  
  
  SEA_R0_plot <- ggplot(rename_est , aes(y = mean, x = city)) +
    geom_point(size = 2, aes(colour= `South and East Asian Countries`)) +
    geom_errorbar(aes(ymin = lower_CI, 
                      ymax = upper_CI, 
                      colour=`South and East Asian Countries`), width = .25) + 
    xlab(" ") + 
    ylab("R0") + 
    theme_hc(base_size = 14) + 
    scale_color_npg() + 
    scale_y_continuous(limits=c(0,12),breaks=seq(0,12,2))+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5), 
          axis.title.y = element_text(vjust = 0.5, hjust=0.3, angle = 0))
  
  return(SEA_R0_plot)
  
} # plot in SEA region 

plot_ME_R0 <- function(R0_estimates, ME) {
  
  x <- left_join(R0_estimates, airport_country)
  
  region_est <- x %>% 
    filter(Asian_countries %in% ME )%>% 
    arrange(Asian_countries, Airport)
  
  
  
  #rename as cities 
  
cities <- c("Manama", "Amman", "Kuwait City", "Beirut", "Muscat", "Doha", "Dammam", 
                 "Jeddah", "Medina", "Riyadh", "Istanbul", "Abu Dhabi", "Dubai")
  
  
  region_est$city <- cities
  
  # reorder by country by changing the factor levels 
  
  region_est$city <- factor(region_est$city , levels = region_est$city  [order(region_est$Asian_countries)])
  
  
  rename_est <- rename(region_est, "West Asia and Middle Eastern Countries"  = Asian_countries)
  
  
  ME_R0_plot <- ggplot(rename_est , aes(y = mean, x = city)) +
    geom_point(size = 2, aes(colour= `West Asia and Middle Eastern Countries`)) +
    geom_errorbar(aes(ymin = lower_CI, 
                      ymax = upper_CI, 
                      colour=`West Asia and Middle Eastern Countries`), width = .25) + 
    xlab(" ") + 
    ylab("R0") + 
    theme_hc(base_size = 14) + 
    scale_color_npg() + 
    scale_y_continuous(limits=c(0,12),breaks=seq(0,12,2))+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5), 
          axis.title.y = element_text(vjust = 0.5, hjust=0.3, angle = 0))
  
  return(ME_R0_plot)
  
} # plot in ME region 



################################################################################
# Functions for sensitivity analysis of the effect of temperature on R0 
################################################################################

temp_param_sens <- function(Temp) {
  
  a <- briere(Temp, parm = c(
    temp_params[1,1], 
    temp_params[2,1], 
    temp_params[3,1]))  #biting rate
  
  lf <- quad(Temp, parm = c(
    temp_params[1,2], 
    temp_params[2,2], 
    temp_params[3,2])) # mortality 
  
  lf <- ifelse( lf<=0, 1, lf ) # restrict to positive 
  
  
  PDR <- briere(Temp, parm = c(
    temp_params[1,3], 
    temp_params[2,3], 
    temp_params[3,3])) #parasite development 
  
  PDR[PDR<0] = 0.001
  EIP <- 1/PDR  #restict to positive 
  
  
  
  temp_variables <- data.frame(a, lf, EIP)
  
  temp_sen <- cbind(Temp, temp_variables)
  
  temp_sen$prop_Surv <- exp(-temp_sen$EIP / temp_sen $lf )  # include proportion surviving 
  
  
  return(temp_sen)
} # Estimate temp sensitivity parameters


est_R0_HV_sen <- function(temp, n,  temp_variables) { 
  
  TV <- temp_variables  #temp variable can be mean, min, max 
  
  mosq_person <- 0.49 # select location specific mosq / person (SEA or Middle East)
  
  biting_rate <- TV$a[TV$temp_range == temp]  # select airport temp specific biting rate 
  
  
  B_HV <- human_vec_transmission(n)  # draw from distribution of effectve human to vector transmission 
  
  prob_surv <- TV$prop_Surv[TV$temp_range == temp]  # select airport temp specific prob surv of EIP 
  
  DI <- infectious(n)  # draw from distribution for infectiousness 
  
  
  R0_HV <- mosq_person * biting_rate * prob_surv * B_HV * DI  # multiply all parameters 
  
  return(R0_HV)
}  # Estimate R0 HV


est_R0_VH_sen <- function(temp, temp_variables) {
  
  TV <- temp_variables #temp variable can be mean, min, max 
  
  biting_rate <- TV$a[TV$temp_range == temp]# select location specific mosq / person (SEA or Middle East)
  
  
  B_VH <- 0.25  # effective transmission from vector to human set at 0.25 following "Global Risk and Elimination of Yellow Fever Epidemics"
  

  
  lf <- TV$lf[TV$temp_range == temp]  # select airport temp specific longevity 
  
  
  R0 <- biting_rate * lf* B_VH  # multiply all parameters 
  
  return(R0)
} # Estimate R0 VH 


est_R0_sen <- function(temp, nsim, temp_variables) {
  
  TV <- temp_variables
  
  temp <- temp
  
  n <- nsim
  
  
  R0_VH <- est_R0_VH_sen(temp, TV)  # estimate R0_VH
  
  R0_HV <- est_R0_HV_sen(temp, n, TV) # estimate R0_HV
  
  
  R0 <- R0_VH * R0_HV  # multiply together 
  
  
  mean <- mean(R0, na.rm = TRUE)   # calculate mean, 95 CI, quantile 
  quant <- quantile(R0, c(.025, .975), na.rm = TRUE)
 
  out <- data.frame( mean = mean, 
                     lower_CI = quant[1],
                     upper_CI = quant[2])
  #return data frame of variables 
  
  return(out)
  
  
} # Estimate R0


tidy_data <- function(x, name_col) { #tidy data
  
  x <- all_R0_estimates_sens
  name_col <- temp_range
  
  x [is.na(x )] <- 0
  write.csv(x, file = "x")
  read <- column_to_rownames(read.csv("x"), "X")

  colnames(read) <- name_col #name columns temperature   
  df <- as.data.frame(t(read))  # transpose 
  
  output <- df %>%  
    rownames_to_column(var = "Variable")
  

  
  return(output)
  
} # tidy output of data 


