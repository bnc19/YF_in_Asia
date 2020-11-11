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



tidy_R0 <- function(x) {
  
  x [is.na(x )] <- 0 # remove nas
  df <- as.data.frame(t(x))  # transpose


  output <- df %>%  
    rownames_to_column(var = "Airport")
  
  
  output[ ,-1] <- apply(output[ , -1], 2,            # Specify own function within apply to ensure columns are numeric 
                      function(x) as.numeric(as.character(x)))
  return(output)
  
} # tidy results 



################################################################################
# Function to organize R0 for all temperatures
################################################################################

sort_temp <- function(temperature, list_R0, airports_risk){

average_temperature <- temperature[, c(1,13)]
minimum_temperature <- temperature[, c(1,12)]
maximum_temperature <- temperature[, c(1,11)]


colnames(average_temperature) <- c("Airport", "temperature")
colnames(minimum_temperature) <- c("Airport", "temperature")
colnames(maximum_temperature) <- c("Airport", "temperature")

list_R0[[1]] <- left_join(list_R0[[1]], average_temperature)
list_R0[[2]] <-left_join(list_R0[[2]], minimum_temperature)
list_R0[[3]] <-left_join(list_R0[[3]], maximum_temperature)

list_R0[[1]]$`Temperature (degree Celsius)` <- "Average"
list_R0[[2]]$`Temperature (degree Celsius)` <- "Minimum"
list_R0[[3]]$`Temperature (degree Celsius)` <- "Maximum"

all_R0_values <- bind_rows(list_R0)
city_airport <- airports_risk[, c(1,6)]

all_R0_values <- left_join(all_R0_values,city_airport)


return(all_R0_values)}


