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


################################################################################
# Functions for estimating biting rate, lifespan and extrinsic incubaiton
# period from temperature 
################################################################################

temp_param <- function(Temp) {
  
  Temp <-  ifelse(Temp < 0, 1, Temp)
  
  a <- briere(Temp, parm = c(
    temp_params[1,1], 
    temp_params[2,1], 
    temp_params[3,1]))  #biting rate
  a[a<0] = 0.001
  

  PDR <- briere(Temp, parm = c(
    temp_params[1,2], 
    temp_params[2,2], 
    temp_params[3,2])) #parasite development 
  
  PDR[PDR<0] = 0.001
  EIP <- 1/PDR  #restict to positive 
  
  
  a_AL <- briere(Temp, parm = c(
    temp_params[1,3], 
    temp_params[2,3], 
    temp_params[3,3]))  #biting rate
  a_AL[a_AL<0] = 0.001
  
  temp_variables <- data.frame(temperature$airport, temperature$country, a, EIP, a_AL)

  
  return(temp_variables)
}

get_lifespan <- function(survival_data, temperature_data){
  
  closest_temp <- sapply(temperature_data, function(x){which.min(abs(survival_data$Temperature - x) )})
  
  lifespan <- survival_data$Mean[closest_temp]
  return(lifespan)} 



################################################################################
# Functions to select cities with mosquito population
################################################################################

# Select cities with AA population

select_cities <- function (mosq_per_person, airports_risk, temp_variable){
  
  mosq <- left_join(mosq_per_person, airports_risk[, c(1,6)])
  mosq_cc <- mosq[complete.cases(mosq[,2]), ]
  
  out <- left_join(mosq_cc, temp_variable, 
                   by = c("Airport" = "temperature.airport"))
  
  if(dim(mosq_cc)[1] == 11) out$species <- "AA" else out$species <- "AL"

  return(out)
}

################################################################################
# Functions to estimate R0 
################################################################################

infectious <- function(n) {  rnorm(n, 4.5, 1.5/1.96) } # randomly draw from the infectious period distribution

# human_vec_transmission <- function(n) {rtri(n, min = 0, max = 1, mode = 0.8)} # randomly draw from the infectious the effective human vector transmission rate
 
est_R0_HV <- function(n,  temp_variables, B_HV) {   # estimate R0_HV
  
  TV <- temp_variables #temp variable can be mean, min, max 
  
  
  mosq_person_AA <- TV$AA # select AA per person
  mosq_person_AL <- TV$AL # select AA per person
  
  mosq_person <- if(TV$species == "AA") c( mosq_person_AA) else c( mosq_person_AL)
  
  biting_rate_AA <- TV$a # select biting rate 
  biting_rate_AL <- TV$a_AL # select biting rate 
  biting_rate <- if(TV$species == "AA") c( biting_rate_AA) else c( biting_rate_AL)
  
  
  prob_surv <- TV$prop_Surv # select prob surv of EIP 
  
  DI <- infectious(n)  # draw from distribution for infectiousness 
  
  R0_HV <- mosq_person * biting_rate * prob_surv * B_HV * DI  # multiply all parameters 
  
  
  return(R0_HV)
}  # estimate R0 HV

est_R0_VH <- function(temp_variables, B_VH) { # estimate R0_VH
  
  TV <- temp_variables#temp variable can be mean, min, max 
  
  
  biting_rate_AA <- TV$a # select biting rate 
  biting_rate_AL <- TV$a_AL # select biting rate 
  biting_rate <- if (TV$species == "AA")  c(biting_rate_AA) else c(biting_rate_AL)
  
  
  
  lf <- TV$lf  # select longevity 
  
  
  R0 <- biting_rate * lf* B_VH  # multiply all parameters 
  
  out <- data.frame(
    Airport = temp_variables$Airport,
    R0_VH_mean = R0
  )

  return(out)
} # estimate R0 VH

est_R0_HV_mean <- function(n, temp_variables, B_HV){ #estimate the mean and 95 CI of R0_HV
  
  TV <- temp_variables
  n <- n

  
  R0_HV <- est_R0_HV(n, TV, B_HV)
  
  
  mean_HV <- mean(R0_HV, na.rm = TRUE)   # calculate mean, 95 CI, quantile of R0_VH
  quant_HV <- quantile(R0_HV, c(.025, .975), na.rm = TRUE)
  sd_HV <- sd(R0_HV)
  
  
  out_HV <- data.frame( 
    R0_HV_mean  = mean_HV, 
    R0_HV_lower_CI  = quant_HV[1],
    R0_HV_upper_CI      = quant_HV[2], 
    R0_HV_sd = sd_HV) 
  
  return(out_HV)} #estimate mean of HV from sampling 

est_R0 <- function(n, temp_variables, B_VH, B_HV) { # estimate mena and 95% CI R0
  
  TV <- temp_variables
  
  
  R0_VH <- est_R0_VH(TV, B_VH)  # estimate R0_VH
  
  R0_HV <- est_R0_HV(n, TV, B_HV) # estimate R0_HV
  
  
  R0 <- R0_VH$R0_VH_mean * R0_HV  # multiply together 
  
  
  mean <- mean(R0, na.rm = TRUE)   # calculate mean, 95 CI, quantile 
  quant <- quantile(R0, c(.025, .975), na.rm = TRUE)
  sd <- sd(R0)
  
  out <- data.frame( 
                     Airport = TV$Airport,
                     mean = mean, 
                     lower_CI = quant[1],
                     upper_CI = quant[2], 
                     sd = sd)
  #return data frame of variables 
  
  return(out)
  
  
}  # estimate R0 



tidy_R0 <- function(x) {
  
  x [is.na(x )] <- 0 # remove nas
  output <- as.data.frame(t(x))  # transpose

  output$Airport <- as.character(output$Airport)
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


