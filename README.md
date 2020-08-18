
# Repository structure

This repository contains data and code pertaining to The Risk Analysis of Yellow Fever in Asia. 
Specifically, it contains: 

* The data and code used to estimate the number of yellow fever case introductions from endemic countries 
into Asia. 

* The data and code used to estimate the temperature dependent R0 values for locations in Asia.

* The data and code used to estimate the probablity of autochthonous transmisssion for locations in Asia. 

* The data and code used to perform sensitivity analyses. 

* Details about the analyses 


## Data: 

The data folder contains: 

* asia_flows_country.csv: Cumulative number of passenger journeys made between countries endemic for yellow fever and countries in Asia during 2016, obtained from the International Air Transport Association.

* asia_flows_airport.csv: Cumulative number of passenger journeys made between countries endemic for yellow fever 
and airports in Asia during 2016, International Air Transport Association.

* asia_ec_locations_airport.csv: Data about each endemic country. For each endemic country there is: 
a location code, the population size, the average number of days international travellers stay for, the cumulative incidence of yellow 
fever cases during 2016 and the time window of cases (in this instance the entirety of 2016). The location code for 
each country and airport in Asia included in this analysis is also listed. 

* prov_asia_temp.csv: Ecological data for all provinces of interest in Asia. 

* temp.params.csv: Medians of parameter samples for thermal minimum (*T0*) and maximum (*Tm*) and 
positive rate constant (*c*) values for the mosquito traits: biting rate (*a*), lifespan (*lf*) and 
parasite development rate (*PDR*). 


Also included are data estimated as part of this project: 

* airports_risk.csv: The number of yellow fever case introductions predicted into Airports in Asia 

* avg_R0_estimates.csv: *R0* estimates for cities in Asia at their average temperature 

* min_R0_estimates.csv: *R0* estimates for cities in Asia at their minimum temperature 

* max_R0_estimates.csv: *R0* estimates for cities in Asia at their maxiumum temperature 

* temp_variables_mean.csv: Temperature dependent mosquito trait (*a*, *lf* and *PDR*) estimates for cities in Asia at
their average temperatures


## Scripts
The scripts folder contains the main scripts for each separate analysis

a. YF_introductions.R: Script for quantifying the risk of yellow fever case introductions in Asia

b. Temp_R0.R: Script for estimating temperature dependent *R0* values for locations in Asia 
at risk of yellow fever introductions 

c. Prob_auto_trans.R: script for estimating the probability of autochthonous transmission 
given the introduction of at least one yellow fever case and related sensitvitiy analyses. 


## R 


The R folder contains the a script of functions for each of the analysis scripts: 

a. FUN_YF_introductions.R

b. FUN_Temp_R0.R

c. FUN_Prob_auto_trans.R


## Details of the analysis 

The scripts provided are in chronological order of the work process, however each script will also run alone to produce each of the figures in the report. 


#### YF introductions

To quantify the risk of introduction of yellow fever, a mathematical model was applied to flight and epidemiological data from 2016, via the epiflows package. First we estimated the number of introductions predicted into countries in Asia during 2016. If the upper 95% confidence interval for the total number of introductions into an Asian country was greater than 1, we investigated the endemic source country and repeated the analysis at the airport level

#### Temperature dependent $R0$ estimates 

A temperature dependent model was used to estimate *R0* values for each city predicted to have at least one yellow fever introduction. For vector borne diseases like yellow fever, *R0* is the product of the average number of infectious mosquitos produced per infectious human (*R0<sup>HM</sup>*) and the average number of infectious humans produced per infectious mosquito (*R0<sup>MH</sup>*). 

The influence of temperature on the predicted range of *R0* was also modelled, with values also ascertained at each locations minimum and maximum temperature.

#### Probability of autochthonous transmission given at least one introduciton of yellow fever

We used a branching process model with a negative binomial distribution to estimate the offspring distribution at each generation.
This value can be drawn from negative binomial probability distributions with dispersion parameter *k* and means *R0<sup>MH</sup>* and *R0<sup>HM</sup>* respectively. The probability of autochthonous transmission, given the introduction of one infectious human, is therefore derived from the probability of infecting at least one mosquito and the infectious mosquito infecting at least one human.  

Sensitivity analyses on key assumptions about vector competence, population immunity and individual variation in infectiousness were carried out. 
