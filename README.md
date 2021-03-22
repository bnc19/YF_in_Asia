# Background

Concern is growing over the international spread of the vector-borne disease, yellow fever. Thus far, yellow fever transmission has never been recorded in Asia, despite the presence of mosquitos and suitable ecological conditions. Global shortages of the yellow fever vaccine mean that any outbreak in Asia could quickly become a major epidemic. As a result, this analysis aims to quantify the risk of yellow fever in Asia. We first predicted the number of yellow fever introductions into Asia through the arrival of viraemic travellers. From this, we estimated the probability that those infectious individuals would result in local transmission. 

# Repository structure

This repository contains data and code pertaining to The Risk Analysis of Yellow Fever in Asia. 
Specifically, it contains: 

* The data and code used to estimate the number of yellow fever case introductions from endemic countries 
into Asia. 

* The data and code used to estimate the temperature dependent R0 values for locations in Asia.

* The data and code used to estimate the probability of autochthonous transmission for locations in Asia. 

* The data and code used to perform sensitivity analyses. 

* Details about the analyses 


## Data: 

The data folder contains: 

* asia_flows_country.csv: Cumulative number of passenger journeys made between countries endemic for yellow fever and countries in Asia during 2016, obtained from the International Air Transport Association (IATA).

* asia_flows_airport.csv: Cumulative number of passenger journeys made between countries endemic for yellow fever 
and airports in Asia during 2016, obtained from the IATA.

* asia_ec_locations_airport.csv: Data about each endemic country. For each endemic country there is: 
a location code, the population size, the average number of days international travellers stay for, the cumulative incidence of yellow 
fever cases during 2016 and the time window of cases (in this instance the entirety of 2016). The location code for 
each country and airport in Asia included in this analysis is also listed. 

* prov_asia_temp.csv: Ecological data for all provinces of interest in Asia. 

* temp.params.csv: Medians of parameter samples for thermal minimum (*T0*) and maximum (*Tm*) and 
positive rate constant (*c*) values for the mosquito traits: biting rate (*a*),  and 
parasite development rate (*PDR*). 

* mean_survival_aegypti.csv: Data on the mean survival of *Ae. aegypti* in the field, modelled across a range of temperatures.

* mean_survival_albopictus.csv: Data on the mean survival of *Ae. albopictus* in the field, modelled across a range of temperatures.

* mosq_per_person.csv: Data on the number of female *Ae. albopictus* and *Ae. aegypti* per person in cities in Asia. 

Also included are data estimated as part of this project: 

* airports_risk.csv: The number of yellow fever case introductions predicted into Airports in Asia. 

* AA_avg_R0_estimates.csv: *R0* estimates for cities in Asia at their average temperature for *Ae. aegypti*.

* AA_max_R0_estimates.csv: *R0* estimates for cities in Asia at their maximum temperature for *Ae. aegypti*.

* AA_min_R0_estimates.csv: *R0* estimates for cities in Asia at their minimum temperature for *Ae. aegypti*.

* AL_avg_R0_estimates.csv: *R0* estimates for cities in Asia at their average temperature for *Ae. albopictus*.

* AL_max_R0_estimates.csv: *R0* estimates for cities in Asia at their maximum temperature for *Ae. albopictus*.

* AL_min_R0_estimates.csv: *R0* estimates for cities in Asia at their minium temperature for *Ae. albopictus*.

* AL_temp_variables_max.csv: Temperature dependent mosquito trait (*a* and *PDR*) estimates for cities in Asia at
their average temperatures for *Ae. albopictus*.

* AA_temp_variables_average.csv: Temperature dependent mosquito trait (*a* and *PDR*) estimates for cities in Asia at
their average temperatures for *Ae. aegypti*.


## Scripts
The scripts folder contains the main scripts for each separate analysis

a. YF_introductions.R: Script for quantifying the risk of yellow fever case introductions in Asia.

b. Temp_R0.R: Script for estimating temperature dependent *R0* values for locations in Asia 
at risk of yellow fever introductions with either an *Ae. aegypti* or *Ae. albopictus* mosquito population.


c. Prob_auto_trans.R: Script for estimating the probability of autochthonous transmission, 
given the introduction of at least one yellow fever case, and related sensitivity analyses. 


## R 


The R folder contains a script of functions for each of the analyses scripts: 

a. FUN_YF_introductions.R

b. FUN_Temp_R0.R

c. FUN_Prob_auto_trans.R


## Details of the analysis 

The scripts provided are in chronological order of the work process, however each script will also run alone to produce each of the figures in the report. 


#### YF introductions

To estimate the number of yellow fever case introduction into countries Asia, we applied a mathematical model to 2016 flight and yellow fever incidence data, via the Epiflows package. If the upper 95% confidence interval for the total number of introductions into an Asian country was greater than 1, we investigated the endemic source country and repeated the analysis at the airport level

#### Temperature dependent *R0* estimates 

A temperature dependent model was used to estimate *R0* values for each Asian city predicted to have at least one yellow fever introduction, with either an *Ae. aegypti* or *Ae. albopictus* mosquito population. As yellow fever is transmitted by mosquitos, *R0* is the product of the average number of infectious mosquitos produced per infectious human (*R0<sup>HM</sup>*) and the average number of infectious humans produced per infectious mosquito (*R0<sup>MH</sup>*). Therefore, *R0<sup>HM</sup>* and *R0<sup>MH</sup>* were estimated at the average temperature of each Asian city. 

Sensitivity analysis on the influence of temperature on the predicted range of *R0* was also modelled, with *R0* values also estimated for the temperature extremes of each Asian city.

#### Probability of autochthonous transmission given at least one introduction of yellow fever

We used a branching process model to estimate the probability of autochthonous transmission. The offspring distributions of humans and mosquitos can be drawn from negative binomial probability distributions with means *R0<sup>MH</sup>* and *R0<sup>HM</sup>* respectively, and the dispersion parameter *k*. The probability of autochthonous transmission, given the introduction of one infectious human, therefore depends on that infectious human infecting at least one mosquito and that infectious mosquito infecting at least one human.



Sensitivity analyses on key assumptions about vector competence, the number of mosquitos per person, population immunity and individual variation in infectiousness were carried out.
