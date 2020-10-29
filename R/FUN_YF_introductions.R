################################################################################
################################################################################
# Functions to quanitfy the risk of yellow fever case introductions into Asia 
################################################################################
################################################################################



################################################################################
# Function to remove airports which have unidirectional travel as epiflows
# requires same length of importations and exportations 
# (e.g. One passanger flew North Korea to Brazil but no passangers flew Brazil 
# to North Korea)
################################################################################

'%!in%' <- function(x,y)!('%in%'(x,y))  # create an operator for object not present in both 

remove_unequal_flows <- function(x) {
  
  discard_from <- which(x$to %!in%  x$from)  # select airports that are origins but not destinations
  discard_to <- which(x$from %!in%  x$to)   # select airports that are destinations but not origins
  
flow <-   x[ -c( discard_from, discard_to), ]  # drop selected airports from flows 

ifelse(dim(flow)[1] < 1, return(x), return(flow) )  # if already equal, return original

}


################################################################################
# Function to filter country and  remove unequal flows 
################################################################################

tidy_epiflows <- function(x, country) {
  
 endem_country <- filter (x, from == country | to == country) 
  
  
even_flows <-  remove_unequal_flows(endem_country)
  
return(even_flows)
  
}


################################################################################
# Functions to randomly draw from the incubation and infectious period
# distributions, and the duration of stay
################################################################################

incubation <- function(n) { rlnorm(n, 1.46, 0.35) }
infectious <- function(n) {  rnorm(n, 4.5, 1.5/1.96) }
stay <- function(n, mean){  rlnorm(n, mean, 2)}


################################################################################
# Function to select Asian countries at risk of at least one introduction 
################################################################################

select_at_risk <- function(x){


country_results_id <- lapply(x, function(x){rownames_to_column(x, var = "country")}) # make the country id a column 

all_country_results <- bind_rows(country_results_id) #convert list of results into single DF

#group by country and aggregate 
total_importations <- aggregate(all_country_results[ ,2:4], by = list(all_country_results$country), FUN= sum) 

# round cases to 2 DP
total_importations_round <- total_importations %>% 
  mutate( mean_cases = round(mean_cases, 2)) %>% 
  mutate(lower_limit_95CI = round(lower_limit_95CI, 2)) %>% 
  mutate( upper_limit_95CI = round(upper_limit_95CI, 2))

# find those with a upper limit greater tha 1 case 
count <- which(total_importations_round$upper_limit_95CI > 1)

countrys_at_risk <- total_importations_round[c(count), ]

return(countrys_at_risk)

}

################################################################################
# Function to plot airports at risk of YF introductions
################################################################################

plot_airport <- function (SEA_airport,ME_airport) {
  SEA_risk <- rename(SEA_airport, "South and East Asian Countries"  = Asian_countries)
  ME_risk <- rename(ME_airport, "West Asia and Middle Eastern Countries"  = Asian_countries)
  
  # countries in south and east asia 
  
  SEA_risk_plot <- ggplot(SEA_risk, aes(y = mean_cases, x = Airport)) +
    geom_point(size = 2, aes(colour= `South and East Asian Countries`)) +
    geom_errorbar(aes(ymin = lower_limit_95CI, ymax = upper_limit_95CI, colour=`South and East Asian Countries`), width = .25) + 
    xlab(" ") + 
    ylab(" ") + 
    theme_hc(base_size = 14) + 
    scale_color_npg() + 
    scale_y_continuous(limits=c(0,45),breaks=seq(0,40,5))+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
  
  
  
  #countries in middle east and west asia 
  
  ME_risk_plot <- ggplot(ME_risk, aes(y = mean_cases, x = Airport)) +
    geom_point(size = 2, aes(colour=`West Asia and Middle Eastern Countries`)) +
    geom_errorbar(aes(ymin = lower_limit_95CI, ymax = upper_limit_95CI, colour=`West Asia and Middle Eastern Countries`), width = .25) +
    xlab(" ") + 
    ylab(" ") + 
    theme_hc(base_size = 14) + 
    scale_color_npg() + 
    scale_y_continuous(limits=c(0,45), breaks=seq(0,40,5)) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
  
  
  
  #combine using cowplot
  
  airport_plots <- plot_grid(SEA_risk_plot, ME_risk_plot, ncol = 1, align = "h", labels = c("A", "B"),  label_size = 20)
  
  
  out <- annotate_figure(airport_plots , left = text_grob("Estimated number of introductions", rot = 90, size = 18),
                         bottom = text_grob("Airport", size = 18, vjust = 0))
  
  return(out)
  
                         
                        }


