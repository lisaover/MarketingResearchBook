# setwd("~/github/marketingresearchbook")
# source("Relationships Between Continuous Var.R")

simRetailerData <- function(ncust=1000) {
    ## 'ncust' number of customers to simulate - default 1000
    ## returns a data frame of simulated customer data
    ## chapter 4 "Relationships Between Continuous Variables"
    
    ## Simulating Customer Data ##
    set.seed(21821)
    cust.df <- data.frame(cust.id=as.factor(c(1:ncust)))
    
    # customer age is simulated from a normal distribution X ~ N(35, 25)
    cust.df$age <- rnorm(n=ncust, mean=35, sd=5)
    # customer credit score is simulated from a normal distribution with a mean
    # that is a function of the customer age so older customers have better scores
    # X ~ N((3*age+620), 2500)
    cust.df$credit.score <- rnorm(n=ncust, mean=3*cust.df$age+620, sd=50)
    # customer email - whether they have regsitered an email - is simulated
    # by sampling from ("yes", "no") with 80% chance of "yes" and 20% chance of "no"
    cust.df$email <- factor(sample(c("yes", "no"), size=ncust, replace=TRUE, prob=c(0.8, 0.2)))
    # customer distance to the store is simulated as lognormal ln(X) ~ N(2, 1.44)
    cust.df$distance.to.store <- exp(rnorm(n=ncust, mean=2, sd=1.2))
    
    ## Simulating Sales Data - online and in-store ##
    
    # number of online visits data is siumlated by the negative binomial distribution 
    # customers without a registered email are started with a base of 15 visits  
    # from the base, a calculated number of visits are 'added' or 'subtracted'*
    # based on the customer's age relative to the median age of the sample
    # customers with a registered email are started with a base of 15 online visits
    # from the base, an additional 15 visits are added and a calculated number of visits
    # are 'added' or 'subtracted'* based on the customer's age relative to the median 
    # age of the sample
    # in the negative binomial distribution, only a few customers make many visits
    # the majority of customers make only a few - the age component assures that
    # younger customers make more online visits - *the product 
    # (-0.7 times age minus median) will be negative for younger customers, 
    # which makes the product positive resulting in an addition of visits
    # the size argument - 0.3 - sets the degree of variation for the samples
    cust.df$online.visits <- rnbinom(ncust, size=0.3, 
                                     mu = 15 + ifelse(cust.df$email == "yes,", 15, 0) 
                                     - 0.7 * (cust.df$age-median(cust.df$age)))
    # whether a customer places an order on any visit is simulated using the binomial
    # distribution with the the number of trials as the number of online visits
    # and the chance of placing an order - 30%
    # amounts spent are simulated with the lognormal distribution
    cust.df$online.trans <- rbinom(ncust, size=cust.df$online.visits, prob=0.3)
    cust.df$online.spend <- exp(rnorm(ncust, mean=3, sd=0.1)) * cust.df$online.trans
    
    # customer purchases in the store are simulated without keeping track of the 
    # number of visits made to the store - a number companies can't really track
    # the number of store purchases is simulated with the negative binomial distribution
    # fewer purchases are made by customers who live far away so the base mean of 3
    # is adjusted by dividing it by the square root of the distance to the store
    cust.df$store.trans <- rnbinom(ncust, size=5, mu=3 / sqrt(cust.df$distance.to.store))
    cust.df$store.spend <- exp(rnorm(ncust, mean=3.5, sd=0.4)) * cust.df$store.trans
    
    ## Simulating Satisfaction Survey Respones ##
    
    # start with an overall satisfaction with the brand - unobserved 'halo'
    # add this overall satisfaction to a randomly generated specific level of 
    # satsifaction for two separate rating categories - service and product selection
    # the specific ratings are simulated with a normal distribution and, to transform
    # the rating so it is on a discrete, ordinal scale, the floor() function is used
    sat.overall <- rnorm(ncust, mean=3.1, sd=0.7)
    sat.service <- floor(sat.overall + rnorm(ncust, mean=0.5, sd=0.4))
    sat.selection <- floor(sat.overall + rnorm(ncust, mean=0.2, sd=0.6))
    # for rating values less than 1 - set them to 1
    # forrating values greater than 5 - set them to 5
    sat.service[sat.service < 1] <- 1
    sat.service[sat.service > 5] <- 5
    sat.selection[sat.service < 1] <- 1
    sat.selection[sat.service > 5] <- 5
    
    ## Simulating Non-Response Data ##
    
    # some customer response data is removed to simulate non-response
    # the binomial distribution is used to simulate a vector of TRUE and FALSE values
    # which will be used to remove corresponding responses in the service and
    # selection vectors - the probability of no response is a function of age
    # with older customers being more likely not to respond
    no.response <- as.logical(rbinom(ncust, size=1, prob=cust.df$age/100))
    sat.service[no.response] <- NA
    sat.selection[no.response] <- NA
    # add sat.service and sat.selection to cust.df
    cust.df$sat.service <- sat.service
    cust.df$sat.selection <- sat.selection
    
    cust.df
}

# cust.df <- simRetailerData()
# summary(cust.df)
# str(cust.df)