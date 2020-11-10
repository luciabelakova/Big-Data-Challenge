#==============================================================================
# This script will clean up our raw data
# We will extract only the column we need and 
# then remove zeros and NA s

# Clean-up for cases data
#read the Dataset sheet into “R”. The dataset will be called "d.t.raw"
d.t.raw <- read.csv(paste(path.raw,"raw.data.csv", sep = ""), 
                   stringsAsFactors = FALSE, strip.white = TRUE)

# Convert the data frame to a tibble for us to view it easily
# through the console
d.t = as_tibble(d.t.raw)

# Convert the dates to legal R dates
# This will help with the further operations on this column
d.t$dateRep = dmy(d.t$dateRep)

# Now the data frame is huge. Most of the columns we won't even
# use, so we will select only the columns we need
# and create a data structure from those ones.
# Create a new file that will keep cases, dates and countries

# Extract the dates from the main data file
date = d.t$dateRep

# Extract the cases data data from the main data file
cases = d.t$cases

# Extract the countries data
countryy = d.t$countriesAndTerritories

# Create a new data frame containing only the dates, cases, and countries
casesData = data.frame(date, cases, countryy)

# Remove zeros and NA s
casesData[casesData == 0] = NA # convert zeros into NA
casesData.no.na = na.omit(casesData)   # remove NA

# Convert the dates in data structure into legal dates in R
casesData.clean = casesData.no.na[order(as.Date(casesData.no.na$date, 
                                                format="%y/%m/%d")),]

# Write the clean data on to a csv file
# This will help create a local copy that other methods can access
# or when we do not have internet access.
write.csv(casesData.clean, paste(path.cleaned,"casesData.csv", sep = ""), 
          row.names = FALSE) 

#==============================================================================
# CLEANING FOR DEATH RATE DATA

# We will now create a local data frame of the data sets
# The purpose of this is that when you use the print function it much cleaner 
# We will start by doing this for mortality
CovidMortality <- tibble::as_tibble(mort.d.t)

# Now we will do the same process as above for the populations
CountryPopulation <- tibble::as_tibble(pop.d.t)

# Now we will create a chard which takes the sum of all of the death of 
# each country makes that into a table corresponding to the country.
CovidMortality %>%
  group_by(ï..Country.Region) %>%
  summarise(sum(Deaths))

# Now we will will take this new table and set it to ALLSumofDeath
ALLSumofDeath <- CovidMortality %>%
  group_by(ï..Country.Region) %>%
  summarise(sum(Deaths))

#--------------------Altering Data Tables for Relevant Information--------------
# Now we will extract the countries that we are looking at to get a new table 
# that we can analyze 
# We got the corresponding row number to extract those rows.
# The rows of interest are: 
# 80, 151, 86, 122, 155, 121, 33, 135, 10, 177, 106, 47, 
# 162, 88, 156, 165, 166, 24
SpecifiedSumofDeath <- ALLSumofDeath[c(80, 151, 86, 122, 155, 121, 33, 135, 
                                       10, 177, 106, 47, 162, 88, 
                                       156, 165, 166, 24), ]
# The rows of interest are:
# 2, 114, 23, 126, 25, 69, 39, 38, 97, 21, 45, 86, 91, 11, 28, 57, 95, 6.
# We will set the new dataset to SpecifiedPopulation.
SpecifiedPopulation <- CountryPopulation[c(2, 114, 23, 126, 25, 69, 39,
                                           38, 97, 21, 45, 86, 91, 11, 
                                           28, 57, 95, 6), ]

#Testing to make sure the length of both Specified death and
# Specified Population is the same
length(SpecifiedSumofDeath$ï..Country.Region)
length(SpecifiedPopulation$ï..Country..or.dependency.)

# Now the next thing we want to do is divide the death by the population number
# But first we have to sort both of the table to ensure each country is
# multiplied with the right one (the same one)
# We will start with mortality
sort(SpecifiedSumofDeath$ï..Country.Region)
# Now we will do population
sort(SpecifiedPopulation$ï..Country..or.dependency.)

# Now they both sorted alphabetically, and since the number of countries is the
# same the order of countries will be the same for both.

# Now we will divide the sum of the mortality by the total population 
# of the countries
SpecifiedSumofDeath$`sum(Deaths)`/SpecifiedPopulation$Population..2020.

# We will set this new ratio to RatioofDeathtoPopulation
RatioofDeathtoPopulation <- (SpecifiedSumofDeath$`sum(Deaths)`/
                               SpecifiedPopulation$Population..2020.)

# Looking at the data to get an idea about how the data is looking
RatioofDeathtoPopulation

# Creating a vector of all the countries to make a new table
# This vector is needed to show the corresponding countries to the data
CountriesofInterest <- c("India", "Singapore", "Italy", "New Zealand", 
                         "South Africa", "Netherlands", "Canada", "Poland", 
                         "Austria", "United Kingdom", "Malaysia", "Czechia", 
                         "Sweden", "Japan", "South Korea", "Taiwan", 
                         "Tajikstan", "Brazil")

# Creating a vector of the categories of lockdown level of the countries
LockdownLevel <- c( "Strict", "Strict", "Strict", "Strict", "Strict", "Strict",
                    "Moderate", "Moderate", "Moderate", "Moderate", "Moderate",
                    "Moderate", "None","None","None","None","None","None" )

# Now we created a dataset with the countries and the ratio of 
# the deathtopopulation and the level of lockdwon they went under
data.frame(CountriesofInterest, RatioofDeathtoPopulation, LockdownLevel)

# Now we set the new ratio table to "Deathtopopulation"
DeathtoPopulation <- data.frame(CountriesofInterest, RatioofDeathtoPopulation, 
                                LockdownLevel)

# Taking a  look at the new table to ensure it looks good
head(DeathtoPopulation)

# We will write this new data set into a csv file and it will go into the wd
write.csv(DeathtoPopulation, paste(path.cleaned,"DeathtoPopulation.csv", sep = ""), 
          row.names = FALSE)




