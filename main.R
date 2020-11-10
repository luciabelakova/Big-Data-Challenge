# ====================== Description and R version ============================
#
# This is a project where I analyze daily weather data for Squamish, Pemberton
# and North Vancouver.
# 
R.version.string
#   "R version 3.6.3 (2020-02-29)"
#
# =============================================================================
# NOTES
# * In this program, I study the relationship between lock down policy and the
#   spread of Covid-19.
#
#
# =============================================================================
# Global Vars

# Set the project's directory as a global variable so that it can be accessed 
# easily by other scripts->improves folder management
main.dir <- getwd() 

# =============================================================================
# Libraries

# install libraries needed for the project
# install.packages("utils")

# Load the important packages into the workspace
library(utils)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)


# ======================= STRUCTURING THE FOLDERS =============================

# store names of the folders in a vector
folders.vector <- c("raw.covid.data","cleaned.covid.data",
                    "figures.covid.data", "results.covid.data")
# and make the folders if they don't exit yet. No need to understand this now
for(i in 1:length(folders.vector)){ 
  if(file.exists(folders.vector[i]) == FALSE){
    dir.create(folders.vector[i])
  } 
}

# determine the paths to the created folders
path.raw <- paste(main.dir, "/", folders.vector[1], "/", sep = "")
path.cleaned <- paste(main.dir, "/", folders.vector[2], "/", sep = "")
path.figures <- paste(main.dir, "/", folders.vector[3], "/", sep = "")
path.results <- paste(main.dir, "/", folders.vector[4], "/", sep = "")


# =============================================================================
# Run the other scripts for the program

# Run the file that handles the all the statistical analyses on the data
# Run this here 
# source("Analysis.R")

#==============================================================================
# File for Analysis using cases
# Save the data locally into a csv file
# Give 2-3 minutes for the data to load
d.t <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                 na.strings = "", fileEncoding = "UTF-8-BOM")

# Write the downloaded data to a csv file locally
write.csv(d.t, paste(path.raw,"raw.data.csv", sep = ""), 
          row.names = FALSE)

# File for analysis using death rates
# In this Project we have obtained 2 data sets which will be analyzed
# To begin we will load these files into R
# The first file has a bunch of data related to covid 19 with daily intervals.
# Opening the mortality data file located in the working directory
# It is called This is the one_Mortality_Please be clean
mort.d.t <- read.csv(paste(path.raw,"MortalityData.csv", sep = ""))

# Now we will load the second file This file has the population of all
# the countries of the world
# Opening the population data file located in the working directory called: 
# World_Population_Clean.csv
pop.d.t <- read.csv(paste(path.raw,"PopulationData.csv", sep = ""))

#================================= END ========================================
