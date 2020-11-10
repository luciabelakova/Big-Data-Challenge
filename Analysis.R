# This data is a bit tricky to analyze. I will set up some tests for the means
# of the probability of finding an infected or once infected individual (p)
# the different populations. One way to think about a measure for the 
# effectiveness of lockdown policies is to compare them to p, the probability 
# of finding an infected or a once infected individual in a country. If 
# if lockdown policies were efficient, we'd expect a lower probability of
# finding an infected or once infected individual.

# Definition:
#             p <- Total Cases to-date/population of country

# Population of country will most likely be huge compared to total cases to-date.
# We will need a way to scale-up p. We will take the negative log (bases 10)
# of the probabilities, to get a new variable. We will call this variable the 
# risk index (RI) of a country.

# For example, Canada has a population of 37.59 million. We can compute its RI as
# follows:
RI.canada <- -log10(sum(separated.dt$Canada$cases)/(37.59 * 10^6))

# This gives an RI values of 2.167157
# Note: RI values inversely proportional to the risk

# Define a function to convert into RI values
f.convert2.RI <- function(SUM_CASES, POPULATION){
  RI.vector <- -log10(SUM_CASES/(POPULATION * 10^6)) # Population in millions
}


# No lockdown countries 
# This is a vector that gives me all the total cases for no lockdown countries
# Sweden | Japan | South Korea | Taiwan | Tajikistan | Brazil
total.cases.noL <- c(sum(separated.dt$Sweden$cases), sum(separated.dt$Japan$cases),
                    sum(separated.dt$South_Korea$cases), sum(separated.dt$Taiwan$cases),
                    sum(separated.dt$Tajikistan$cases), sum(separated.dt$Brazil$cases))

# 146461 , 105914,  27284,   573,  11256, 5590025

# vector to keep the country populations in millions  in their correct order
# Values from google
populations.noL <- c(10.23, 126.5, 51.3, 23.8, 9.53, 213.0)

# The RI values
RI.noL = f.convert2.RI(total.cases.noL, populations.noL)

# Strict Lockdown countries
# This is a vector that gives me all the total cases for strict lockdown countries
# India | Singapore | Italy | New Zealand | South Africa | Netherlands
total.cases.strictL <- c(sum(separated.dt$India$cases), sum(separated.dt$Singapore$cases),
                     sum(separated.dt$Italy$cases), sum(separated.dt$New_Zealand$cases),
                     sum(separated.dt$South_Africa$cases), sum(separated.dt$Netherlands$cases))

# vector to keep the country populations in millions  in their correct order
# Values from google
populations.strictL <- c(1380, 5.85, 60.5, 5.00, 59.62, 17.00)

# The RI values
RI.strictL = f.convert2.RI(total.cases.strictL, populations.strictL)

# Intermediate lockdown countries
# This is a vector that gives me all the total cases for intermediate lockdown countries
# Canada | Poland | Australia | The UK | Malaysia | Czech Republic
total.cases.intermediateL <- c(sum(separated.dt$Canada$cases), sum(separated.dt$Poland$cases),
                         sum(separated.dt$Australia$cases), sum(separated.dt$United_Kingdom$cases),
                         sum(separated.dt$Malaysia$cases), sum(separated.dt$Czechia$cases))

# vector to keep the country populations in millions  in their correct order
# Values from google
populations.intermediateL <- c(37.74, 37.85, 25.50, 67.89, 32.7, 10.71)

# The RI values
RI.interL <- f.convert2.RI(total.cases.intermediateL, populations.intermediateL)

# Create a data structure from the RI value.
# the RI values
RI.value <- c(RI.noL, RI.interL, RI.strictL)

# The different groups/lockdown policies in their correct order
lockdown.policy <- c("None/Minimal", "None/Minimal", "None/Minimal", "None/Minimal",
                     "None/Minimal", "None/Minimal", "Intermediate", "Intermediate",
                     "Intermediate", "Intermediate", "Intermediate", "Intermediate",
                     "Strict/Full", "Strict/Full", "Strict/Full", "Strict/Full",
                     "Strict/Full", "Strict/Full")

# Now create the data frame
RI.data <- data.frame(lockdown.policy, RI.value)

# Write this data on to a csv file into the clean data folder
write.csv(RI.data, paste(path.cleaned,"RIdata.csv", sep = ""), 
          row.names = FALSE)


# One way ANOVA
# NULL HYPOTHESIS: All mean RI values are equal
# ALTERNATIVE: At least the mean of one RI group is different


# Apply the one-way ANOVA in R
ri.aov = aov(RI.value ~ lockdown.policy, data = RI.data)

# View the results if the ANOVA
summary(ri.aov)

# R Output

#                  Df  um Sq Mean Sq  F value Pr(>F)
# lockdown.policy  2  1.963  0.9815   1.441  0.268
# Residuals       15 10.218  0.6812 

# Reject the null hypothesis since the f-value is greater than 1.
# Since we the result is significant, we now apply the post hoc
# Turkey Cramer test

# Turkey HSD test as a post hoc test
ri.tk <- TukeyHSD(ri.aov)
#                                diff        lwr       upr     p adj
# None/Minimal-Intermediate  0.695874240 -0.5418803 1.9336288 0.3369468
# Strict/Full-Intermediate  -0.009206929 -1.2469615 1.2285476 0.9997942
# Strict/Full-None/Minimal  -0.705081169 -1.9428357 0.5326734 0.3280284

#==================================================================================
# ANALYSIS FOR DEATH RATES
#-----------------------------Testing the Data----------------------------------

# We are now going to use the aggregate function to see the means of all the 
# different ratios for the various countries that went under lockdown or didn't.
aggregate( DeathtoPopulation$RatioofDeathtoPopulation ~ LockdownLevel, 
           DeathtoPopulation, mean)

# We are now going to use the aggregate function to see the lists of all the 
# different ratios for the various countries that went under lockdown or didn't.
aggregate( DeathtoPopulation$RatioofDeathtoPopulation ~ LockdownLevel,
           DeathtoPopulation, list) 

# We are now going to use the aggregate function to see the standard deviation
# of all the different ratios for the various countries that went under lockdown 
# or didn't.
aggregate( DeathtoPopulation$RatioofDeathtoPopulation ~ LockdownLevel, 
           DeathtoPopulation, sd)

#====================================================================================
# ANALYSIS FOR DEATH RATE DATA

#-----------------------------Analysis and Test---------------------------------

# Conducting an Anova test to compare the mean Ratios to see whether 
# or not it is significant or not.

# Null Hypothesis: Going under lock down has no effect on your mortality to 
# population ratio

# Alternative Hypothesis: Going under lock down has an effect on your mortality 
# to population ratio

# We will now conduct the Anova test
res.aov <- aov(RatioofDeathtoPopulation ~ LockdownLevel, data = DeathtoPopulation)

# Seeing what we got
res.aov
# Df   Sum Sq  Mean Sq F value Pr(>F)
# LockdownLevel  2 0.000126 6.31e-05   0.127  0.881
# Residuals     15 0.007441 4.96e-04               
# > res.aov
# Call:
# aov(formula = RatioofDeathtoPopulation ~ LockdownLevel, data = 
#                                                    DeathtoPopulation)
# Terms:
#  LockdownLevel   Residuals
# Sum of Squares    0.000126202 0.007440744
# Deg. of Freedom             2          15
# Residual standard error: 0.02227217
# Estimated effects may be unbalanced

# We will now look at the results to see if it is significant or not.
summary(res.aov)


















