#==============================================================================
# Here we create all the visualizations in our project
# We start by creating all the time series plots and then the 
# box and whisker plots for the RI values 

# Load the Analysis.R file
# This will create the data needed to blot the box and whisker plots
source('Analysis.R')

# Load the data into R
cases.dt <- read.csv(paste(path.cleaned,"casesData.csv", sep = "")) 

# Make sure the data are grouped by country
by.country = cases.dt %>% group_by(country)

# Split the whole data set into a new data set containing data frames for 
# for each country
separated.dt = split(cases.dt, cases.dt$country, drop = F)

# Call the pdf command to start the plot
pdf(file = paste(path.figures,"cases.pdf", sep = ""),   # The directory you 
    # want to save the file in
    width = 10, # The width of the plot in inches
    height = 7) # The height of the plot in inches

#=================================================================================================
# --countries with intermediate lockdown
generate.colors1 = colorRampPalette(c("blue", "red"))(100)

colours1 = c(generate.colors1[1], generate.colors1[17], generate.colors1[34], 
            generate.colors1[69],generate.colors1[85], generate.colors1[99])


plot(NA, xlim = c(30,311), ylim = c(0,16), type = "l", xlab = "", ylab = "log(COVID-19 cases)", 
    main = "Intermediate Lockdown Policy Countries ",  xaxt = "n", yaxt = "n", bty = "n", las = 1)
box(lty = 'solid', col = 'black')
legend("topright", c("Canada", "Poland", "Czech Republic", "Australia", 
                     "United Kingdom", "Malaysia"), pch = c(19, 19, 19, 19, 19, 19),
       col = colours1)
axis(2, seq(0, 14, by=2), seq(0, 14, by=2))
axis(1, seq(0, 311, by = 26), c("Nov '19", "Jan '20", "Feb '20", "Mar '20", "Apr '20", "May '20",
                              "Jun '20", "Jul '20", "Aug '20", "Sep '20", "Oct '20", "Nov '20"), 
     las = 2, hadj = 1)

points(log(separated.dt$Canada$cases), type = "l", col = generate.colors1[1])
points(log(separated.dt$Poland$cases), type = "l", col = generate.colors1[17])
points(log(separated.dt$Czechia$cases), type = "l", col = generate.colors1[34])
points(log(separated.dt$Australia$cases), type = "l", col = generate.colors1[69])
points(log(separated.dt$United_Kingdom$cases), type = "l", col = generate.colors1[85])
points(log(separated.dt$Malaysia$cases), type = "l", col = generate.colors1[99])


#===================================================================================================
# --countries with stringent lockdown policies
generate.colors = colorRampPalette(c("purple", "green"))(100)
colours = c(generate.colors[1], generate.colors[17], generate.colors[34], 
            generate.colors[69],generate.colors[85], generate.colors[99])
# We want tp custo
plot(NA, xlim = c(30,311), ylim = c(0,16), type = "l", xlab = "", ylab = "log(COVID-19 cases)", 
     main = "Full Lockdown Policy Countries ",  xaxt = "n", yaxt = "n", bty = "n", las = 1)
box(lty = 'solid', col = 'black')
legend("topright", c("India", "Singapore", "Italy", "New Zealand", 
                     "South Africa", "Netherlands"), pch = c(19, 19, 19, 19, 19, 19),
       col = colours)
axis(2, seq(0, 14, by=2), seq(0, 14, by=2))
axis(1, seq(0, 311, by = 26), c("Nov '19", "Jan '20", "Feb '20", "Mar '20", "Apr '20", "May '20",
                                "Jun '20", "Jul '20", "Aug '20", "Sep '20", "Oct '20", "Nov '20"), 
     las = 2, hadj = 1)

points(log(separated.dt$India$cases), type = "l", col = generate.colors[1])
points(log(separated.dt$Singapore$cases), type = "l", col = generate.colors[17])
points(log(separated.dt$Italy$cases), type = "l", col = generate.colors[34])
points(log(separated.dt$New_Zealand$cases), type = "l", col = generate.colors[69])
points(log(separated.dt$South_Africa$cases), type = "l", col = generate.colors[85])
points(log(separated.dt$Netherlands$cases), type = "l", col = generate.colors[99])

#========================================================================================
#--Countries with no lockdown policy--

# Generate a vector containing the colors for our different plots
generate.colors2 = colorRampPalette(c("brown", "orange"))(100)

# Now select a few colors from the vector we created
colours2 = c(generate.colors[1], generate.colors[17], generate.colors[34], 
            generate.colors[69],generate.colors[85], generate.colors[99])

plot(NA, xlim = c(30,311), ylim = c(0,16), type = "l", xlab = "", ylab = "log(COVID-19 cases)", 
     main = "No Lockdown Policy Countries ",  xaxt = "n", yaxt = "n", bty = "n", las = 1)
box(lty = 'solid', col = 'black')
legend("topright", c("Sweden", "Japan", "South Korea", "Taiwan", 
                     "Tajikistan", "Brazil"), pch = c(19, 19, 19, 19, 19, 19),
       col = colours2)
axis(2, seq(0, 14, by=2), seq(0, 14, by=2))
axis(1, seq(0, 311, by = 26), c("Nov '19", "Jan '20", "Feb '20", "Mar '20", "Apr '20", "May '20",
                                "Jun '20", "Jul '20", "Aug '20", "Sep '20", "Oct '20", "Nov '20"), 
     las = 2, hadj = 1)

points(log(separated.dt$Sweden$cases), type = "l", col = generate.colors[1])
points(log(separated.dt$Japan$cases), type = "l", col = generate.colors[17])
points(log(separated.dt$South_Korea$cases), type = "l", col = generate.colors[34])
points(log(separated.dt$Taiwan$cases), type = "l", col = generate.colors[69])
points(log(separated.dt$Tajikistan$cases), type = "l", col = generate.colors[85])
points(log(separated.dt$Brazil$cases), type = "l", col = generate.colors[99])

#Create the pdf file
dev.off()

# Visualize the RI values using a boxplot

# Load the data into R

ri.dt = read.csv(paste(path.cleaned,"RIdata.csv", sep = ""))

# Call the pdf command to start the plot
pdf(file = paste(path.figures,"RIvalues.pdf", sep = ""),   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 7) # The height of the plot in inches

# Plot the box plot
colorsRI = colorRampPalette(c("orange", "light blue"))(100) # Get some colors for the plot
boxplot(RI.value ~ lockdown.policy, data = ri.dt, ylab = "RI value", 
        xlab = "Lockdown Policy Level", col =c(colorsRI[1], colorsRI[80], colorsRI[100]),
        main = "RI Values with Lockdown Policy")

#Create the pdf file
dev.off()

#================================================================================================================
# DATA VISUALIZATION FOR DEATH RATES
# Call the pdf command to start the plot
pdf(file = paste(path.figures,"AnalysisDeathRates.pdf", sep = ""),   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 7) # The height of the plot in inches

# plotting the Rato of death by countries of interest and sub setting the 
# lockdown level with colout using the qplot function in the ggplot package.
qplot(CountriesofInterest, RatioofDeathtoPopulation, data = DeathtoPopulation, 
      color = LockdownLevel) + theme(axis.text.x=element_text
                                     (angle=90, size=5, vjust=0.5)) 

# Now we will make 3 separate sections for each graph to see how they vary this
# is done using the facets function within qplot
qplot(CountriesofInterest, RatioofDeathtoPopulation, data = DeathtoPopulation, 
      facets =.~LockdownLevel) + theme(axis.text.x=element_text
                                       (angle=90, size=5, vjust=0.5)) + theme(
                                         axis.title.x = element_text
                                         (color="Black", vjust=-0.25),
                                         axis.title.y = 
                                           element_text(color="Black"
                                                        , vjust=0.25)) + labs(x="Countries",
                                                                              y=expression(paste("Ratio of Mortality to Population")), 
                                                                              title="") + stat_summary
(fun.data=mean_cl_normal) + geom_smooth(method='lm', formula= y~x)

dev.off()








