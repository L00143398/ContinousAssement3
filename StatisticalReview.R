# The research question related to Crime in ireland was 
# "Is there a correlation between a rise in unemployment figures and the numbers of crimes recorded 
# due to the negative effects of the 2008 recession"
#
# To address this question I utilized Unemployment Claiment figures for Norther Ireland
# and the Police Recorded crime rate in Northern Ireland for the period 1998 through 2017

# A limitation on this data is that it is reported at a yearly level so this obviously
# limits that amount of statistical analysis that can be performed but it will show the yearly
# trend in unemployment figures and also the yearly crime figures and this data can be utilized
# to determine whether there is a significant correlation between the two sets of data


install.packages("dplyr")
install.packages("readxl")
library("readxl")
library(dplyr)
library("lattice")

# The first step is to read the Unemployment Claiment figures into the UnemploymentStats dataframe
# from a csv file.
# I also read the Police Recorded crime statistics from a worksheet in the excel file.

UnemploymentStats <- read.csv(file = "Unemployment_Claiment.csv", header=FALSE, na.strings=c("","NA"))
CrimeStats<-read_xls("Police_recorded_crime_in_northern_ireland_1998-99_to_2017-18.xls", 
                     sheet="Table 2.2")
AA_Crime <- read.csv(file = "AA_All_Crime.csv", header=FALSE, na.strings=c("","NA"))
AA_Unemployment <- read.csv(file = "AA_Unemployment_stats.csv", header=FALSE, na.strings=c("","NA"))

View(AA_Crime)
View(AA_Unemployment)


# Cleaning up the Unemployment claiment data but first removing the first lines 
# and then selecting the years from 1998 to 2018.   The reason I am starting from 1998 is twofold.
# The Norther Ireland peace agreement was signed in 1994 and that triggered the end 
# of what were called the "Troubles" and from that point onwards Norther Ireland moved more towards
# a normal economy where we see the unemployment rates go down 
# In addition the metrics that I have from the NI Crime Rates begins in 1998.
# The Crime statistics information only goes up to 2017 so although I do have unemployment figures for
# 2018 I am not including the 2018 statistics as my focus is on the years between 2007 and 2014

# The rececssion in 2008 gave a unique opportunity to test whether there is a signficant relationship between
# unemployment figures and crime rates because unlike general areas of high unemployment that are 
# also affected by high crime rates, the sudden spike in unemployment numbers along with the 
# cessation of the troubles in Northern Ireland give a unique opportunity to determine if there is a 
# correlation.

#  The following code cleans up the Unemployment data
UnemploymentStats <- UnemploymentStats[-c(1:35), ]
colnames(UnemploymentStats) <- c("Year", "EmploymentRate")
UnemploymentStats <- UnemploymentStats[1:20, ]
UnemploymentStats$Year <- factor(UnemploymentStats$Year)
UnemploymentStats$EmploymentRate <- factor(UnemploymentStats$EmploymentRate)
str(UnemploymentStats)

# The following command cleans up the Reported Crime data.


CrimeStats <- as.data.frame(CrimeStats)
str(CrimeStats)

#Removing the first two lines of the file as this row is not required
CrimeStats <- CrimeStats[-2,]

colnames(CrimeStats) <- c("Offence", "1998", "1999", "2000", "2001", 
                          "2002", "2003", "2004", "2005", "2006", 
                          "2007", "2008", "2009",	"2010",	"2011",
                          "2012", "2013", "2014",	"2015",	"2016",
                          "2017", "Change", "Percent Change")

# Some key parts of cleaning up the Crime data was that I deleted the first section of statsitics 
# because in my opinion serious crimes like murder, attempted murder or rape would not be triggered 
# directly by the persons economical wellbeing.   I am excluding these metrics from my overall analysis.
# The metrics that are more pertinent to this analysis are in general robbery, criminal damage 
# and public order offences
CrimeStats <-  CrimeStats[-c(1:80), ]
CrimeStats[110:115, ]
CrimeStats <- CrimeStats[-c(110:115), ]

# I will now select only those crime statistics which I would expect to be impacted by a persons unemployment 
# status.  That is to say that the crime would be one of a financial nature where it would not neccessarily
# require the involvement of a criminal core but more driven by opportunity or need.  For this I am selecting
# the following categories for further analysis:
# TOTAL ROBBERY OFFENCES          -> Robbery_Offences
# Total domestic burglary32       -> Domestic_Burglary
# Total non-domestic burglary32   -> Non_Domestic_Burglary
# Total theft from the person     -> Theft_from_Person
# Total bicycle theft             -> Bicycle_Theft
# Total theft - shoplifting       -> Shoplifting
# Total all other theft offences  -> Other_Theft_Offences
# TOTAL PUBLIC ORDER OFFENCES     -> Tot_Public_Order

FocusedCrimeStats = CrimeStats[c(1,12, 16, 33, 35, 37, 39, 71), ]
View(FocusedCrimeStats)

# To now allow for a like to like comparison of the Crime data againse the unemployment figures
# the next command requires me to Transpose the data.  I then:
# 1. Transposed the data so that it is ordered by year and can be compared to the unemployment data
# 2. added more readable column names for the data selected above
# 3. removed the first and last two records to clean up the data
# 4. Added a now column called Year to allow for merging with the unemployment data
TransposedCrimeStats = t(FocusedCrimeStats)

colnames(TransposedCrimeStats) <- c("Robbery_Offences", "Domestic_Burglary", "Non_Domestic_Burglary", 
                                    "Theft_from_Person", "Bicycle_Theft", 
                                    "Shoplifting", "Other_Theft_Offences", "Tot_Public_Order")

TransposedCrimeStats <- as.data.frame(TransposedCrimeStats)

TransposedCrimeStats <- TransposedCrimeStats[-1,]
TransposedCrimeStats <-  TransposedCrimeStats[-c(21:22), ]

TransposedCrimeStats$Year <- c("1998", "1999", "2000", "2001", 
                               "2002", "2003", "2004", "2005", "2006", 
                               "2007", "2008", "2009",	"2010",	"2011",
                               "2012", "2013", "2014",	"2015",	"2016","2017")




# This command merges the two datasets into a single file to allow for easier analysis in next steps
#  The "CombinedData" dataframe can then be analysed


CombinedData <- merge(UnemploymentStats, TransposedCrimeStats, by="Year")

CombinedData$Year <- factor(CombinedData$Year)
View(CombinedData)

plot(CombinedData$Year, as.numeric(CombinedData$EmploymentRate), type = "o", col = "red")

plot(CombinedData$Year, as.numeric(CombinedData$Robbery_Offences), type = "o", col = "red")


plot(CombinedData$Year, type="l", col=plot_colors[1], 
     ylim=range(CombinedData), axes=F, ann=T, xlab="Days",
     ylab="Total", cex.lab=0.8, lwd=2)


plot_colors <- c(rgb(r=0.0,g=0.0,b=0.9), "red", "forestgreen")
box()

# Graph trucks with thicker red dashed line
lines(CombinedData$EmploymentRate, type="l", lty=2, lwd=2, 
      col=plot_colors[2])

# Graph suvs with thicker green dotted line
lines(CombinedData$Robbery_Offences, type="l", lty=3, lwd=2, 
      col=plot_colors[3])

lines(CombinedData$Domestic_Burglary, type="l", lty=3, lwd=2, 
      col=plot_colors[1])


lines(CombinedData$Non_Domestic_Burglary, type="l", lty=3, lwd=2, 
      col=plot_colors[4])

shapiro.test(as.numeric(CombinedData$EmploymentRate))
shapiro.test(as.numeric(CombinedData$Robbery_Offences))

shapiro.test(as.numeric(CombinedData$Domestic_Burglary))


kruskal.test(CombinedData$EmploymentRate~CombinedData$Year)

kruskal.test(CombinedData$Robbery_Offences~CombinedData$EmploymentRate)

kruskal.test(CombinedData$Domestic_Burglary~CombinedData$EmploymentRate)

boxplot(CombinedData$Robbery_Offences, CombinedData$Domestic_Burglary)

cor.test(as.numeric(CombinedData$EmploymentRate), as.numeric(CombinedData$Robbery_Offences))

cor.test(as.numeric(CombinedData$EmploymentRate), as.numeric(CombinedData$Domestic_Burglary))

cor.test(as.numeric(CombinedData$EmploymentRate), as.numeric(CombinedData$Non_Domestic_Burglary))

cor.test(as.numeric(CombinedData$EmploymentRate), as.numeric(CombinedData$Theft_from_Person), method = "spearman")


# Create a legend in the top-left corner that is slightly  
# smaller and has no border
legend("topleft", names(autos_data), cex=0.8, col=plot_colors, 
       lty=1:3, lwd=2, bty="n");







# Formal test of normality
# provided through widely used Shapiro-Wilks test

normality_test <- shapiro.test(transform.beaver$temp)

normality_test$p.value

# p-value tells us the chances that the sample comes 

# from a normal distribution 

# In this example, p-value is clearly lower than 0.05

# so not normally distributed



# We can check the normality in each variable

# using the tapply() function

with(transformed_beaver_data, tapply(temp, activ, shapiro.test))



# Comparing 2 samples - most widely used test

# eg comparing mileage in cars with manual and auto gearboxes

# R provides 2 tests for comparing numerical data

# the t-test and the Wilcoxon test

# Wilcoxon test does not require normally distributed data



# Carrying out a t-test

# Normally we can only carry out t-test on samples where variances

# are equal. Applying Welch variation allows for unequal variances



# In this test we are evaluating temp within groups determined by activ

t.test(temp ~ activ, data = transformed_beaver_data)



# t = test statistic

# df = degrees of freedom

# p = p value. Small p = means of both samples differ significantly

# Alternative hypothesis = what you can conclude if the p-value 

# is lower than the limit for significance (<0.05)

# This shows us that the true mean of the difference is not 0 

# ie that we reject the null hypothesis

# 95 percent confidence interval contains the difference between the means

# with 95% probability, in this case the difference between the means lies probably

# between 0.72 and 0.89



# We can also use two separate vectors for the samples you want to compare

# and pass both to the function

with(transformed_beaver_data,
     
     t.test(temp[activ == "yes"],
            
            temp[activ == "no"]))



# We can use the Wilcox.text() function 

# for data that deviates from normality

# In this test we get the test statistic (W)

# as well as the p value

# This test examines whether the centre of the data

# differs for both samples

wilcox.test(temp ~ activ, data = transformed_beaver_data)

# This test is the same as the Mann-Whitney U test

# so R doent have a separate test for Mann-Whitney



t.test(extra ~ group, data = sleep, paired = TRUE)



# Comparing paired data

# Paired data occurs when 2 different treatments are

# given to the same subjects



# Sleep dataset contains data from 10 participants

# who are given 2 types of sleep medicine

# Researchers record difference in sleep for

# each person with and without drugs

str(sleep)

# Extra = extra hours of sleep after medication

# Group = which variant each participant took

# Id = participant ID



# Each person gets both variants - data is therefore paired

# We want to know if both types of sleep medicine had an effect

# on length of sleep



# t test and wilcox test have paired argument

t.test(extra ~ group, data = sleep, paired = TRUE)

# We only get 1 mean instead of 2 this time



# Testing counts and proportions

# Counts are summarised in tables



# Example of patients involved in car accidents

# whether they wore a seat belt or not

survivors <- matrix(c(1781, 1443, 135, 47), ncol = 2)

colnames(survivors) <- c("survived", "died")

rownames(survivors) <- c("no seat belt", "seat belt")

survivors



# A proportion test can examine the probability

# that both proportions are the same

result_prop_test <- prop.test(survivors)

result_prop_test

# Results are almost identical to t.test function

# p value tells us how likely it is that both 

# proportions are equal.

# p = 0.0000008105 - null hypothesis not true

# therefore dying in a hospital after a crash is lower 

# if you’re wearing a seat belt at the time of the crash



# We can use the prop.test() function with 2 columns

# Alternatively we can use the chisq.test()function 

# to examine this same seat belt data

chisq.test(survivors)

# Results from both tests are the same - as expected



# These tests can also be used with more than 2 columns

# We'll use HairEyeColor to proove this



str(HairEyeColor)

class(HairEyeColor)

# Contains 3 dimensions - hair, eye colour, and sex

# We can use dimnames to collapse to extract dimension names

# See help for more details

dimnames(HairEyeColor)



# Check if hair colour and eye colour are related

# we have to collapse the table first using the margin.table() function

# The margin argumant is an index number, so

# we're building the table firstly by hair and then organise

# data by eye colour

hair_eye_margin <- margin.table(HairEyeColor, margin = c(1, 2))

hair_eye_margin



# Once this is built then we can simply check

# whether hair and eye colour are related

chisq.test(hair_eye_margin)

# The p-value tells us some combinations of hair and eye color are more

# common than others.
