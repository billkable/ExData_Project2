library(dplyr)

###############################################################
## Demonstrate whether or not total emissions from PM2.5 
## have decreased in the United States from 1999 -> 2008.
##
## Barplots are chosen because data is discrete (Annual only
## samples by category)
###############################################################


# Load data set if not already loaded
if(!exists("emissionsDf")) emissionsDf <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")

# Build data frame for total emmissions grouped by year
# The emmissions total is aligned to millions scale for better readability
annualEmissionTotalsDf <- ddply(emissionsDf,"year",function(df) {sum(df$Emissions)/1000000})
names(annualEmissionTotalsDf) <- c("Year","TotalEmissions")

# Plot Totals for each year in set (1999,2002,2005,2008)
#  Bar charts are chosen because data is discreet, by year
barplot(annualEmissionTotalsDf$TotalEmissions,
        names.arg = annualEmissionTotalsDf$Year,
        main = "PM2.5 Emission Totals by Year",
        ylab = "PM2.5 Emissions (Millions of Tons)",
        xlab = "Year",
        col = "red")
png("./plot1.png")
dev.off()
