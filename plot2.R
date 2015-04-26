library(dplyr)

###############################################################
## Demonstrate whether or not total emissions from PM2.5 
## decreased in the Baltimore City, Maryland (fips == "24510")
## from 1999 -> 2008
##
## Barplots are chosen because data is discrete (Annual only
## samples by category)
###############################################################

# Load data set if not already loaded
if(!exists("emissionsDf")) emissionsDf <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")

# Filter for the Baltimore area (fips == "24510")
baltimoreEmissionsDf <- filter(emissionsDf,fips == "24510")

# Build data frame for total emmissions grouped by year
# The emmissions total is aligned to thousands scale for better readability
annualEmissionTotalsDf <- ddply(baltimoreEmissionsDf,"year",function(df) {sum(df$Emissions)/1000})
names(annualEmissionTotalsDf) <- c("Year","TotalEmissions")


# Plot Totals for Baltimore area each year in set (1999,2002,2005,2008)
#  Bar charts are chosen because data is discreet, by year
barplot(annualEmissionTotalsDf$TotalEmissions,
        names.arg = annualEmissionTotalsDf$Year,
        main = "PM2.5 Baltimore Emission Totals by Year",
        ylab = "PM2.5 Emissions (Thousands of Tons)",
        xlab = "Year",
        col = "red")
png("./plot2.png")
dev.off()
