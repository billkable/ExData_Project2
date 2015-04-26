library(plyr)
library(dplyr)
library(ggplot2)

###############################################################
## Demonstrate of the 4 types of sources (point, nonpoint, 
## road, nonroad) have either increasing or decreasing
## emissions from 1999 -> 2008
##
## Barplots are chosen because data is discrete (Annual only
## samples by category)
###############################################################

# Load data set and reference data frames if not already loaded
if(!exists("emissionsDf")) emissionsDf <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
if(!exists("sccRefDf")) sccRefDf <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")

# Filter for the Baltimore area (fips == "24510")
baltimoreEmissionsDf <- filter(emissionsDf,fips == "24510")


# Generate grouped and ordered dataset for baltimore totals
baltimoreAnnualEmissionTotalsByTypeYearDf <- ddply(baltimoreEmissionsDf,
                                                   c("type","year"),
                                                   function(df) {sum(df$Emissions)})
names(baltimoreAnnualEmissionTotalsByTypeYearDf) <- c("Type","Year","TotalEmissions")
baltimoreAnnualEmissionTotalsByTypeYearDf$Year <- factor(baltimoreAnnualEmissionTotalsByTypeYearDf$Year)

baltimoreAnnualEmissionTotalsByTypeYearDf <- arrange(baltimoreAnnualEmissionTotalsByTypeYearDf,
                                                     Year,
                                                     Type)

# Plot Baltimore Total Emmissions by Year, by Facet per Type
# Bar plots used given Type, Year groupings are discreet
p <- ggplot(data=baltimoreAnnualEmissionTotalsByTypeYearDf,
       aes(x=Year,y=TotalEmissions,fill=Type)) +
        geom_bar(colour="black", stat="identity") +
        facet_wrap(~ Type, ncol=2) + 
        ggtitle("Annual Baltimore Emission Totals by Type") +
        ylab("Emissions (Tons)")

print(p)
ggsave("./plot3.png")
