library(plyr)
library(dplyr)
library(ggplot2)

###############################################################
## Show changes in annual Emissions of Coal-Combustion-Based
## sources in the US from 1999 -> 2008
## 
## Barplots are chosen because data is discrete (Annual only
## samples by category)
###############################################################

# Load data set and reference data frames if not already loaded
if(!exists("emissionsDf")) {
        emissionsDf <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
        emissionsDf$SCC <- factor(emissionsDf$SCC)
}

if(!exists("sccRefDf")) sccRefDf <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")

# Filter out SCCs for only Coal Producing Pollutants
# Coal pollutants identified from EI.Sector, compression from SCC.Level.One
# Build logical index list from ref table
coalIndexL <- grepl("Coal",sccRefDf$EI.Sector) &
                grepl("Combustion",sccRefDf$SCC.Level.One)

coalSccRefDf <- subset(sccRefDf, coalIndexL)

coalEmissionsDf <- inner_join(select(coalSccRefDf,SCC),
                       select(emissionsDf,SCC,year,type,Emissions),
                       by = "SCC")

coalTotalEmissionsByYear <- ddply(coalEmissionsDf,"year",function(df) sum(df$Emissions)/1000)
names(coalTotalEmissionsByYear) <- c("Year","TotalEmissions")
coalTotalEmissionsByYear$Year <- factor(coalTotalEmissionsByYear$Year)


# Plot Coal Combustion Total Emmissions by Year
# Bar plots used given Year groupings are discreet
p <- ggplot(data=coalTotalEmissionsByYear,
            aes(x=Year,y=TotalEmissions)) +
        geom_bar(colour="black", stat="identity", fill="red") + 
        ggtitle("Emissions from Coal-Combustion Sources across US") +
        ylab("Emissions (Tons)")

print(p)
ggsave("./plot4.png")
