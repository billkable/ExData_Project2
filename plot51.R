library(plyr)
library(dplyr)
library(ggplot2)

###############################################################
## Show how emissions from motor vehicle sources changed from 
## 1999–2008 in Baltimore City.
##
## Definition of Motor Vehicle interpreted as:
##  "A road vehicle powered by an internal combustion engine"
##
##  From analysis of reference data Source_Classification_Code
##  it is assumed Data.Category of "Onroad" sufficiently
##  categorizes motor vehicles for this study
###############################################################

# Load Reference and data sets if not already loaded
loadEmissionsData <- function() {
        if(!exists("emissionsDf")) {
                emissionsDf <<- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
                emissionsDf$SCC <- factor(emissionsDf$SCC)
        }
        emissionsDf
}

loadEmissionsSCCRefData <- function() {
        if(!exists("sccRefDf")) sccRefDf <<- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
        sccRefDf
}

# Filter motor vehicle based from Onroad data categories
# See correlation of onroad and offroad types to EI.Sector (Mobile....)
# and Short.Name (On highway)
filterMotorVehicleSCCRef <- function() {
        sccRefDf <- loadEmissionsSCCRefData()
        
        motorVehicleRefDf <- select(subset(sccRefDf,sccRefDf$Data.Category == "Onroad"),
                                    SCC,EI.Sector)
        
        motorVehicleRefDf$EI.Sector <- gsub(" Vehicles","",
                                            gsub("Mobile - On-Road ", "",
                                                 motorVehicleRefDf$EI.Sector))   
        
        motorVehicleRefDf
}

# Filter emmissions data for motor vehicle and baltimore
filterMotorVehicleEmissionsForBaltimore <- function(emissionsDf) {
        motorVehicleRefDf <- filterMotorVehicleSCCRef();

        motorVehicleEmissionsDf <- inner_join(motorVehicleRefDf,
                                              select(subset(emissionsDf,emissionsDf$fips == "24510"),SCC,year,Emissions),
                                              by="SCC")
        motorVehicleEmissionsDf
}

# Generate Totals Data Frame by Year and EI.Sector (Category)
buildEmissionsByYearAndCategory <- function() {
        emissionsDf <- loadEmissionsData()
        
        motorVehicleEmissionsDf <- filterMotorVehicleEmissionsForBaltimore(emissionsDf)
        
        motorVehicleEmmissionsSubTotalsDf <- ddply(motorVehicleEmissionsDf,
                                                   c("year","EI.Sector"),
                                                   function(df) sum(df$Emissions))
        names(motorVehicleEmmissionsSubTotalsDf) = c("Year","Category","Emissions")
        motorVehicleEmmissionsSubTotalsDf$Year <- factor(motorVehicleEmmissionsSubTotalsDf$Year)
        motorVehicleEmmissionsSubTotalsDf
}

# Generate Annual Emissions Grand Totals
buildAnnualEmissionsGrandTotals <- function(motorVehicleEmmissionsSubTotalsDf) {
        motorVehicleEmmissionsTotalsDf <- ddply(motorVehicleEmmissionsSubTotalsDf,
                                                "Year",
                                                function(df) sum(df$Emissions))
        names(motorVehicleEmmissionsTotalsDf) = c("Year","Emissions")
        motorVehicleEmmissionsTotalsDf$Year <- factor(motorVehicleEmmissionsTotalsDf$Year)
        motorVehicleEmmissionsTotalsDf
}

# Build Report Dataframe to merge subtotals and grand total
buildReportDf <- function(motorVehicleEmmissionsSubTotalsDf) {
        motorVehicleEmmissionsTotalsDf <- buildAnnualEmissionsGrandTotals(motorVehicleEmmissionsSubTotalsDf)
        
        reportDf <- motorVehicleEmmissionsSubTotalsDf
        reportDf <- rbind(reportDf,
                          data.frame(Year=motorVehicleEmmissionsTotalsDf$Year,
                                     Category="Total: Annual Emission",
                                     Emissions=motorVehicleEmmissionsTotalsDf$Emissions))
        reportDf
}

# Plot Baltimore Total Emmissions by Year for Onroad and Offroad Vehicles
# Bar plots used given Type, Year groupings are discreet
plotEmissions <- function(reportDf) {
        p <- ggplot(data=reportDf,
                    aes(x=Year,y=Emissions,fill=Category)) +
                geom_bar(colour="black", stat="identity") +
                facet_wrap(~ Category, ncol = 2) +
                ggtitle("Baltimore Motor Vehicle Emissions")
        print(p)
        ggsave("./plot5.png")
}

# Execute Plot 5 load/build/plot process
buildEmissionsByYearAndCategory() %>%
buildReportDf() %>%
plotEmissions()





