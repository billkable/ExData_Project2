library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

###############################################################
## Compare emissions from motor vehicle sources in Baltimore 
## City (fips == "24510") with emissions from motor vehicle 
## sources in Los Angeles County, California (fips == "06037"). 
## 
## Which city has seen greater changes over time in motor 
## vehicle emissions?
##
## Definition of Motor Vehicle interpreted as:
##  "A road vehicle powered by an internal combustion engine"
##
## Given the reader may interpret by automotive (light) or
## Heavy Duty Gasoline or Diesel vehicles, more assesssment
## done to determine additional references.
## 
## From analysis of reference data Source_Classification_Code:
## 1.  It is assumed Data.Category of "Onroad" sufficiently
##     categorizes motor vehicles for this study.
## 2.  There are effective sub categories of Onroad that
##      apply to Onroad sources:
##      - Gasoline Light Duty Vehicles
##      - Gasoline Heavy Duty Vehicles
##      - Diesel Light Duty Vehicles
##      - Diesel Heavy Duty Vehicles
##
## Barplots are chosen because data is discrete (Annual only
## samples by category)
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
                                              select(subset(emissionsDf,emissionsDf$fips == "24510" |
                                                                    emissionsDf$fips == "06037"),SCC,year,fips,Emissions),
                                              by="SCC")
        motorVehicleEmissionsDf
}

# Generate Totals Data Frame by Year and EI.Sector (Category)
buildEmissionsByYearAndCategory <- function() {
        emissionsDf <- loadEmissionsData()
        
        motorVehicleEmissionsDf <- filterMotorVehicleEmissionsForBaltimore(emissionsDf)
        
        motorVehicleEmmissionsSubTotalsDf <- ddply(motorVehicleEmissionsDf,
                                                   c("year","fips","EI.Sector"),
                                                   function(df) sum(df$Emissions))
        names(motorVehicleEmmissionsSubTotalsDf) = c("Year","City","Category","Emissions")
        motorVehicleEmmissionsSubTotalsDf$Year <- factor(motorVehicleEmmissionsSubTotalsDf$Year)
        motorVehicleEmmissionsSubTotalsDf
}

# Generate Annual Emissions Grand Totals
buildAnnualEmissionsGrandTotals <- function(motorVehicleEmmissionsSubTotalsDf) {
        motorVehicleEmmissionsTotalsDf <- ddply(motorVehicleEmmissionsSubTotalsDf,
                                                c("Year","City"),
                                                function(df) sum(df$Emissions))
        names(motorVehicleEmmissionsTotalsDf) = c("Year","City","Emissions")
        motorVehicleEmmissionsTotalsDf$Year <- factor(motorVehicleEmmissionsTotalsDf$Year)
        motorVehicleEmmissionsTotalsDf
}

# Build Report Dataframe to merge subtotals and totals
buildReportDf <- function(motorVehicleEmmissionsSubTotalsDf) {
        motorVehicleEmmissionsTotalsDf <- buildAnnualEmissionsGrandTotals(motorVehicleEmmissionsSubTotalsDf)
        
        reportDf <- motorVehicleEmmissionsSubTotalsDf
        reportDf <- rbind(reportDf,
                          data.frame(Year=motorVehicleEmmissionsTotalsDf$Year,
                                     City=motorVehicleEmmissionsTotalsDf$City,
                                     Category="Total: Annual Emission",
                                     Emissions=motorVehicleEmmissionsTotalsDf$Emissions))
        reportDf$City <- gsub("24510","Baltimore",reportDf$City)
        reportDf$City <- gsub("06037","Los Angeles",reportDf$City)
        
        reportDf
}

# Plot Baltimore Total Emmissions by Year for Onroad and Offroad Vehicles
# Bar plots used given Type, Year groupings are discreet
plotEmissions <- function(reportDf) {
        p <- ggplot(data=reportDf,
                    aes(x=Year,y=Emissions,fill=City)) +
                geom_bar(colour="black", stat="identity",position="dodge") +
                facet_wrap(~ Category, ncol = 2) +
                ylab("Emissions Totals (Tons)") +
                ggtitle("Baltimore vs. Los Angeles Motor Vehicle Emissions")
        print(p)
        ggsave("./plot6.png")
}

# Execute Plot 6 load/build/plot process
buildEmissionsByYearAndCategory() %>%
        buildReportDf() %>%
        plotEmissions()

