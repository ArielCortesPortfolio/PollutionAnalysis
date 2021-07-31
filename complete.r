#First, we import the libraries we are going to use

library(dplyr)
library(plyr)
library(readr)

#Then, we import the CSV files

CSVfiles <- list.files(path= "specdata",pattern = "*.csv", full.names = TRUE)

class(CSVfiles)

print(CSVfiles)

BigData <- ldply(CSVfiles, read_csv, show_col_types = FALSE)

class(BigData)
nrow(BigData)

#Now, we make the function to calculate the mean for each pollutant in a monitor

MeanPollutant <- function(dataframe, pollutant, id = 1:332) {
    CleanBigData = na.omit(dataframe)
    mean(CleanBigData[id,pollutant], na.rm = TRUE)

}

#We can select which pollutant and monitor we want to look up

MeanPollutantTest <- MeanPollutant(BigData,"nitrate",330)
print(MeanPollutantTest)

#Complete cases by monitor

complete <- function(dataframe , id = 1:332, na.rm = TRUE) {
    CleanComplete <- na.omit(dataframe)
    CompleteFreq <- ddply(CleanComplete,.(ID),nrow)
    CompleteReturn <- CompleteFreq[id,"V1"]
    DFreturn <- data.frame(Monitor = id, nobs = CompleteReturn)
}

#Now, we can count how many complete cases we had by monitor

completeCasesTest <- complete(BigData,3:10)
print(completeCasesTest)

#Last, we shall find the correlation between sulfate and nitrogene for monitors where the numer of completed cases is greater than a threshold

#First I will create a data frame with the means and complete cases per monitor

Warehouse <- 1

for (i in 1:332) {
    Mula <- MeanPollutant(BigData,"nitrate",i)
    Warehouse <- c(Warehouse,Mula)
}

nitrateAllMeans <- Warehouse[-1]

Warehouse <- 1


for (i in 1:332) {
    Mula <- MeanPollutant(BigData,"sulfate",i)
    Warehouse <- c(Warehouse,Mula)
}

sulfateAllMeans <- Warehouse[-1]

cor(sulfateAllMeans,nitrateAllMeans)

completeCases <- complete(BigData,1:332)

completeCases$nitrateMean <- nitrateAllMeans
completeCases$sulfateMean <- sulfateAllMeans

names(completeCases)

nrow(completeCases)

print(completeCases)

#Afterwards, we write the function to determine the correlation

corr <- function(dataframe,threshold = 0) {
    cleanForWarehouse <- na.omit(dataframe)
    corrWarehouse <- subset(cleanForWarehouse,nobs>threshold,select=c(nitrateMean,sulfateMean))
    cor(corrWarehouse,)
}

#At last, we test it

corrTest <- corr(completeCases,150)
print(corrTest)
summary(corrTest)