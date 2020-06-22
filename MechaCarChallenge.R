library(tidyverse)
library(jsonlite)
library(readxl)

## MECHACAR

MechaCar <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# Change header names
names(MechaCar) <- c("Vehicle_Length", "Vehicle_Weight", "Spoiler_Angle", "Ground_Clearance", "AWD", "mpg")

# Create multiple linear regression
# Looking at mpg based on 
lm(mpg ~ Vehicle_Length + Vehicle_Weight + Spoiler_Angle + 
     Ground_Clearance + AWD, data=MechaCar)
# obtain statistical metrics using summary
summary(lm(mpg ~ Vehicle_Length + Vehicle_Weight + Spoiler_Angle + 
             Ground_Clearance + AWD, data=MechaCar))

## SUSPENSION COIL TEST

suspensionCoil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Summary statistics table
perInchSummary<- suspensionCoil %>% summarize(meanPSI=mean(PSI),median_PSI=median(PSI), Variance=var(PSI), St_Dev=sd(PSI)) 
perInchSummary

# Rotate the table
inchSummaryRotate <- t(as.matrix(perInchSummary))
inchSummaryRotate

## Suspension Coil T-Test
# Visualize distribution
ggplot(suspensionCoil, aes(x=PSI)) + geom_density()

# Run a t-test on the suspensionCoil data frame as your sample and 1500 as the population mean
t.test(suspensionCoil$PSI, mu=1500)

