# This script performs the econometrics analysis for the US + KP paper

# Loading libraries

library(dplyr)
library(Synth)
library(ggplot2)
library(modelsummary)

# Directory info

direc <- 'D:/KPUS/'

# Reading in the data

data <- read.csv(paste(direc, 'data/kpus_data.csv', sep = ''))

# Adding lagged outcomes data

data$GHG.LAG <- dplyr::lag(data$Total.greenhouse.gas.emissions..kt.of.CO2.equivalent., n = 1)
data$CO2.LAG <- dplyr::lag(data$CO2.emissions..kt., n = 1)
data$CH4.LAG <- dplyr::lag(data$Methane.emissions..kt.of.CO2.equivalent., n = 1)
data$NO2.LAG <- dplyr::lag(data$Nitrous.oxide.emissions..thousand.metric.tons.of.CO2.equivalent., n = 1)
data$AG.CH4.LAG <- dplyr::lag(data$Agricultural.methane.emissions..thousand.metric.tons.of.CO2.equivalent., n = 1)
data$AG.NO2.LAG <- dplyr::lag(data$Agricultural.nitrous.oxide.emissions..thousand.metric.tons.of.CO2.equivalent., n = 1)

# Subset for 1991 - 2019

data <- data[which(data$Year %in% 1991:2019),]

# Transforming outcome variables

ghg <- c()
co2 <- c()
ch4 <- c()
no2 <- c()
ch4.ag <- c()
no2.ag <- c()

for (i in 1:dim(data)[1]) {
  
  tmp.df <- data[which(data$Country.Name == data$Country.Name[i] & data$Year == 1991),]
  
  ghg <- c(ghg, data$Total.greenhouse.gas.emissions..kt.of.CO2.equivalent.[i] / tmp.df$Total.greenhouse.gas.emissions..kt.of.CO2.equivalent.[1])
  co2 <- c(co2, data$CO2.emissions..kt.[i] / tmp.df$CO2.emissions..kt.[1])
  ch4 <- c(ch4, data$Methane.emissions..kt.of.CO2.equivalent.[i] / tmp.df$Methane.emissions..kt.of.CO2.equivalent.[1])
  no2 <- c(no2, data$Nitrous.oxide.emissions..thousand.metric.tons.of.CO2.equivalent.[i] / tmp.df$Nitrous.oxide.emissions..thousand.metric.tons.of.CO2.equivalent.[1])
  ch4.ag <- c(ch4.ag, data$Agricultural.methane.emissions..thousand.metric.tons.of.CO2.equivalent.[i] / tmp.df$Agricultural.methane.emissions..thousand.metric.tons.of.CO2.equivalent.[1])
  no2.ag <- c(no2.ag, data$Agricultural.nitrous.oxide.emissions..thousand.metric.tons.of.CO2.equivalent.[i] / tmp.df$Agricultural.nitrous.oxide.emissions..thousand.metric.tons.of.CO2.equivalent.[1])
  
}

data$GHG <- ghg
data$CO2 <- co2
data$CH4 <- ch4
data$NO2 <- no2
data$AG.CH4 <- ch4.ag
data$AG.NO2 <- no2.ag

# Transforming predictors

gdp <- c()
gdp.pc <- c()
pop <- c()
popden <- c()
rural.pop <- c()
urban.pop <- c()
tariff.rate <- c()
forest.rents <- c()
epc <- c()
coal <- c()
hydro <- c()
gas <- c()
nuclear <- c()
oil <- c()
renewables <- c()
lag.ghg <- c()
lag.co2 <- c()
lag.ch4 <- c()
lag.no2 <- c()
lag.ch4.ag <- c()
lag.no2.ag <- c()

for (i in 1:dim(data)[1]) {
  
  tmp.df <- data[which(data$Country.Name == data$Country.Name[i] & data$Year == 1991),]
  gdp <- c(gdp, data$GDP..constant.2015.US..[i] / tmp.df$GDP..constant.2015.US..[1])
  gdp.pc <- c(gdp.pc, data$GDP.per.capita..constant.2015.US..[i] / tmp.df$GDP.per.capita..constant.2015.US..[1])
  pop <- c(pop, data$Population..total[i] / tmp.df$Population..total[1])
  popden <- c(popden, data$Population.density..people.per.sq..km.of.land.area.[i] / tmp.df$Population.density..people.per.sq..km.of.land.area.[1])
  rural.pop <- c(rural.pop, data$Rural.population[i] / tmp.df$Rural.population[1])
  urban.pop <- c(urban.pop, data$Urban.population[i] / tmp.df$Urban.population[1])
  tariff.rate <- c(tariff.rate, data$Tariff.rate..applied..weighted.mean..all.products....[i] / tmp.df$Tariff.rate..applied..weighted.mean..all.products....[1])
  forest.rents <- c(forest.rents, data$Forest.rents....of.GDP.[i] / tmp.df$Forest.rents....of.GDP.[1])
  epc <- c(epc, data$Electric.power.consumption..kWh.per.capita.[i] / tmp.df$Electric.power.consumption..kWh.per.capita.[1])
  coal <- c(coal, data$Electricity.production.from.coal.sources....of.total.[i] / tmp.df$Electricity.production.from.coal.sources....of.total.[1])
  hydro <- c(hydro, data$Electricity.production.from.hydroelectric.sources....of.total.[i] / tmp.df$Electricity.production.from.hydroelectric.sources....of.total.[1])
  gas <- c(gas, data$Electricity.production.from.natural.gas.sources....of.total.[i] / tmp.df$Electricity.production.from.natural.gas.sources....of.total.[1])
  nuclear <- c(nuclear, data$Electricity.production.from.nuclear.sources....of.total.[i] / tmp.df$Electricity.production.from.nuclear.sources....of.total.[1])
  oil <- c(oil, data$Electricity.production.from.oil.sources....of.total.[i] / tmp.df$Electricity.production.from.oil.sources....of.total.[1])
  renewables <- c(renewables, data$Electricity.production.from.renewable.sources..excluding.hydroelectric....of.total.[i] / tmp.df$Electricity.production.from.renewable.sources..excluding.hydroelectric....of.total.[1])
  lag.ghg <- c(lag.ghg, data$GHG.LAG[i] / tmp.df$GHG.LAG[1])
  lag.co2 <- c(lag.co2, data$CO2.LAG[i] / tmp.df$CO2.LAG[1])
  lag.ch4 <- c(lag.ch4, data$CH4.LAG[i] / tmp.df$CH4.LAG[1])
  lag.no2 <- c(lag.no2, data$NO2.LAG[i] / tmp.df$NO2.LAG[1])
  lag.ch4.ag <- c(lag.ch4.ag, data$AG.CH4.LAG[i] / tmp.df$AG.CH4.LAG[1])
  lag.no2.ag <- c(lag.no2.ag, data$AG.NO2.LAG[i] / tmp.df$AG.NO2.LAG[1])
  
}

data$GDP <- gdp
data$GDP.pc <- gdp.pc
data$Population <- pop
data$Population.Density <- popden
data$Rural <- rural.pop
data$Urban <- urban.pop
data$Tariff.Rate <- tariff.rate
data$Forest.Rents <- forest.rents
data$Electricity.Consumption.pc <- epc
data$Coal <- coal
data$Hydroelectric <- hydro
data$Natural.Gas <- gas
data$Nuclear <- nuclear
data$Oil <- oil
data$Other.Renewables <- renewables
data$GHG.Lag <- lag.ghg
data$CO2.Lag <- lag.co2
data$CH4.Lag <- lag.ch4
data$NO2.Lag <- lag.no2
data$AG.CH4.Lag <- lag.ch4.ag
data$AG.NO2.Lag <- lag.no2.ag

# Removing nations with missing data

for (nation in unique(data$Country.Name)) {
  
  if (is.na(data[which(data$Country.Name == nation),]$GDP[1]) == TRUE | is.na(data[which(data$Country.Name == nation),]$Population.Density[1]) == TRUE) {
    
    data <- data[which(data$Country.Name != nation),]
    
  }
  
}

# Creating two data.frames

kp.data <- data[which(data$Kyoto == 1 | data$Country.Name == 'United States'),]
control.data <- data[which(data$Kyoto == 0 | data$Country.Name == 'United States'),]

# Defining the control variables

preds <- c('GDP.pc', 'Population', 'Electricity.Consumption.pc', 'Forest.Rents', 'Rural', 'Urban')

# Defining the outcome variables

outcomes <- c('GHG', 'CO2', 'CH4', 'NO2', 'AG.CH4', 'AG.NO2')

# Checking for complete cases

kp.data.ghg <- kp.data[,names(kp.data) %in% c(preds, outcomes, 'GHG.Lag', 'Country.Name', 'Year', 'ID')]
kp.data.co2 <- kp.data[,names(kp.data) %in% c(preds, outcomes, 'CO2.Lag', 'Country.Name', 'Year', 'ID')]
kp.data.ch4 <- kp.data[,names(kp.data) %in% c(preds, outcomes, 'CH4.Lag', 'Country.Name', 'Year', 'ID')]
kp.data.no2 <- kp.data[,names(kp.data) %in% c(preds, outcomes, 'NO2.Lag', 'Country.Name', 'Year', 'ID')]
kp.data.ag.ch4 <- kp.data[,names(kp.data) %in% c(preds, outcomes, 'AG.CH4.Lag', 'Country.Name', 'Year', 'ID')]
kp.data.ag.no2 <- kp.data[,names(kp.data) %in% c(preds, outcomes, 'AG.NO2.Lag', 'Country.Name', 'Year', 'ID')]

control.data.ghg <- control.data[,names(control.data) %in% c(preds, outcomes, 'GHG.Lag', 'Country.Name', 'Year', 'ID')]
control.data.co2 <- control.data[,names(control.data) %in% c(preds, outcomes, 'CO2.Lag', 'Country.Name', 'Year', 'ID')]
control.data.ch4 <- control.data[,names(control.data) %in% c(preds, outcomes, 'CH4.Lag', 'Country.Name', 'Year', 'ID')]
control.data.no2 <- control.data[,names(control.data) %in% c(preds, outcomes, 'NO2.Lag', 'Country.Name', 'Year', 'ID')]
control.data.ag.ch4 <- control.data[,names(control.data) %in% c(preds, outcomes, 'AG.CH4.Lag', 'Country.Name', 'Year', 'ID')]
control.data.ag.no2 <- control.data[,names(control.data) %in% c(preds, outcomes, 'AG.NO2.Lag', 'Country.Name', 'Year', 'ID')]

kp.data.ghg <- kp.data.ghg[which(complete.cases(kp.data.ghg) == TRUE),]
kp.data.co2 <- kp.data.co2[which(complete.cases(kp.data.co2) == TRUE),]
kp.data.ch4 <- kp.data.ch4[which(complete.cases(kp.data.ch4) == TRUE),]
kp.data.no2 <- kp.data.no2[which(complete.cases(kp.data.no2) == TRUE),]
kp.data.ag.ch4 <- kp.data.ag.ch4[which(complete.cases(kp.data.ag.ch4) == TRUE),]
kp.data.ag.no2 <- kp.data.ag.no2[which(complete.cases(kp.data.ag.no2) == TRUE),]

control.data.ghg <- control.data.ghg[which(complete.cases(control.data.ghg) == TRUE),]
control.data.co2 <- control.data.co2[which(complete.cases(control.data.co2) == TRUE),]
control.data.ch4 <- control.data.ch4[which(complete.cases(control.data.ch4) == TRUE),]
control.data.no2 <- control.data.no2[which(complete.cases(control.data.no2) == TRUE),]
control.data.ag.ch4 <- control.data.ag.ch4[which(complete.cases(control.data.ag.ch4) == TRUE),]
control.data.ag.no2 <- control.data.ag.no2[which(complete.cases(control.data.ag.no2) == TRUE),]

# Check to see which nations have complete data for all years

for (nation in unique(kp.data.ghg$Country.Name)) {
  
  print(nation)
  print(dim(kp.data.ghg[which(kp.data.ghg$Country.Name == nation),]))
  print(dim(kp.data.co2[which(kp.data.co2$Country.Name == nation),]))
  print(dim(kp.data.ch4[which(kp.data.ch4$Country.Name == nation),]))
  print(dim(kp.data.no2[which(kp.data.no2$Country.Name == nation),]))
  print(dim(kp.data.ag.ch4[which(kp.data.ag.ch4$Country.Name == nation),]))
  print(dim(kp.data.ag.no2[which(kp.data.ag.no2$Country.Name == nation),]))
  
}

for (nation in unique(control.data.ghg$Country.Name)) {
  
  print(nation)
  print(dim(control.data.ghg[which(control.data.ghg$Country.Name == nation),]))
  print(dim(control.data.co2[which(control.data.co2$Country.Name == nation),]))
  print(dim(control.data.ch4[which(control.data.ch4$Country.Name == nation),]))
  print(dim(control.data.no2[which(control.data.no2$Country.Name == nation),]))
  print(dim(control.data.ag.ch4[which(control.data.ag.ch4$Country.Name == nation),]))
  print(dim(control.data.ag.no2[which(control.data.ag.no2$Country.Name == nation),]))
  
}

# Based on the above

kp.data.ghg <- kp.data.ghg[which(!kp.data.ghg$Country.Name %in% c('Belarus', 'Kazakhstan', 'Ukraine')),]
kp.data.co2 <- kp.data.co2[which(!kp.data.co2$Country.Name %in% c('Belarus', 'Kazakhstan', 'Ukraine')),]
kp.data.ch4 <- kp.data.ch4[which(!kp.data.ch4$Country.Name %in% c('Belarus', 'Kazakhstan', 'Ukraine')),]
kp.data.no2 <- kp.data.no2[which(!kp.data.no2$Country.Name %in% c('Belarus', 'Kazakhstan', 'Ukraine')),]
kp.data.ag.ch4 <- kp.data.ag.ch4[which(!kp.data.ag.ch4$Country.Name %in% c('Belarus', 'Kazakhstan', 'Ukraine')),]
kp.data.ag.no2 <- kp.data.ag.no2[which(!kp.data.ag.no2$Country.Name %in% c('Belarus', 'Kazakhstan', 'Ukraine')),]

control.data.ghg <- control.data.ghg[which(!control.data.ghg$Country.Name %in% c('Angola', 'Cuba', 'Panama', 'Togo', 'Russian Federation', 'Czechia', 'North Macedonia', 'Armenia', 'Kyrgyz Republic', 'Azerbaijan', 'Georgia',  'Turkmenistan', 'Uzbekistan', 'Tajikistan', 'Sudan')),]
control.data.co2 <- control.data.co2[which(!control.data.co2$Country.Name %in% c('Angola', 'Cuba', 'Panama', 'Togo', 'Russian Federation', 'Czechia', 'North Macedonia', 'Armenia', 'Kyrgyz Republic', 'Azerbaijan', 'Georgia',  'Turkmenistan', 'Uzbekistan', 'Tajikistan', 'Sudan')),]
control.data.ch4 <- control.data.ch4[which(!control.data.ch4$Country.Name %in% c('Angola', 'Cuba', 'Panama', 'Togo', 'Russian Federation', 'Czechia', 'North Macedonia', 'Armenia', 'Kyrgyz Republic', 'Azerbaijan', 'Georgia',  'Turkmenistan', 'Uzbekistan', 'Tajikistan', 'Sudan')),]
control.data.no2 <- control.data.no2[which(!control.data.no2$Country.Name %in% c('Angola', 'Cuba', 'Panama', 'Togo', 'Russian Federation', 'Czechia', 'North Macedonia', 'Armenia', 'Kyrgyz Republic', 'Azerbaijan', 'Georgia',  'Turkmenistan', 'Uzbekistan', 'Tajikistan', 'Sudan')),]
control.data.ag.ch4 <- control.data.ag.ch4[which(!control.data.ag.ch4$Country.Name %in% c('Angola', 'Cuba', 'Panama', 'Togo', 'Russian Federation', 'Czechia', 'North Macedonia', 'Armenia', 'Kyrgyz Republic', 'Azerbaijan', 'Georgia',  'Turkmenistan', 'Uzbekistan', 'Tajikistan', 'Sudan')),]
control.data.ag.no2 <- control.data.ag.no2[which(!control.data.ag.no2$Country.Name %in% c('Angola', 'Cuba', 'Panama', 'Togo', 'Russian Federation', 'Czechia', 'North Macedonia', 'Armenia', 'Kyrgyz Republic', 'Azerbaijan', 'Georgia',  'Turkmenistan', 'Uzbekistan', 'Tajikistan', 'Sudan')),]

# Adding data for 2015 - 2019

for (nation in unique(kp.data.ghg$Country.Name)) {
  
  kp.data.ghg <- rbind(kp.data.ghg, kp.data[which(kp.data$Country.Name == nation & kp.data$Year %in% 2015:2019), which(names(kp.data) %in% names(kp.data.ghg))])
  kp.data.co2 <- rbind(kp.data.co2, kp.data[which(kp.data$Country.Name == nation & kp.data$Year %in% 2015:2019), which(names(kp.data) %in% names(kp.data.co2))])
  kp.data.ch4 <- rbind(kp.data.ch4, kp.data[which(kp.data$Country.Name == nation & kp.data$Year %in% 2015:2019), which(names(kp.data) %in% names(kp.data.ch4))])
  kp.data.no2 <- rbind(kp.data.no2, kp.data[which(kp.data$Country.Name == nation & kp.data$Year %in% 2015:2019), which(names(kp.data) %in% names(kp.data.no2))])
  kp.data.ag.ch4 <- rbind(kp.data.ag.ch4, kp.data[which(kp.data$Country.Name == nation & kp.data$Year %in% 2015:2019), which(names(kp.data) %in% names(kp.data.ag.ch4))])
  kp.data.ag.no2 <- rbind(kp.data.ag.no2, kp.data[which(kp.data$Country.Name == nation & kp.data$Year %in% 2015:2019), which(names(kp.data) %in% names(kp.data.ag.no2))])
  
}

for (nation in unique(control.data.ghg$Country.Name)) {
  
  if (nation != 'Indonesia') {# Indonesia data already comes through
    
    control.data.ghg <- rbind(control.data.ghg, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.ghg))])
    control.data.co2 <- rbind(control.data.co2, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.co2))])
    control.data.ch4 <- rbind(control.data.ch4, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.ch4))])
    control.data.no2 <- rbind(control.data.no2, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.no2))])
    control.data.ag.ch4 <- rbind(control.data.ag.ch4, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.ag.ch4))])
    control.data.ag.no2 <- rbind(control.data.ag.no2, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.ag.no2))])
    
  }
  
}

# Adding a column of IDs for Synth

kp.ids <- c()
c.ids <- c()

for (i in 1:dim(kp.data.ghg)[1]) {
  
  kp.ids <- c(kp.ids, which(unique(kp.data.ghg$Country.Name) == kp.data.ghg$Country.Name[i]))
  
}

for (i in 1:dim(control.data.ghg)[1]) {
  
  c.ids <- c(c.ids, which(unique(control.data.ghg$Country.Name) == control.data.ghg$Country.Name[i]))
  
} 

kp.data.ghg$ID <- kp.ids
kp.data.co2$ID <- kp.ids
kp.data.ch4$ID <- kp.ids
kp.data.no2$ID <- kp.ids
kp.data.ag.ch4$ID <- kp.ids
kp.data.ag.no2$ID <- kp.ids

control.data.ghg$ID <- c.ids
control.data.co2$ID <- c.ids
control.data.ch4$ID <- c.ids
control.data.no2$ID <- c.ids
control.data.ag.ch4$ID <- c.ids
control.data.ag.no2$ID <- c.ids

# Creating dataprep objects for Synth for KP sample

kp.ghg.dp <- dataprep(foo = kp.data.ghg, predictors = c(preds, 'GHG.Lag'), predictors.op = c('mean'),
                      dependent = c('GHG'), unit.variable = c('ID'), time.variable = c('Year'),
                      treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1991:2004),
                      time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

kp.co2.dp <- dataprep(foo = kp.data.co2, predictors = c(preds, 'CO2.Lag'), predictors.op = c('mean'),
                      dependent = c('CO2'), unit.variable = c('ID'), time.variable = c('Year'),
                      treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1991:2004),
                      time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

kp.ch4.dp <- dataprep(foo = kp.data.ch4, predictors = c(preds, 'CH4.Lag'), predictors.op = c('mean'),
                      dependent = c('CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                      treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1991:2004),
                      time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

kp.no2.dp <- dataprep(foo = kp.data.no2, predictors = c(preds, 'NO2.Lag'), predictors.op = c('mean'),
                      dependent = c('NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                      treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1991:2004),
                      time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

kp.ag.ch4.dp <- dataprep(foo = kp.data.ag.ch4, predictors = c(preds, 'AG.CH4.Lag'), predictors.op = c('mean'),
                         dependent = c('AG.CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                         treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1991:2004),
                         time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

kp.ag.no2.dp <- dataprep(foo = kp.data.ag.no2, predictors = c(preds, 'AG.NO2.Lag'), predictors.op = c('mean'),
                         dependent = c('AG.NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                         treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1991:2004),
                         time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

# Creating dataprep objects for Synth for non-KP sample

control.ghg.dp <- dataprep(foo = control.data.ghg, predictors = c(preds, 'GHG.Lag'), predictors.op = c('mean'),
                           dependent = c('GHG'), unit.variable = c('ID'), time.variable = c('Year'),
                           treatment.identifier = 57, controls.identifier = c(1:56, 58:61), time.predictors.prior = c(1991:2004),
                           time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

control.co2.dp <- dataprep(foo = control.data.co2, predictors = c(preds, 'CO2.Lag'), predictors.op = c('mean'),
                           dependent = c('CO2'), unit.variable = c('ID'), time.variable = c('Year'),
                           treatment.identifier = 57, controls.identifier = c(1:56, 58:61), time.predictors.prior = c(1991:2004),
                           time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

control.ch4.dp <- dataprep(foo = control.data.ch4, predictors = c(preds, 'CH4.Lag'), predictors.op = c('mean'),
                           dependent = c('CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                           treatment.identifier = 57, controls.identifier = c(1:56, 58:61), time.predictors.prior = c(1991:2004),
                           time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

control.no2.dp <- dataprep(foo = control.data.no2, predictors = c(preds, 'NO2.Lag'), predictors.op = c('mean'),
                           dependent = c('NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                           treatment.identifier = 57, controls.identifier = c(1:56, 58:61), time.predictors.prior = c(1991:2004),
                           time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

control.ag.ch4.dp <- dataprep(foo = control.data.ag.ch4, predictors = c(preds, 'AG.CH4.Lag'), predictors.op = c('mean'),
                              dependent = c('AG.CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                              treatment.identifier = 57, controls.identifier = c(1:56, 58:61), time.predictors.prior = c(1991:2004),
                              time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

control.ag.no2.dp <- dataprep(foo = control.data.ag.no2, predictors = c(preds, 'AG.NO2.Lag'), predictors.op = c('mean'),
                              dependent = c('AG.NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                              treatment.identifier = 57, controls.identifier = c(1:56, 58:61), time.predictors.prior = c(1991:2004),
                              time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))

# Running Synth for KP sample

kp.ghg.synth <- synth(data.prep.obj = kp.ghg.dp)
png(paste(direc, 'figures/kp_synth_ghg.png', sep = ''))
path.plot(synth.res = kp.ghg.synth, dataprep.res = kp.ghg.dp, Ylab = 'Relative to 1991', Main = 'Total Greenhouse Gas Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

kp.co2.synth <- synth(data.prep.obj = kp.co2.dp)
png(paste(direc, 'figures/kp_synth_co2.png', sep = ''))
path.plot(synth.res = kp.co2.synth, dataprep.res = kp.co2.dp, Ylab = 'Relative to 1991', Main = 'Carbon Dioxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

kp.ch4.synth <- synth(data.prep.obj = kp.ch4.dp)
png(paste(direc, 'figures/kp_synth_ch4.png', sep = ''))
path.plot(synth.res = kp.ch4.synth, dataprep.res = kp.ch4.dp, Ylab = 'Relative to 1991', Main = 'Methane Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

kp.no2.synth <- synth(data.prep.obj = kp.no2.dp)
png(paste(direc, 'figures/kp_synth_no2.png', sep = ''))
path.plot(synth.res = kp.no2.synth, dataprep.res = kp.no2.dp, Ylab = 'Relative to 1991', Main = 'Nitrous Oxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

kp.ag.ch4.synth <- synth(data.prep.obj = kp.ag.ch4.dp)
png(paste(direc, 'figures/kp_synth_ag_ch4.png', sep = ''))
path.plot(synth.res = kp.ag.ch4.synth, dataprep.res = kp.ag.ch4.dp, Ylab = 'Relative to 1991', Main = 'Methane Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

kp.ag.no2.synth <- synth(data.prep.obj = kp.ag.no2.dp)
png(paste(direc, 'figures/kp_synth_ag_no2.png', sep = ''))
path.plot(synth.res = kp.ag.no2.synth, dataprep.res = kp.ag.no2.dp, Ylab = 'Relative to 1991', Main = 'Nitrous Oxide Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

# Running Synth for non-KP sample

control.ghg.synth <- synth(data.prep.obj = control.ghg.dp)
png(paste(direc, 'figures/non_kp_synth_ghg.png', sep = ''))
path.plot(synth.res = control.ghg.synth, dataprep.res = control.ghg.dp, Ylab = 'Relative to 1991', Main = 'Total Greenhouse Gas Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

control.co2.synth <- synth(data.prep.obj = control.co2.dp)
png(paste(direc, 'figures/non_kp_synth_co2.png', sep = ''))
path.plot(synth.res = control.co2.synth, dataprep.res = control.co2.dp, Ylab = 'Relative to 1991', Main = 'Carbon Dioxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

control.ch4.synth <- synth(data.prep.obj = control.ch4.dp)
png(paste(direc, 'figures/non_kp_synth_ch4.png', sep = ''))
path.plot(synth.res = control.ch4.synth, dataprep.res = control.ch4.dp, Ylab = 'Relative to 1991', Main = 'Methane Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

control.no2.synth <- synth(data.prep.obj = control.no2.dp)
png(paste(direc, 'figures/non_kp_synth_no2.png', sep = ''))
path.plot(synth.res = control.no2.synth, dataprep.res = control.no2.dp, Ylab = 'Relative to 1991', Main = 'Nitrous Oxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

control.ag.ch4.synth <- synth(data.prep.obj = control.ag.ch4.dp)
png(paste(direc, 'figures/non_kp_synth_ag_ch4.png', sep = ''))
path.plot(synth.res = control.ag.ch4.synth, dataprep.res = control.ag.ch4.dp, Ylab = 'Relative to 1991', Main = 'Methane Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

control.ag.no2.synth <- synth(data.prep.obj = control.ag.no2.dp)
png(paste(direc, 'figures/non_kp_synth_ag_no2.png', sep = ''))
path.plot(synth.res = control.ag.no2.synth, dataprep.res = control.ag.no2.dp, Ylab = 'Relative to 1991', Main = 'Nitrous Oxide Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

# Placebo testing for KP nations

ghg.placebo.vals <- c()
co2.placebo.vals <- c()
ch4.placebo.vals <- c()
no2.placebo.vals <- c()
ag.ch4.placebo.vals <- c()
ag.no2.placebo.vals <- c()

for (i in 1:20) {
  
  print(i)
  
  cids <- 1:20
  cids <- cids[!cids == i]
  
  ghg.dp <- dataprep(foo = kp.data.ghg, predictors = c(preds, 'GHG.Lag'), predictors.op = c('mean'),
                     dependent = c('GHG'), unit.variable = c('ID'), time.variable = c('Year'),
                     treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                     time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  co2.dp <- dataprep(foo = kp.data.co2, predictors = c(preds, 'CO2.Lag'), predictors.op = c('mean'),
                     dependent = c('CO2'), unit.variable = c('ID'), time.variable = c('Year'),
                     treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                     time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  ch4.dp <- dataprep(foo = kp.data.ch4, predictors = c(preds, 'CH4.Lag'), predictors.op = c('mean'),
                     dependent = c('CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                     treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                     time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  no2.dp <- dataprep(foo = kp.data.no2, predictors = c(preds, 'NO2.Lag'), predictors.op = c('mean'),
                     dependent = c('NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                     treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                     time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  ag.ch4.dp <- dataprep(foo = kp.data.ag.ch4, predictors = c(preds, 'AG.CH4.Lag'), predictors.op = c('mean'),
                        dependent = c('AG.CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                        treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                        time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  ag.no2.dp <- dataprep(foo = kp.data.ag.no2, predictors = c(preds, 'AG.NO2.Lag'), predictors.op = c('mean'),
                        dependent = c('AG.NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                        treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                        time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  ghg.synth <- synth(data.prep.obj = ghg.dp)
  co2.synth <- synth(data.prep.obj = co2.dp)
  ch4.synth <- synth(data.prep.obj = ch4.dp)
  no2.synth <- synth(data.prep.obj = no2.dp)
  ag.ch4.synth <- synth(data.prep.obj = ag.ch4.dp)
  ag.no2.synth <- synth(data.prep.obj = ag.no2.dp)
  
  ghg.placebo.vals <- c(ghg.placebo.vals, ghg.synth$solution.w)
  co2.placebo.vals <- c(co2.placebo.vals, co2.synth$solution.w)
  ch4.placebo.vals <- c(ch4.placebo.vals, ch4.synth$solution.w)
  no2.placebo.vals <- c(no2.placebo.vals, no2.synth$solution.w)
  ag.ch4.placebo.vals <- c(ag.ch4.placebo.vals, ag.ch4.synth$solution.w)
  ag.no2.placebo.vals <- c(ag.no2.placebo.vals, ag.no2.synth$solution.w)
  
}

# Plotting results for KP nations

ghg.plot.data <- c()
co2.plot.data <- c()
ch4.plot.data <- c()
no2.plot.data <- c()
ag.ch4.plot.data <- c()
ag.no2.plot.data <- c()

for (i in 1:20) {
  
  print(i)
  
  ts1 <- c()
  ts2 <- c()
  ts3 <- c()
  ts4 <- c()
  ts5 <- c()
  ts6 <- c()
  
  for (j in 1991:2019) {
    
    a <- 19*i - 18
    b <- 19*i
    
    next.val1 <- sum(ghg.placebo.vals[a:b] * kp.data.ghg[which(kp.data.ghg$Year == j & kp.data.ghg$Country.Name != unique(kp.data.ghg$Country.Name)[i]),]$GHG)
    next.val2 <- sum(co2.placebo.vals[a:b] * kp.data.co2[which(kp.data.co2$Year == j & kp.data.co2$Country.Name != unique(kp.data.co2$Country.Name)[i]),]$CO2)
    next.val3 <- sum(ch4.placebo.vals[a:b] * kp.data.ch4[which(kp.data.ch4$Year == j & kp.data.ch4$Country.Name != unique(kp.data.ch4$Country.Name)[i]),]$CH4)
    next.val4 <- sum(no2.placebo.vals[a:b] * kp.data.no2[which(kp.data.no2$Year == j & kp.data.no2$Country.Name != unique(kp.data.no2$Country.Name)[i]),]$NO2)
    next.val5 <- sum(ag.ch4.placebo.vals[a:b] * kp.data.ag.ch4[which(kp.data.ag.ch4$Year == j & kp.data.ag.ch4$Country.Name != unique(kp.data.ag.ch4$Country.Name)[i]),]$AG.CH4)
    next.val6 <- sum(ag.no2.placebo.vals[a:b] * kp.data.ag.no2[which(kp.data.ag.no2$Year == j & kp.data.ag.no2$Country.Name != unique(kp.data.ag.no2$Country.Name)[i]),]$AG.NO2)
    
    ts1 <- c(ts1, next.val1)
    ts2 <- c(ts2, next.val2)
    ts3 <- c(ts3, next.val3)
    ts4 <- c(ts4, next.val4)
    ts5 <- c(ts5, next.val5)
    ts6 <- c(ts6, next.val6)
    
  }
  
  ghg.plot.data <- c(ghg.plot.data, ts1)
  co2.plot.data <- c(co2.plot.data, ts2)
  ch4.plot.data <- c(ch4.plot.data, ts3)
  no2.plot.data <- c(no2.plot.data, ts4)
  ag.ch4.plot.data <- c(ag.ch4.plot.data, ts5)
  ag.no2.plot.data <- c(ag.no2.plot.data, ts6)
  
}

ghg.plot.df <- as.data.frame(matrix(ghg.plot.data, nrow = 29, ncol = 20))
co2.plot.df <- as.data.frame(matrix(co2.plot.data, nrow = 29, ncol = 20))
ch4.plot.df <- as.data.frame(matrix(ch4.plot.data, nrow = 29, ncol = 20))
no2.plot.df <- as.data.frame(matrix(no2.plot.data, nrow = 29, ncol = 20))
ag.ch4.plot.df <- as.data.frame(matrix(ag.ch4.plot.data, nrow = 29, ncol = 20))
ag.no2.plot.df <- as.data.frame(matrix(ag.no2.plot.data, nrow = 29, ncol = 20))

names(ghg.plot.df) <- unique(kp.data.ghg$Country.Name)
names(co2.plot.df) <- unique(kp.data.ghg$Country.Name)
names(ch4.plot.df) <- unique(kp.data.ghg$Country.Name)
names(no2.plot.df) <- unique(kp.data.ghg$Country.Name)
names(ag.ch4.plot.df) <- unique(kp.data.ghg$Country.Name)
names(ag.no2.plot.df) <- unique(kp.data.ghg$Country.Name)

ghg.plot.df$Year <- 1991:2019
co2.plot.df$Year <- 1991:2019
ch4.plot.df$Year <- 1991:2019
no2.plot.df$Year <- 1991:2019
ag.ch4.plot.df$Year <- 1991:2019
ag.no2.plot.df$Year <- 1991:2019

png(paste(direc, 'figures/kp_placebo_ghg.png', sep = ''))
ggplot(data = ghg.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Total GHG Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/kp_placebo_co2.png', sep = ''))
ggplot(data = ghg.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Carbon Dioxide Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/kp_placebo_ch4.png', sep = ''))
ggplot(data = ch4.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Methane Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/kp_placebo_no2.png', sep = ''))
ggplot(data = no2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/kp_placebo_ag_ch4.png', sep = ''))
ggplot(data = ag.ch4.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Methane Emissions from Agriculture') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/kp_placebo_ag_no2.png', sep = ''))
ggplot(data = ag.no2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions from Agriculture') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

# Placebo testing for non-KP nations

c.ghg.placebo.vals <- c()
c.co2.placebo.vals <- c()
c.ch4.placebo.vals <- c()
c.no2.placebo.vals <- c()
c.ag.ch4.placebo.vals <- c()
c.ag.no2.placebo.vals <- c()

for (i in 1:61) {
  
  print(i)
  
  cids <- 1:61
  cids <- cids[!cids == i]
  
  control.ghg.dp <- dataprep(foo = control.data.ghg, predictors = c(preds, 'GHG.Lag'), predictors.op = c('mean'),
                             dependent = c('GHG'), unit.variable = c('ID'), time.variable = c('Year'),
                             treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                             time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  control.co2.dp <- dataprep(foo = control.data.co2, predictors = c(preds, 'CO2.Lag'), predictors.op = c('mean'),
                             dependent = c('CO2'), unit.variable = c('ID'), time.variable = c('Year'),
                             treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                             time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  control.ch4.dp <- dataprep(foo = control.data.ch4, predictors = c(preds, 'CH4.Lag'), predictors.op = c('mean'),
                             dependent = c('CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                             treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                             time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  control.no2.dp <- dataprep(foo = control.data.no2, predictors = c(preds, 'NO2.Lag'), predictors.op = c('mean'),
                             dependent = c('NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                             treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                             time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  control.ag.ch4.dp <- dataprep(foo = control.data.ag.ch4, predictors = c(preds, 'AG.CH4.Lag'), predictors.op = c('mean'),
                                dependent = c('AG.CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                                treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                                time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  control.ag.no2.dp <- dataprep(foo = control.data.ag.no2, predictors = c(preds, 'AG.NO2.Lag'), predictors.op = c('mean'),
                                dependent = c('AG.NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                                treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1991:2004),
                                time.optimize.ssr = c(1991:2004), unit.names.variable = 'Country.Name', time.plot = c(1991:2019))
  
  c.ghg.synth <- synth(data.prep.obj = control.ghg.dp, optimxmethod = 'All')
  c.co2.synth <- synth(data.prep.obj = control.co2.dp, optimxmethod = 'All')
  c.ch4.synth <- synth(data.prep.obj = control.ch4.dp, optimxmethod = 'All')
  c.no2.synth <- synth(data.prep.obj = control.no2.dp, optimxmethod = 'All')
  c.ag.ch4.synth <- synth(data.prep.obj = control.ag.ch4.dp, optimxmethod = 'All')
  c.ag.no2.synth <- synth(data.prep.obj = control.ag.no2.dp, optimxmethod = 'All')
  
  c.ghg.placebo.vals <- c(c.ghg.placebo.vals, c.ghg.synth$solution.w)
  c.co2.placebo.vals <- c(c.co2.placebo.vals, c.co2.synth$solution.w)
  c.ch4.placebo.vals <- c(c.ch4.placebo.vals, c.ch4.synth$solution.w)
  c.no2.placebo.vals <- c(c.no2.placebo.vals, c.no2.synth$solution.w)
  c.ag.ch4.placebo.vals <- c(c.ag.ch4.placebo.vals, c.ag.ch4.synth$solution.w)
  c.ag.no2.placebo.vals <- c(c.ag.no2.placebo.vals, c.ag.no2.synth$solution.w)

}

# Plotting results for non-KP nations

c.ghg.plot.data <- c()
c.co2.plot.data <- c()
c.ch4.plot.data <- c()
c.no2.plot.data <- c()
c.ag.ch4.plot.data <- c()
c.ag.no2.plot.data <- c()

for (i in 1:61) {
  
  print(i)
  
  ts1 <- c()
  ts2 <- c()
  ts3 <- c()
  ts4 <- c()
  ts5 <- c()
  ts6 <- c()
  
  for (j in 1991:2019) {
    
    a <- 60*i - 59
    b <- 60*i
    
    next.val1 <- sum(c.ghg.placebo.vals[a:b] * control.data.ghg[which(control.data.ghg$Year == j & control.data.ghg$Country.Name != unique(control.data.ghg$Country.Name)[i]),]$GHG)
    next.val2 <- sum(c.co2.placebo.vals[a:b] * control.data.co2[which(control.data.co2$Year == j & control.data.co2$Country.Name != unique(control.data.co2$Country.Name)[i]),]$CO2)
    next.val3 <- sum(c.ch4.placebo.vals[a:b] * control.data.ch4[which(control.data.ch4$Year == j & control.data.ch4$Country.Name != unique(control.data.ch4$Country.Name)[i]),]$CH4)
    next.val4 <- sum(c.no2.placebo.vals[a:b] * control.data.no2[which(control.data.no2$Year == j & control.data.no2$Country.Name != unique(control.data.no2$Country.Name)[i]),]$NO2)
    next.val5 <- sum(c.ag.ch4.placebo.vals[a:b] * control.data.ag.ch4[which(control.data.ag.ch4$Year == j & control.data.ag.ch4$Country.Name != unique(control.data.ag.ch4$Country.Name)[i]),]$AG.CH4)
    next.val6 <- sum(c.ag.no2.placebo.vals[a:b] * control.data.ag.no2[which(control.data.ag.no2$Year == j & control.data.ag.no2$Country.Name != unique(control.data.ag.no2$Country.Name)[i]),]$AG.NO2)
    
    ts1 <- c(ts1, next.val1)
    ts2 <- c(ts2, next.val2)
    ts3 <- c(ts3, next.val3)
    ts4 <- c(ts4, next.val4)
    ts5 <- c(ts5, next.val5)
    ts6 <- c(ts6, next.val6)
    
  }
  
  c.ghg.plot.data <- c(c.ghg.plot.data, ts1)
  c.co2.plot.data <- c(c.co2.plot.data, ts2)
  c.ch4.plot.data <- c(c.ch4.plot.data, ts3)
  c.no2.plot.data <- c(c.no2.plot.data, ts4)
  c.ag.ch4.plot.data <- c(c.ag.ch4.plot.data, ts5)
  c.ag.no2.plot.data <- c(c.ag.no2.plot.data, ts6)
  
}

c.ghg.plot.df <- as.data.frame(matrix(c.ghg.plot.data, nrow = 29, ncol = 61))
c.co2.plot.df <- as.data.frame(matrix(c.co2.plot.data, nrow = 29, ncol = 61))
c.ch4.plot.df <- as.data.frame(matrix(c.ch4.plot.data, nrow = 29, ncol = 61))
c.no2.plot.df <- as.data.frame(matrix(c.no2.plot.data, nrow = 29, ncol = 61))
c.ag.ch4.plot.df <- as.data.frame(matrix(c.ag.ch4.plot.data, nrow = 29, ncol = 61))
c.ag.no2.plot.df <- as.data.frame(matrix(c.ag.no2.plot.data, nrow = 29, ncol = 61))

names(c.ghg.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.co2.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.ch4.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.no2.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.ag.ch4.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.ag.no2.plot.df) <- unique(control.data.ghg$Country.Name)

c.ghg.plot.df$Year <- 1991:2019
c.co2.plot.df$Year <- 1991:2019
c.ch4.plot.df$Year <- 1991:2019
c.no2.plot.df$Year <- 1991:2019
c.ag.ch4.plot.df$Year <- 1991:2019
c.ag.no2.plot.df$Year <- 1991:2019

png(paste(direc, 'figures/non_kp_placebo_ghg.png', sep = ''))
ggplot(data = c.ghg.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Total GHG Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_co2.png', sep = ''))
ggplot(data = c.co2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Carbon Dioxide Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_ch4.png', sep = ''))
ggplot(data = c.ch4.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Methane Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_no2.png', sep = ''))
ggplot(data = c.no2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_ag_ch4.png', sep = ''))
ggplot(data = c.ag.ch4.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Methane Emissions from Agriculture') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_ag_no2.png', sep = ''))
ggplot(data = c.ag.no2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions from Agriculture') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

# Creating multiplots as an alternative for the paper

png(paste(direc, 'figures/kp_synth.png', sep = ''))
par(mfrow = c(3,2))

path.plot(synth.res = kp.ghg.synth, dataprep.res = kp.ghg.dp, Ylab = 'Relative to 1991', Main = 'Total Greenhouse Gas Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = kp.co2.synth, dataprep.res = kp.co2.dp, Ylab = 'Relative to 1991', Main = 'Carbon Dioxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = kp.ch4.synth, dataprep.res = kp.ch4.dp, Ylab = 'Relative to 1991', Main = 'Methane Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = kp.no2.synth, dataprep.res = kp.no2.dp, Ylab = 'Relative to 1991', Main = 'Nitrous Oxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = kp.ag.ch4.synth, dataprep.res = kp.ag.ch4.dp, Ylab = 'Relative to 1991', Main = 'Methane Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = kp.ag.no2.synth, dataprep.res = kp.ag.no2.dp, Ylab = 'Relative to 1991', Main = 'Nitrous Oxide Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

png(paste(direc, 'figures/non_kp_synth.png', sep = ''))
par(mfrow = c(3,2))

path.plot(synth.res = control.ghg.synth, dataprep.res = control.ghg.dp, Ylab = 'Relative to 1991', Main = 'Total Greenhouse Gas Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = control.co2.synth, dataprep.res = control.co2.dp, Ylab = 'Relative to 1991', Main = 'Carbon Dioxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = control.ch4.synth, dataprep.res = control.ch4.dp, Ylab = 'Relative to 1991', Main = 'Methane Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = control.no2.synth, dataprep.res = control.no2.dp, Ylab = 'Relative to 1991', Main = 'Nitrous Oxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = control.ag.ch4.synth, dataprep.res = control.ag.ch4.dp, Ylab = 'Relative to 1991', Main = 'Methane Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)

path.plot(synth.res = control.ag.no2.synth, dataprep.res = control.ag.no2.dp, Ylab = 'Relative to 1991', Main = 'Nitrous Oxide Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
dev.off()

png(paste(direc, 'figures/kp_placebo.png', sep = ''))
par(mfrow = c(3,2))

ggplot(data = ghg.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Total GHG Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = ghg.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Carbon Dioxide Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = ch4.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Methane Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = no2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = ag.ch4.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Methane Emissions from Agriculture') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = ag.no2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions from Agriculture') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Austria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Belgium`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bulgaria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Canada`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Denmark`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Finland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `France`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Germany`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ireland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Italy`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Japan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Netherlands`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Poland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Portugal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Romania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Spain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sweden`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Switzerland`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Kingdom`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

png(paste(direc, 'figures/non_kp_placebo.png', sep = ''))
par(mfrow = c(3,2))

ggplot(data = c.ghg.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Total GHG Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = c.co2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Carbon Dioxide Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = c.ch4.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Methane Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = c.no2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = c.ag.ch4.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Methane Emissions from Agriculture') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)

ggplot(data = c.ag.no2.plot.df, aes(x = Year, y = `United States`)) + 
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions from Agriculture') + 
  ylab('Relative to 1991') + 
  geom_line(aes(x = Year, y = `Albania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Argentina`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bahrain`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bangladesh`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Benin`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Bolivia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Botswana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Brazil`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cameroon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `China`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Colombia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Dem. Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Congo, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Costa Rica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Cote d'Ivoire`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Indonesia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Iraq`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jamaica`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Jordan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Kenya`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Korea, Rep.`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Lebanon`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Malaysia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mauritius`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mexico`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Morocco`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Mozambique`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Namibia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nepal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nicaragua`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Nigeria`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Oman`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Pakistan`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Paraguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Peru`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Philippines`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Saudi Arabia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Senegal`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `South Africa`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Sri Lanka`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tanzania`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Thailand`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Trinidad and Tobago`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Tunisia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Turkiye`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United Arab Emirates`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Uruguay`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Vietnam`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zambia`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `Zimbabwe`), size = 1, alpha = 1, color = 'gray') + 
  geom_line(aes(x = Year, y = `United States`), size = 1, alpha = 1, color = 'black') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  geom_vline(xintercept = 2004)
dev.off()

# Cute summary stats table

keepers <- c('GHG', 'CO2', 'CH4', 'NO2', 'AG.CH4', 'AG.NO2', 'GDP.pc', 'Forest.Rents', 'Electricity.Consumption.pc', 'Population', 'Rural', 'Urban')
new_names <- c('Total Greenhouse Gas Emissions', 'Carbon Dioxide', 'Methane', 'Nitrous Oxide', 'Methane from Agriculture', 'Nitrous Oxide from Agriculture', 'GDP', 'Forest Rents', 'Electricity Consumption', 'Population', 'Rural Population', 'Urban Population')

kp.sumdata <- kp.data.ghg[which(kp.data.ghg$Country.Name != 'United States'), names(kp.data.ghg) %in% keepers]
c.sumdata <- control.data.ghg[which(control.data.ghg$Country.Name != 'United States'), names(control.data.ghg) %in% keepers]
us.sumdata <- control.data.ghg[which(control.data.ghg$Country.Name == 'United States'), names(control.data.ghg) %in% keepers]

names(kp.sumdata) <- new_names
names(c.sumdata) <- new_names
names(us.sumdata) <- new_names

png(paste(direc, 'figures/kp_sum_stats.png', sep = ''))
datasummary_skim(kp.sumdata, fmt = '%.3f')
dev.off()

png(paste(direc, 'figures/non_kp_sum_stats.png', sep = ''))
datasummary_skim(c.sumdata, fmt = '%.3f')
dev.off()

png(paste(direc, 'figures/us_sum_stats.png', sep = ''))
datasummary_skim(us.sumdata, fmt = '%.3f')
dev.off()

