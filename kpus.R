# This script performs the econometrics analysis for the US + KP paper without imputed data

# Loading libraries

library(dplyr)
library(Synth)
library(ggrepel)
library(ggplot2)
library(patchwork)
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

# Transforming outcome variables

ghg <- c()
co2 <- c()
ch4 <- c()
no2 <- c()
ch4.ag <- c()
no2.ag <- c()

for (i in 1:dim(data)[1]) {

  tmp.df <- data[which(data$Country.Name == data$Country.Name[i] & data$Year == 1990),]

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

  tmp.df <- data[which(data$Country.Name == data$Country.Name[i] & data$Year == 1990),]
  
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

# Subset for 1990 - 2019

data <- data[which(data$Year %in% 1990:2019),]

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

drop.kp <- c()

for (nation in unique(kp.data.ghg$Country.Name)) {

  u1 <- dim(kp.data.ghg[which(kp.data.ghg$Country.Name == 'United States'),])[1]
  u2 <- dim(kp.data.co2[which(kp.data.co2$Country.Name == 'United States'),])[1]
  u3 <- dim(kp.data.ch4[which(kp.data.ch4$Country.Name == 'United States'),])[1]
  u4 <- dim(kp.data.no2[which(kp.data.no2$Country.Name == 'United States'),])[1]
  u5 <- dim(kp.data.ag.ch4[which(kp.data.ag.ch4$Country.Name == 'United States'),])[1]
  u6 <- dim(kp.data.ag.no2[which(kp.data.ag.no2$Country.Name == 'United States'),])[1]

  x1 <- dim(kp.data.ghg[which(kp.data.ghg$Country.Name == nation),])[1]
  x2 <- dim(kp.data.co2[which(kp.data.co2$Country.Name == nation),])[1]
  x3 <- dim(kp.data.ch4[which(kp.data.ch4$Country.Name == nation),])[1]
  x4 <- dim(kp.data.no2[which(kp.data.no2$Country.Name == nation),])[1]
  x5 <- dim(kp.data.ag.ch4[which(kp.data.ag.ch4$Country.Name == nation),])[1]
  x6 <- dim(kp.data.ag.no2[which(kp.data.ag.no2$Country.Name == nation),])[1]

  val <- as.integer(u1 == x1) * as.integer(u2 == x2) * as.integer(u3 == x3) * as.integer(u4 == x4) * as.integer(u5 == x5) * as.integer(u6 == x6)

  if (val != 1) {drop.kp <- c(drop.kp, nation)}

}

drop.c <- c()

for (nation in unique(control.data.ghg$Country.Name)) {

  u1 <- dim(control.data.ghg[which(control.data.ghg$Country.Name == 'United States'),])[1]
  u2 <- dim(control.data.co2[which(control.data.co2$Country.Name == 'United States'),])[1]
  u3 <- dim(control.data.ch4[which(control.data.ch4$Country.Name == 'United States'),])[1]
  u4 <- dim(control.data.no2[which(control.data.no2$Country.Name == 'United States'),])[1]
  u5 <- dim(control.data.ag.ch4[which(control.data.ag.ch4$Country.Name == 'United States'),])[1]
  u6 <- dim(control.data.ag.no2[which(control.data.ag.no2$Country.Name == 'United States'),])[1]

  x1 <- dim(control.data.ghg[which(control.data.ghg$Country.Name == nation),])[1]
  x2 <- dim(control.data.co2[which(control.data.co2$Country.Name == nation),])[1]
  x3 <- dim(control.data.ch4[which(control.data.ch4$Country.Name == nation),])[1]
  x4 <- dim(control.data.no2[which(control.data.no2$Country.Name == nation),])[1]
  x5 <- dim(control.data.ag.ch4[which(control.data.ag.ch4$Country.Name == nation),])[1]
  x6 <- dim(control.data.ag.no2[which(control.data.ag.no2$Country.Name == nation),])[1]

  val <- as.integer(u1 == x1) * as.integer(u2 == x2) * as.integer(u3 == x3) * as.integer(u4 == x4) * as.integer(u5 == x5) * as.integer(u6 == x6)

  if (val != 1) {drop.c <- c(drop.c, nation)}

}

# Based on the above

kp.data.ghg <- kp.data.ghg[which(!kp.data.ghg$Country.Name %in% drop.kp),]
kp.data.co2 <- kp.data.co2[which(!kp.data.co2$Country.Name %in% drop.kp),]
kp.data.ch4 <- kp.data.ch4[which(!kp.data.ch4$Country.Name %in% drop.kp),]
kp.data.no2 <- kp.data.no2[which(!kp.data.no2$Country.Name %in% drop.kp),]
kp.data.ag.ch4 <- kp.data.ag.ch4[which(!kp.data.ag.ch4$Country.Name %in% drop.kp),]
kp.data.ag.no2 <- kp.data.ag.no2[which(!kp.data.ag.no2$Country.Name %in% drop.kp),]

control.data.ghg <- control.data.ghg[which(!control.data.ghg$Country.Name %in% drop.c),]
control.data.co2 <- control.data.co2[which(!control.data.co2$Country.Name %in% drop.c),]
control.data.ch4 <- control.data.ch4[which(!control.data.ch4$Country.Name %in% drop.c),]
control.data.no2 <- control.data.no2[which(!control.data.no2$Country.Name %in% drop.c),]
control.data.ag.ch4 <- control.data.ag.ch4[which(!control.data.ag.ch4$Country.Name %in% drop.c),]
control.data.ag.no2 <- control.data.ag.no2[which(!control.data.ag.no2$Country.Name %in% drop.c),]

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

  control.data.ghg <- rbind(control.data.ghg, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.ghg))])
  control.data.co2 <- rbind(control.data.co2, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.co2))])
  control.data.ch4 <- rbind(control.data.ch4, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.ch4))])
  control.data.no2 <- rbind(control.data.no2, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.no2))])
  control.data.ag.ch4 <- rbind(control.data.ag.ch4, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.ag.ch4))])
  control.data.ag.no2 <- rbind(control.data.ag.no2, control.data[which(control.data$Country.Name == nation & control.data$Year %in% 2015:2019), which(names(control.data) %in% names(control.data.ag.no2))])

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
                      treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1990:2004),
                      time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

kp.co2.dp <- dataprep(foo = kp.data.co2, predictors = c(preds, 'CO2.Lag'), predictors.op = c('mean'),
                      dependent = c('CO2'), unit.variable = c('ID'), time.variable = c('Year'),
                      treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1990:2004),
                      time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

kp.ch4.dp <- dataprep(foo = kp.data.ch4, predictors = c(preds, 'CH4.Lag'), predictors.op = c('mean'),
                      dependent = c('CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                      treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1990:2004),
                      time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

kp.no2.dp <- dataprep(foo = kp.data.no2, predictors = c(preds, 'NO2.Lag'), predictors.op = c('mean'),
                      dependent = c('NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                      treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1990:2004),
                      time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

kp.ag.ch4.dp <- dataprep(foo = kp.data.ag.ch4, predictors = c(preds, 'AG.CH4.Lag'), predictors.op = c('mean'),
                         dependent = c('AG.CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                         treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1990:2004),
                         time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

kp.ag.no2.dp <- dataprep(foo = kp.data.ag.no2, predictors = c(preds, 'AG.NO2.Lag'), predictors.op = c('mean'),
                         dependent = c('AG.NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                         treatment.identifier = 20, controls.identifier = c(1:19), time.predictors.prior = c(1990:2004),
                         time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

# Creating dataprep objects for Synth for non-KP sample

control.ghg.dp <- dataprep(foo = control.data.ghg, predictors = c(preds, 'GHG.Lag'), predictors.op = c('mean'),
                           dependent = c('GHG'), unit.variable = c('ID'), time.variable = c('Year'),
                           treatment.identifier = 55, controls.identifier = c(1:54, 56:59), time.predictors.prior = c(1990:2004),
                           time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

control.co2.dp <- dataprep(foo = control.data.co2, predictors = c(preds, 'CO2.Lag'), predictors.op = c('mean'),
                           dependent = c('CO2'), unit.variable = c('ID'), time.variable = c('Year'),
                           treatment.identifier = 55, controls.identifier = c(1:54, 56:59), time.predictors.prior = c(1990:2004),
                           time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

control.ch4.dp <- dataprep(foo = control.data.ch4, predictors = c(preds, 'CH4.Lag'), predictors.op = c('mean'),
                           dependent = c('CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                           treatment.identifier = 55, controls.identifier = c(1:54, 56:59), time.predictors.prior = c(1990:2004),
                           time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

control.no2.dp <- dataprep(foo = control.data.no2, predictors = c(preds, 'NO2.Lag'), predictors.op = c('mean'),
                           dependent = c('NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                           treatment.identifier = 55, controls.identifier = c(1:54, 56:59), time.predictors.prior = c(1990:2004),
                           time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

control.ag.ch4.dp <- dataprep(foo = control.data.ag.ch4, predictors = c(preds, 'AG.CH4.Lag'), predictors.op = c('mean'),
                              dependent = c('AG.CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                              treatment.identifier = 55, controls.identifier = c(1:54, 56:59), time.predictors.prior = c(1990:2004),
                              time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

control.ag.no2.dp <- dataprep(foo = control.data.ag.no2, predictors = c(preds, 'AG.NO2.Lag'), predictors.op = c('mean'),
                              dependent = c('AG.NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                              treatment.identifier = 55, controls.identifier = c(1:54, 56:59), time.predictors.prior = c(1990:2004),
                              time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

# Running Synth for KP sample

kp.ghg.synth <- synth(data.prep.obj = kp.ghg.dp, optimxmethod = 'All')
png(paste(direc, 'figures/kp_synth_ghg.png', sep = ''))
path.plot(synth.res = kp.ghg.synth, dataprep.res = kp.ghg.dp, Ylab = 'Relative to 1990', Main = 'Total Greenhouse Gas Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

kp.co2.synth <- synth(data.prep.obj = kp.co2.dp, optimxmethod = 'All')
png(paste(direc, 'figures/kp_synth_co2.png', sep = ''))
path.plot(synth.res = kp.co2.synth, dataprep.res = kp.co2.dp, Ylab = 'Relative to 1990', Main = 'Carbon Dioxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

kp.ch4.synth <- synth(data.prep.obj = kp.ch4.dp, optimxmethod = 'All')
png(paste(direc, 'figures/kp_synth_ch4.png', sep = ''))
path.plot(synth.res = kp.ch4.synth, dataprep.res = kp.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

kp.no2.synth <- synth(data.prep.obj = kp.no2.dp, optimxmethod = 'All')
png(paste(direc, 'figures/kp_synth_no2.png', sep = ''))
path.plot(synth.res = kp.no2.synth, dataprep.res = kp.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

kp.ag.ch4.synth <- synth(data.prep.obj = kp.ag.ch4.dp, optimxmethod = 'All')
png(paste(direc, 'figures/kp_synth_ag_ch4.png', sep = ''))
path.plot(synth.res = kp.ag.ch4.synth, dataprep.res = kp.ag.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

kp.ag.no2.synth <- synth(data.prep.obj = kp.ag.no2.dp, optimxmethod = 'All')
png(paste(direc, 'figures/kp_synth_ag_no2.png', sep = ''))
path.plot(synth.res = kp.ag.no2.synth, dataprep.res = kp.ag.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

# Running Synth for non-KP sample

control.ghg.synth <- synth(data.prep.obj = control.ghg.dp, optimxmethod = 'All')
png(paste(direc, 'figures/non_kp_synth_ghg.png', sep = ''))
path.plot(synth.res = control.ghg.synth, dataprep.res = control.ghg.dp, Ylab = 'Relative to 1990', Main = 'Total Greenhouse Gas Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

control.co2.synth <- synth(data.prep.obj = control.co2.dp, optimxmethod = 'All')
png(paste(direc, 'figures/non_kp_synth_co2.png', sep = ''))
path.plot(synth.res = control.co2.synth, dataprep.res = control.co2.dp, Ylab = 'Relative to 1990', Main = 'Carbon Dioxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

control.ch4.synth <- synth(data.prep.obj = control.ch4.dp, optimxmethod = 'All')
png(paste(direc, 'figures/non_kp_synth_ch4.png', sep = ''))
path.plot(synth.res = control.ch4.synth, dataprep.res = control.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

control.no2.synth <- synth(data.prep.obj = control.no2.dp, optimxmethod = 'All')
png(paste(direc, 'figures/non_kp_synth_no2.png', sep = ''))
path.plot(synth.res = control.no2.synth, dataprep.res = control.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

control.ag.ch4.synth <- synth(data.prep.obj = control.ag.ch4.dp, optimxmethod = 'All')
png(paste(direc, 'figures/non_kp_synth_ag_ch4.png', sep = ''))
path.plot(synth.res = control.ag.ch4.synth, dataprep.res = control.ag.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

control.ag.no2.synth <- synth(data.prep.obj = control.ag.no2.dp, optimxmethod = 'All')
png(paste(direc, 'figures/non_kp_synth_ag_no2.png', sep = ''))
path.plot(synth.res = control.ag.no2.synth, dataprep.res = control.ag.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)
dev.off()

png(paste(direc, 'figures/non_kp_synth.png', sep = ''))
par(mfrow = c(2,3))

path.plot(synth.res = control.ghg.synth, dataprep.res = control.ghg.dp, Ylab = 'Relative to 1990', Main = 'Total Greenhouse Gas Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.co2.synth, dataprep.res = control.co2.dp, Ylab = 'Relative to 1990', Main = 'Carbon Dioxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.ch4.synth, dataprep.res = control.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.no2.synth, dataprep.res = control.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.ag.ch4.synth, dataprep.res = control.ag.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.ag.no2.synth, dataprep.res = control.ag.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

dev.off()

# Who comprises the synthetic US?

sr.vals <- rev(sort(kp.ghg.synth$solution.w))
sr.ids <- rev(order(kp.ghg.synth$solution.w))
sr.labs <- unique(kp.data.ghg$Country.Name)[sr.ids]
sr.df1 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df1) <- c('Value', 'ID', 'Label')
sr.df1$ID <- as.integer(sr.df1$ID)
sr.df1$Value <- as.numeric(sr.df1$Value)
sr.df1$Rank <- 1:length(sr.df1$ID)

sr.vals <- rev(sort(kp.co2.synth$solution.w))
sr.ids <- rev(order(kp.co2.synth$solution.w))
sr.labs <- unique(kp.data.co2$Country.Name)[sr.ids]
sr.df2 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df2) <- c('Value', 'ID', 'Label')
sr.df2$ID <- as.integer(sr.df2$ID)
sr.df2$Value <- as.numeric(sr.df2$Value)
sr.df2$Rank <- 1:length(sr.df2$ID)

sr.vals <- rev(sort(kp.ch4.synth$solution.w))
sr.ids <- rev(order(kp.ch4.synth$solution.w))
sr.labs <- unique(kp.data.ch4$Country.Name)[sr.ids]
sr.df3 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df3) <- c('Value', 'ID', 'Label')
sr.df3$ID <- as.integer(sr.df3$ID)
sr.df3$Value <- as.numeric(sr.df3$Value)
sr.df3$Rank <- 1:length(sr.df3$ID)

sr.vals <- rev(sort(kp.no2.synth$solution.w))
sr.ids <- rev(order(kp.no2.synth$solution.w))
sr.labs <- unique(kp.data.no2$Country.Name)[sr.ids]
sr.df4 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df4) <- c('Value', 'ID', 'Label')
sr.df4$ID <- as.integer(sr.df4$ID)
sr.df4$Value <- as.numeric(sr.df4$Value)
sr.df4$Rank <- 1:length(sr.df4$ID)

sr.vals <- rev(sort(kp.ag.ch4.synth$solution.w))
sr.ids <- rev(order(kp.ag.ch4.synth$solution.w))
sr.labs <- unique(kp.data.ag.ch4$Country.Name)[sr.ids]
sr.df5 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df5) <- c('Value', 'ID', 'Label')
sr.df5$ID <- as.integer(sr.df5$ID)
sr.df5$Value <- as.numeric(sr.df5$Value)
sr.df5$Rank <- 1:length(sr.df5$ID)

sr.vals <- rev(sort(kp.ag.no2.synth$solution.w))
sr.ids <- rev(order(kp.ag.no2.synth$solution.w))
sr.labs <- unique(kp.data.ag.no2$Country.Name)[sr.ids]
sr.df6 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df6) <- c('Value', 'ID', 'Label')
sr.df6$ID <- as.integer(sr.df6$ID)
sr.df6$Value <- as.numeric(sr.df6$Value)
sr.df6$Rank <- 1:length(sr.df6$ID)

f1 <- ggplot(data = sr.df1[1:5,], aes(x = Rank, y = Value)) +
      theme_bw() +
      ggtitle('Total Greenhouse Gas Emissions') +
      ylab('Contribution to Synthetic US') +
      xlab('Contribution Rank') +
      geom_point(size = 2, pch = 19) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = 'none') +
      geom_text_repel(aes(label = Label)) +
      ylim(c(0,1))

f2 <- ggplot(data = sr.df2[1:5,], aes(x = Rank, y = Value)) +
      theme_bw() +
      ggtitle('Carbon Dioxide Emissions') +
      ylab('Contribution to Synthetic US') +
      xlab('Contribution Rank') +
      geom_point(size = 2, pch = 19) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = 'none') +
      geom_text_repel(aes(label = Label)) +
      ylim(c(0,1))

f3 <- ggplot(data = sr.df3[1:5,], aes(x = Rank, y = Value)) +
      theme_bw() +
      ggtitle('Methane Emissions') +
      ylab('Contribution to Synthetic US') +
      xlab('Contribution Rank') +
      geom_point(size = 2, pch = 19) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = 'none') +
      geom_text_repel(aes(label = Label)) +
      ylim(c(0,1))

f4 <- ggplot(data = sr.df4[1:5,], aes(x = Rank, y = Value)) +
      theme_bw() +
      ggtitle('Nitrous Oxide Emissions') +
      ylab('Contribution to Synthetic US') +
      xlab('Contribution Rank') +
      geom_point(size = 2, pch = 19) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = 'none') +
      geom_text_repel(aes(label = Label)) +
      ylim(c(0,1))

f5 <- ggplot(data = sr.df5[1:5,], aes(x = Rank, y = Value)) +
      theme_bw() +
      ggtitle('Methane Emissions from Agriculture') +
      ylab('Contribution to Synthetic US') +
      xlab('Contribution Rank') +
      geom_point(size = 2, pch = 19) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = 'none') +
      geom_text_repel(aes(label = Label)) +
      ylim(c(0,1))

f6 <- ggplot(data = sr.df6[1:5,], aes(x = Rank, y = Value)) +
      theme_bw() +
      ggtitle('Nitrous Oxide Emissions from Agriculture') +
      ylab('Contribution to Synthetic US') +
      xlab('Contribution Rank') +
      geom_point(size = 2, pch = 19) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = 'none') +
      geom_text_repel(aes(label = Label)) +
      ylim(c(0,1))

png(paste(direc, 'figures/kp_contributions.png', sep = ''))
par(mfrow = c(2,3))
f1 + f2 + f3 + f4 + f5 + f6 + plot_layout(ncol = 2)
dev.off()

f1 + f2 + f3 + f4 + f5 + f6 + plot_layout(ncol = 2)

sr.vals <- rev(sort(control.ghg.synth$solution.w))
sr.idsx <- rev(order(control.ghg.synth$solution.w))
sr.ids <- c()
for (x in sr.idsx) {if (x > 54) {sr.ids <- c(sr.ids, x+1)} else {sr.ids <- c(sr.ids, x)}}
sr.labs <- unique(control.data.ghg$Country.Name)[sr.ids]
sr.df1 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df1) <- c('Value', 'ID', 'Label')
sr.df1$ID <- as.integer(sr.df1$ID)
sr.df1$Value <- as.numeric(sr.df1$Value)
sr.df1$Rank <- 1:length(sr.df1$ID)

sr.vals <- rev(sort(control.co2.synth$solution.w))
sr.idsx <- rev(order(control.co2.synth$solution.w))
sr.ids <- c()
for (x in sr.idsx) {if (x > 54) {sr.ids <- c(sr.ids, x+1)} else {sr.ids <- c(sr.ids, x)}}
sr.labs <- unique(control.data.co2$Country.Name)[sr.ids]
sr.df2 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df2) <- c('Value', 'ID', 'Label')
sr.df2$ID <- as.integer(sr.df2$ID)
sr.df2$Value <- as.numeric(sr.df2$Value)
sr.df2$Rank <- 1:length(sr.df2$ID)

sr.vals <- rev(sort(control.ch4.synth$solution.w))
sr.idsx <- rev(order(control.ch4.synth$solution.w))
sr.ids <- c()
for (x in sr.idsx) {if (x > 54) {sr.ids <- c(sr.ids, x+1)} else {sr.ids <- c(sr.ids, x)}}
sr.labs <- unique(control.data.ch4$Country.Name)[sr.ids]
sr.df3 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df3) <- c('Value', 'ID', 'Label')
sr.df3$ID <- as.integer(sr.df3$ID)
sr.df3$Value <- as.numeric(sr.df3$Value)
sr.df3$Rank <- 1:length(sr.df3$ID)

sr.vals <- rev(sort(control.no2.synth$solution.w))
sr.idsx <- rev(order(control.no2.synth$solution.w))
sr.ids <- c()
for (x in sr.idsx) {if (x > 54) {sr.ids <- c(sr.ids, x+1)} else {sr.ids <- c(sr.ids, x)}}
sr.labs <- unique(control.data.no2$Country.Name)[sr.ids]
sr.df4 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df4) <- c('Value', 'ID', 'Label')
sr.df4$ID <- as.integer(sr.df4$ID)
sr.df4$Value <- as.numeric(sr.df4$Value)
sr.df4$Rank <- 1:length(sr.df4$ID)

sr.vals <- rev(sort(control.ag.ch4.synth$solution.w))
sr.idsx <- rev(order(control.ag.ch4.synth$solution.w))
sr.ids <- c()
for (x in sr.idsx) {if (x > 54) {sr.ids <- c(sr.ids, x+1)} else {sr.ids <- c(sr.ids, x)}}
sr.labs <- unique(control.data.ag.ch4$Country.Name)[sr.ids]
sr.df5 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df5) <- c('Value', 'ID', 'Label')
sr.df5$ID <- as.integer(sr.df5$ID)
sr.df5$Value <- as.numeric(sr.df5$Value)
sr.df5$Rank <- 1:length(sr.df5$ID)

sr.vals <- rev(sort(control.ag.no2.synth$solution.w))
sr.idsx <- rev(order(control.ag.no2.synth$solution.w))
sr.ids <- c()
for (x in sr.idsx) {if (x > 54) {sr.ids <- c(sr.ids, x+1)} else {sr.ids <- c(sr.ids, x)}}
sr.labs <- unique(control.data.ag.no2$Country.Name)[sr.ids]
sr.df6 <- as.data.frame(cbind(sr.vals, sr.ids, sr.labs))
names(sr.df6) <- c('Value', 'ID', 'Label')
sr.df6$ID <- as.integer(sr.df6$ID)
sr.df6$Value <- as.numeric(sr.df6$Value)
sr.df6$Rank <- 1:length(sr.df6$ID)

f1 <- ggplot(data = sr.df1[1:5,], aes(x = Rank, y = Value)) +
  theme_bw() +
  ggtitle('Total Greenhouse Gas Emissions') +
  ylab('Contribution to Synthetic US') +
  xlab('Contribution Rank') +
  geom_point(size = 2, pch = 19) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = Label)) +
  ylim(c(0,1))

f2 <- ggplot(data = sr.df2[1:5,], aes(x = Rank, y = Value)) +
  theme_bw() +
  ggtitle('Carbon Dioxide Emissions') +
  ylab('Contribution to Synthetic US') +
  xlab('Contribution Rank') +
  geom_point(size = 2, pch = 19) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = Label)) +
  ylim(c(0,1))

f3 <- ggplot(data = sr.df3[1:5,], aes(x = Rank, y = Value)) +
  theme_bw() +
  ggtitle('Methane Emissions') +
  ylab('Contribution to Synthetic US') +
  xlab('Contribution Rank') +
  geom_point(size = 2, pch = 19) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = Label)) +
  ylim(c(0,1))

f4 <- ggplot(data = sr.df4[1:5,], aes(x = Rank, y = Value)) +
  theme_bw() +
  ggtitle('Nitrous Oxide Emissions') +
  ylab('Contribution to Synthetic US') +
  xlab('Contribution Rank') +
  geom_point(size = 2, pch = 19) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = Label)) +
  ylim(c(0,1))

f5 <- ggplot(data = sr.df5[1:5,], aes(x = Rank, y = Value)) +
  theme_bw() +
  ggtitle('Methane Emissions from Agriculture') +
  ylab('Contribution to Synthetic US') +
  xlab('Contribution Rank') +
  geom_point(size = 2, pch = 19) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = Label)) +
  ylim(c(0,1))

f6 <- ggplot(data = sr.df6[1:5,], aes(x = Rank, y = Value)) +
  theme_bw() +
  ggtitle('Nitrous Oxide Emissions from Agriculture') +
  ylab('Contribution to Synthetic US') +
  xlab('Contribution Rank') +
  geom_point(size = 2, pch = 19) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = Label)) +
  ylim(c(0,1))

png(paste(direc, 'figures/non_kp_contributions.png', sep = ''))
par(mfrow = c(2,3))
f1 + f2 + f3 + f4 + f5 + f6 + plot_layout(ncol = 2)
dev.off()

f1 + f2 + f3 + f4 + f5 + f6 + plot_layout(ncol = 2)

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
                     treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                     time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  co2.dp <- dataprep(foo = kp.data.co2, predictors = c(preds, 'CO2.Lag'), predictors.op = c('mean'),
                     dependent = c('CO2'), unit.variable = c('ID'), time.variable = c('Year'),
                     treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                     time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  ch4.dp <- dataprep(foo = kp.data.ch4, predictors = c(preds, 'CH4.Lag'), predictors.op = c('mean'),
                     dependent = c('CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                     treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                     time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  no2.dp <- dataprep(foo = kp.data.no2, predictors = c(preds, 'NO2.Lag'), predictors.op = c('mean'),
                     dependent = c('NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                     treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                     time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  ag.ch4.dp <- dataprep(foo = kp.data.ag.ch4, predictors = c(preds, 'AG.CH4.Lag'), predictors.op = c('mean'),
                        dependent = c('AG.CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                        treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                        time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  ag.no2.dp <- dataprep(foo = kp.data.ag.no2, predictors = c(preds, 'AG.NO2.Lag'), predictors.op = c('mean'),
                        dependent = c('AG.NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                        treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                        time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  ghg.synth <- synth(data.prep.obj = ghg.dp, optimxmethod = 'All')
  co2.synth <- synth(data.prep.obj = co2.dp, optimxmethod = 'All')
  ch4.synth <- synth(data.prep.obj = ch4.dp, optimxmethod = 'All')
  no2.synth <- synth(data.prep.obj = no2.dp, optimxmethod = 'All')
  ag.ch4.synth <- synth(data.prep.obj = ag.ch4.dp, optimxmethod = 'All')
  ag.no2.synth <- synth(data.prep.obj = ag.no2.dp, optimxmethod = 'All')

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

  for (j in 1990:2019) {

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

ghg.plot.df <- as.data.frame(matrix(ghg.plot.data, nrow = 30, ncol = 20))
co2.plot.df <- as.data.frame(matrix(co2.plot.data, nrow = 30, ncol = 20))
ch4.plot.df <- as.data.frame(matrix(ch4.plot.data, nrow = 30, ncol = 20))
no2.plot.df <- as.data.frame(matrix(no2.plot.data, nrow = 30, ncol = 20))
ag.ch4.plot.df <- as.data.frame(matrix(ag.ch4.plot.data, nrow = 30, ncol = 20))
ag.no2.plot.df <- as.data.frame(matrix(ag.no2.plot.data, nrow = 30, ncol = 20))

names(ghg.plot.df) <- unique(kp.data.ghg$Country.Name)
names(co2.plot.df) <- unique(kp.data.ghg$Country.Name)
names(ch4.plot.df) <- unique(kp.data.ghg$Country.Name)
names(no2.plot.df) <- unique(kp.data.ghg$Country.Name)
names(ag.ch4.plot.df) <- unique(kp.data.ghg$Country.Name)
names(ag.no2.plot.df) <- unique(kp.data.ghg$Country.Name)

ghg.plot.df$Year <- 1990:2019
co2.plot.df$Year <- 1990:2019
ch4.plot.df$Year <- 1990:2019
no2.plot.df$Year <- 1990:2019
ag.ch4.plot.df$Year <- 1990:2019
ag.no2.plot.df$Year <- 1990:2019

png(paste(direc, 'figures/kp_placebo_ghg.png', sep = ''))
ggplot(data = ghg.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Total GHG Emissions') +
  ylab('Relative to 1990') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/kp_placebo_co2.png', sep = ''))
ggplot(data = ghg.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Carbon Dioxide Emissions') +
  ylab('Relative to 1990') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/kp_placebo_ch4.png', sep = ''))
ggplot(data = ch4.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Methane Emissions') +
  ylab('Relative to 1990') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/kp_placebo_no2.png', sep = ''))
ggplot(data = no2.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions') +
  ylab('Relative to 1990') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/kp_placebo_ag_ch4.png', sep = ''))
ggplot(data = ag.ch4.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Methane Emissions from Agriculture') +
  ylab('Relative to 1990') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/kp_placebo_ag_no2.png', sep = ''))
ggplot(data = ag.no2.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions from Agriculture') +
  ylab('Relative to 1990') +
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
  geom_vline(xintercept = 2005)
dev.off()

# Placebo testing for non-KP nations

c.ghg.placebo.vals <- c()
c.co2.placebo.vals <- c()
c.ch4.placebo.vals <- c()
c.no2.placebo.vals <- c()
c.ag.ch4.placebo.vals <- c()
c.ag.no2.placebo.vals <- c()

for (i in 1:59) {

  print(i)

  cids <- 1:59
  cids <- cids[!cids == i]

  control.ghg.dp <- dataprep(foo = control.data.ghg, predictors = c(preds, 'GHG.Lag'), predictors.op = c('mean'),
                             dependent = c('GHG'), unit.variable = c('ID'), time.variable = c('Year'),
                             treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                             time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  control.co2.dp <- dataprep(foo = control.data.co2, predictors = c(preds, 'CO2.Lag'), predictors.op = c('mean'),
                             dependent = c('CO2'), unit.variable = c('ID'), time.variable = c('Year'),
                             treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                             time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  control.ch4.dp <- dataprep(foo = control.data.ch4, predictors = c(preds, 'CH4.Lag'), predictors.op = c('mean'),
                             dependent = c('CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                             treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                             time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  control.no2.dp <- dataprep(foo = control.data.no2, predictors = c(preds, 'NO2.Lag'), predictors.op = c('mean'),
                             dependent = c('NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                             treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                             time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  control.ag.ch4.dp <- dataprep(foo = control.data.ag.ch4, predictors = c(preds, 'AG.CH4.Lag'), predictors.op = c('mean'),
                                dependent = c('AG.CH4'), unit.variable = c('ID'), time.variable = c('Year'),
                                treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                                time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

  control.ag.no2.dp <- dataprep(foo = control.data.ag.no2, predictors = c(preds, 'AG.NO2.Lag'), predictors.op = c('mean'),
                                dependent = c('AG.NO2'), unit.variable = c('ID'), time.variable = c('Year'),
                                treatment.identifier = i, controls.identifier = cids, time.predictors.prior = c(1990:2004),
                                time.optimize.ssr = c(1990:2004), unit.names.variable = 'Country.Name', time.plot = c(1990:2019))

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

for (i in 1:59) {

  print(i)

  ts1 <- c()
  ts2 <- c()
  ts3 <- c()
  ts4 <- c()
  ts5 <- c()
  ts6 <- c()

  for (j in 1990:2019) {

    a <- 58*i - 57
    b <- 58*i

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

c.ghg.plot.df <- as.data.frame(matrix(c.ghg.plot.data, nrow = 30, ncol = 59))
c.co2.plot.df <- as.data.frame(matrix(c.co2.plot.data, nrow = 30, ncol = 59))
c.ch4.plot.df <- as.data.frame(matrix(c.ch4.plot.data, nrow = 30, ncol = 59))
c.no2.plot.df <- as.data.frame(matrix(c.no2.plot.data, nrow = 30, ncol = 59))
c.ag.ch4.plot.df <- as.data.frame(matrix(c.ag.ch4.plot.data, nrow = 30, ncol = 59))
c.ag.no2.plot.df <- as.data.frame(matrix(c.ag.no2.plot.data, nrow = 30, ncol = 59))

names(c.ghg.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.co2.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.ch4.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.no2.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.ag.ch4.plot.df) <- unique(control.data.ghg$Country.Name)
names(c.ag.no2.plot.df) <- unique(control.data.ghg$Country.Name)

c.ghg.plot.df$Year <- 1990:2019
c.co2.plot.df$Year <- 1990:2019
c.ch4.plot.df$Year <- 1990:2019
c.no2.plot.df$Year <- 1990:2019
c.ag.ch4.plot.df$Year <- 1990:2019
c.ag.no2.plot.df$Year <- 1990:2019

png(paste(direc, 'figures/non_kp_placebo_ghg.png', sep = ''))
ggplot(data = c.ghg.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Total GHG Emissions') +
  ylab('Relative to 1990') +
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
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_co2.png', sep = ''))
ggplot(data = c.co2.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Carbon Dioxide Emissions') +
  ylab('Relative to 1990') +
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
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_ch4.png', sep = ''))
ggplot(data = c.ch4.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Methane Emissions') +
  ylab('Relative to 1990') +
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
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_no2.png', sep = ''))
ggplot(data = c.no2.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions') +
  ylab('Relative to 1990') +
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
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_ag_ch4.png', sep = ''))
ggplot(data = c.ag.ch4.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Methane Emissions from Agriculture') +
  ylab('Relative to 1990') +
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
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
  geom_vline(xintercept = 2005)
dev.off()

png(paste(direc, 'figures/non_kp_placebo_ag_no2.png', sep = ''))
ggplot(data = c.ag.no2.plot.df, aes(x = Year, y = `United States`)) +
  ggtitle('Placebo Test Results for Nitrous Oxide Emissions from Agriculture') +
  ylab('Relative to 1990') +
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
  geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
  geom_vline(xintercept = 2005)
dev.off()

# Creating multiplots as an alternative for the paper

png(paste(direc, 'figures/kp_synth.png', sep = ''))
par(mfrow = c(2,3))

path.plot(synth.res = kp.ghg.synth, dataprep.res = kp.ghg.dp, Ylab = 'Relative to 1990', Main = 'Total Greenhouse Gas Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = kp.co2.synth, dataprep.res = kp.co2.dp, Ylab = 'Relative to 1990', Main = 'Carbon Dioxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = kp.ch4.synth, dataprep.res = kp.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = kp.no2.synth, dataprep.res = kp.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = kp.ag.ch4.synth, dataprep.res = kp.ag.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = kp.ag.no2.synth, dataprep.res = kp.ag.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

dev.off()

png(paste(direc, 'figures/non_kp_synth.png', sep = ''))
par(mfrow = c(2,3))

path.plot(synth.res = control.ghg.synth, dataprep.res = control.ghg.dp, Ylab = 'Relative to 1990', Main = 'Total Greenhouse Gas Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.co2.synth, dataprep.res = control.co2.dp, Ylab = 'Relative to 1990', Main = 'Carbon Dioxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.ch4.synth, dataprep.res = control.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.no2.synth, dataprep.res = control.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.ag.ch4.synth, dataprep.res = control.ag.ch4.dp, Ylab = 'Relative to 1990', Main = 'Methane Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

path.plot(synth.res = control.ag.no2.synth, dataprep.res = control.ag.no2.dp, Ylab = 'Relative to 1990', Main = 'Nitrous Oxide Emissions from Agriculture',
          Xlab = 'Year', Legend = c('United States', 'Synthetic Control'), Legend.position = 'topright')
abline(v = 2004)
abline(h = 1)

dev.off()

f1 <- ggplot(data = ghg.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Total GHG Emissions') +
      ylab('Relative to 1990') +
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
      geom_vline(xintercept = 2005)

f2 <- ggplot(data = ghg.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Carbon Dioxide Emissions') +
      ylab('Relative to 1990') +
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
      geom_vline(xintercept = 2005)

f3 <- ggplot(data = ch4.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Methane Emissions') +
      ylab('Relative to 1990') +
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
      geom_vline(xintercept = 2005)

f4 <- ggplot(data = no2.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Nitrous Oxide Emissions') +
      ylab('Relative to 1990') +
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
      geom_vline(xintercept = 2005)

f5 <- ggplot(data = ag.ch4.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Methane Emissions from Agriculture') +
      ylab('Relative to 1990') +
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
      geom_vline(xintercept = 2005)

f6 <- ggplot(data = ag.no2.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Nitrous Oxide Emissions from Agriculture') +
      ylab('Relative to 1990') +
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
  geom_vline(xintercept = 2005)

png(paste(direc, 'figures/kp_placebo.png', sep = ''))
par(mfrow = c(2,3))
f1 + f2 + f3 + f4 + f5 + f6 + plot_layout(ncol = 2)
dev.off()

f1 + f2 + f3 + f4 + f5 + f6 + plot_layout(ncol = 2)

f1 <- ggplot(data = c.ghg.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Total GHG Emissions') +
      ylab('Relative to 1990') +
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
      geom_line(aes(x = Year, y = `Cuba`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') +
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
      geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
      geom_vline(xintercept = 2005)

f2 <- ggplot(data = c.co2.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Carbon Dioxide Emissions') +
      ylab('Relative to 1990') +
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
      geom_line(aes(x = Year, y = `Cuba`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') +
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
      geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
      geom_vline(xintercept = 2005)

f3 <- ggplot(data = c.ch4.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Methane Emissions') +
      ylab('Relative to 1990') +
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
      geom_line(aes(x = Year, y = `Cuba`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') +
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
      geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
      geom_vline(xintercept = 2005)

f4 <- ggplot(data = c.no2.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Nitrous Oxide Emissions') +
      ylab('Relative to 1990') +
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
      geom_line(aes(x = Year, y = `Cuba`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') +
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
      geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
      geom_vline(xintercept = 2005)

f5 <- ggplot(data = c.ag.ch4.plot.df, aes(x = Year, y = `United States`)) +
      ggtitle('Placebo Test Results for Methane Emissions from Agriculture') +
      ylab('Relative to 1990') +
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
      geom_line(aes(x = Year, y = `Cuba`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') +
      geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') +
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
      geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
      geom_vline(xintercept = 2005)

f6 <- ggplot(data = c.ag.no2.plot.df, aes(x = Year, y = `United States`)) +
    ggtitle('Placebo Test Results for Nitrous Oxide Emissions from Agriculture') +
    ylab('Relative to 1990') +
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
    geom_line(aes(x = Year, y = `Cuba`), size = 1, alpha = 1, color = 'gray') +
    geom_line(aes(x = Year, y = `Dominican Republic`), size = 1, alpha = 1, color = 'gray') +
    geom_line(aes(x = Year, y = `Ecuador`), size = 1, alpha = 1, color = 'gray') +
    geom_line(aes(x = Year, y = `Egypt, Arab Rep.`), size = 1, alpha = 1, color = 'gray') +
    geom_line(aes(x = Year, y = `El Salvador`), size = 1, alpha = 1, color = 'gray') +
    geom_line(aes(x = Year, y = `Ethiopia`), size = 1, alpha = 1, color = 'gray') +
    geom_line(aes(x = Year, y = `Ghana`), size = 1, alpha = 1, color = 'gray') +
    geom_line(aes(x = Year, y = `Haiti`), size = 1, alpha = 1, color = 'gray') +
    geom_line(aes(x = Year, y = `Honduras`), size = 1, alpha = 1, color = 'gray') +
    geom_line(aes(x = Year, y = `India`), size = 1, alpha = 1, color = 'gray') +
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
    geom_line(aes(x = Year, y = `Myanmar`), size = 1, alpha = 1, color = 'gray') +
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
    geom_vline(xintercept = 2005)

png(paste(direc, 'figures/non_kp_placebo.png', sep = ''))
par(mfrow = c(2,3))
f1 + f2 + f3 + f4 + f5 + f6 + plot_layout(ncol = 2)
dev.off()

f1 + f2 + f3 + f4 + f5 + f6 + plot_layout(ncol = 2)

# Main function for implementing the bootstrapped Synthetic Control Method

pleather <- function (dataframex, treat.id, iterations, subsample, pred.vars, preds.op, dep.var, unit.var, time.var, pre.time, opt.time, unit.var.name, plot.time, synth.opt.method, plot.title, plot.y, plot.x, plot.y.int, plot.x.int, treated.name, treated.color, save.to) {

  # Setting a seed for replicability

  set.seed(42069)

  # Data storage for main loop

  plot.data <- c()

  # Main loop

  for (i in 1:iterations) {

    boot.vals <- c()

    # Subset controls with subsample

    control.ids <- sample(unique(dataframex[,unit.var])[which(unique(dataframex[,unit.var]) != treat.id)], floor(subsample * length(unique(dataframex[,unit.var])[which(unique(dataframex[,unit.var]) != treat.id)])), replace = FALSE)
    dataframe <- dataframex[which(dataframex[,unit.var] %in% c(control.ids, treat.id)),]

    # Create the data prep object for Synth

    synth.data.prep.object <- dataprep(foo = dataframe, predictors = pred.vars, predictors.op = preds.op, dependent = dep.var,
                                       unit.variable = unit.var, time.variable = time.var, treatment.identifier = treat.id,
                                       controls.identifier = control.ids, time.predictors.prior = pre.time, time.optimize.ssr = opt.time,
                                       unit.names.variable = unit.var.name, time.plot = plot.time)

    # Run Synth

    if (missing(synth.opt.method) == TRUE) {

      synth.output <- synth(data.prep.obj = synth.data.prep.object)

    } else {

      synth.output <- synth(data.prep.obj = synth.data.prep.object, optimxmethod = synth.opt.method)

    }

    # Store data

    for (i in min(dataframe[,time.var]):max(dataframe[,time.var])) {

      tmp <- dataframe[which(dataframe[,unit.var] != treat.id),]
      tmp <- tmp[which(tmp[,time.var] == i),]
      boot.vals <- c(boot.vals, sum(tmp[,dep.var] * synth.output$solution.w))

    }

    plot.data <- cbind(plot.data, boot.vals)

  }

  # Creating a data.frame for to make a figure with

  plot.data <- as.data.frame(plot.data)
  means <- rowMeans(plot.data)
  sds <- apply(plot.data, 1, sd)
  plot.df <- as.data.frame(cbind(means, sds, dataframex[which(dataframex[,unit.var] == treat.id), dep.var], unique(dataframe[,time.var])))
  colnames(plot.df) <- c('Mean', 'SD', 'Value', 'X')

  # Creating a figure with ggplot

  if (missing(treated.color) == TRUE) {

    treated.color = 'red4' # Go Hokies

  }

  p <- ggplot(data = plot.df, aes(x = X, y = Value)) +
    theme_bw() +
    ggtitle(plot.title) +
    ylab(plot.y) +
    xlab(plot.x) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_line(aes(y = Mean, col = 'Synthetic Control'), size = 2, alpha = 1) +
    geom_ribbon(aes(ymin = Mean - 2*SD, ymax = Mean + 2*SD), fill = 'lightgray') +
    geom_line(aes(y = Value, col = treated.name), size = 1, alpha = 1) +
    scale_color_manual(name = '', breaks = c(treated.name, 'Synthetic Control'), values = c(treated.color, 'lightgray'))

  if (missing(plot.x.int) == FALSE) {

    p <- p + geom_vline(xintercept = plot.x.int)

  }

  if (missing(plot.y.int) == FALSE) {

    p <- p + geom_hline(yintercept = plot.y.int)

  }

  if (missing(save.to) == FALSE) {

    png(save.to)
    print(p)
    dev.off()

  }

  print(p)

  # Creating a results table in text format

  crit.t.10 <- 1.645
  crit.t.05 <- 1.960
  crit.t.01 <- 2.576

  restab.text <- '-------------------------------------\n'
  restab.text <- paste0(restab.text, '            Pre-Treatment            \n')
  restab.text <- paste0(restab.text, '-------------------------------------\n')
  restab.text <- paste0(restab.text, 'Time Period          Treatment Effect\n')
  restab.text <- paste0(restab.text, '-------------------------------------\n')

  for (i in pre.time) {

    new.val <- round((plot.df$Value[i+1-min(pre.time)] - plot.df$Mean[i+1-min(pre.time)]),3)
    t.stat <- (plot.df$Mean[i+1-min(pre.time)] - plot.df$Value[i+1-min(pre.time)]) / plot.df$SD[i+1-min(pre.time)]

    if (isTRUE(abs(t.stat) >= crit.t.01)) {

      new.val <- paste0(new.val, '***')

    } else if (isTRUE(abs(t.stat) >= crit.t.05)) {

      new.val <- paste0(new.val, '**')

    } else if (isTRUE(abs(t.stat) >= crit.t.10)) {

      new.val <- paste0(new.val, '*')

    }

    new.string <- paste0(i, '                  ', new.val, '\n')
    restab.text <- paste0(restab.text, new.string)
    restab.text <- paste0(restab.text, '                     (', round(plot.df$SD[i+1-min(pre.time)],3), ')         \n')

  }

  restab.text <- paste0(restab.text, '-------------------------------------\n')
  restab.text <- paste0(restab.text, '           Post-Treatment            \n')
  restab.text <- paste0(restab.text, '-------------------------------------\n')
  restab.text <- paste0(restab.text, 'Time Period          Treatment Effect\n')
  restab.text <- paste0(restab.text, '-------------------------------------\n')

  for (i in (max(pre.time)+1):max(unique(dataframex[,time.var]))) {

    new.val <- round((plot.df$Value[i+1-min(pre.time)] - plot.df$Mean[i+1-min(pre.time)]),3)
    t.stat <- (plot.df$Mean[i+1-min(pre.time)] - plot.df$Value[i+1-min(pre.time)]) / plot.df$SD[i+1-min(pre.time)]

    if (isTRUE(abs(t.stat) >= crit.t.01)) {

      new.val <- paste0(new.val, '***')

    } else if (isTRUE(abs(t.stat) >= crit.t.05)) {

      new.val <- paste0(new.val, '**')

    } else if (isTRUE(abs(t.stat) >= crit.t.10)) {

      new.val <- paste0(new.val, '*')

    }

    new.string <- paste0(i, '                  ', new.val, '\n')
    restab.text <- paste0(restab.text, new.string)
    restab.text <- paste0(restab.text, '                     (', round(plot.df$SD[i+1-min(pre.time)],3), ')         \n')

  }

  restab.text <- paste0(restab.text, '-------------------------------------\n')
  restab.text <- paste0(restab.text, 'Note:     *p<0.1; **p<0.05; ***p<0.01\n')

  # Creating a latex table

  restab.tex <- '\\hline\\hline\\\\\n'
  restab.tex <- paste0(restab.tex, '\\multicolumn{2}{c}{Pre-Treatment}\\\\\n')
  restab.tex <- paste0(restab.tex, '\\hline\\\\\n')
  restab.tex <- paste0(restab.tex, 'Time Period & Treatment Effect\\\n')
  restab.tex <- paste0(restab.tex, '\\hline\\\\\n')

  for (i in pre.time) {

    new.val <- round((plot.df$Value[i+1-min(pre.time)] - plot.df$Mean[i+1-min(pre.time)]),3)
    t.stat <- (plot.df$Mean[i+1-min(pre.time)] - plot.df$Value[i+1-min(pre.time)]) / plot.df$SD[i+1-min(pre.time)]

    if (isTRUE(abs(t.stat) >= crit.t.01)) {

      new.val <- paste0(new.val, '$^{***}$')

    } else if (isTRUE(abs(t.stat) >= crit.t.05)) {

      new.val <- paste0(new.val, '$^{**}$')

    } else if (isTRUE(abs(t.stat) >= crit.t.10)) {

      new.val <- paste0(new.val, '$^{*}$')

    }

    new.string <- paste0(i, ' & ', new.val, '\\\\\n')
    restab.tex <- paste0(restab.tex, new.string)
    restab.tex <- paste0(restab.tex, ' & (', round(plot.df$SD[i+1-min(pre.time)],3), ')\\\\\n')

  }

  restab.tex <- paste0(restab.tex, '\\hline\\\\\n')
  restab.tex <- paste0(restab.tex, '\\multicolumn{2}{c}{Post-Treatment}\\\\\n')
  restab.tex <- paste0(restab.tex, '\\hline\\\\\n')
  restab.tex <- paste0(restab.tex, 'Time Period & Treatment Effect\\\n')
  restab.tex <- paste0(restab.tex, '\\hline\\\\\n')

  for (i in (max(pre.time)+1):max(unique(dataframex[,time.var]))) {

    new.val <- round((plot.df$Value[i+1-min(pre.time)] - plot.df$Mean[i+1-min(pre.time)]),3)
    t.stat <- (plot.df$Mean[i+1-min(pre.time)] - plot.df$Value[i+1-min(pre.time)]) / plot.df$SD[i+1-min(pre.time)]

    if (isTRUE(abs(t.stat) >= crit.t.01)) {

      new.val <- paste0(new.val, '$^{***}')

    } else if (isTRUE(abs(t.stat) >= crit.t.05)) {

      new.val <- paste0(new.val, '$^{**}')

    } else if (isTRUE(abs(t.stat) >= crit.t.10)) {

      new.val <- paste0(new.val, '$^{*}')

    }

    new.string <- paste0(i, ' & ', new.val, '\\\\\n')
    restab.tex <- paste0(restab.tex, new.string)
    restab.tex <- paste0(restab.tex, ' & (', round(plot.df$SD[i+1-min(pre.time)],3), ')\\\\\n')

  }

  restab.tex <- paste0(restab.tex, '\\hline\\\\\n')
  restab.tex <- paste0(restab.tex, ' & Note: *p<0.1; **p<0.05; ***p<0.01\n')

  # Printing the results table in text format

  writeLines(restab.text, con = stdout(), sep = '\n', useBytes = FALSE)

  # Creating an output object

  output <- list(restab.text, restab.tex, plot.df, plot.data, p)

  return(output)

}

# Running the bootstrapped Synthetic Control Method

pk1 <- pleather(dataframe = kp.data.ghg, treat.id = 20, iterations = 100, subsample = .5, pred.vars = c(preds, 'GHG.Lag'), preds.op = 'mean', dep.var = 'GHG', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Total Greenhouse Gas Emissions', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pk2 <- pleather(dataframe = kp.data.co2, treat.id = 20, iterations = 100, subsample = .5, pred.vars = c(preds, 'CO2.Lag'), preds.op = 'mean', dep.var = 'CO2', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Carbon Dioxide Emissions', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pk3 <- pleather(dataframe = kp.data.ch4, treat.id = 20, iterations = 100, subsample = .5, pred.vars = c(preds, 'CH4.Lag'), preds.op = 'mean', dep.var = 'CH4', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Methane Emissions', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pk4 <- pleather(dataframe = kp.data.no2, treat.id = 20, iterations = 100, subsample = .5, pred.vars = c(preds, 'NO2.Lag'), preds.op = 'mean', dep.var = 'NO2', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Nitrous Oxide Emissions', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pk5 <- pleather(dataframe = kp.data.ag.ch4, treat.id = 20, iterations = 100, subsample = .5, pred.vars = c(preds, 'AG.CH4.Lag'), preds.op = 'mean', dep.var = 'AG.CH4', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Methane Emissions from Agriculture', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pk6 <- pleather(dataframe = kp.data.ag.no2, treat.id = 20, iterations = 100, subsample = .5, pred.vars = c(preds, 'AG.NO2.Lag'), preds.op = 'mean', dep.var = 'AG.NO2', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Nitrous Oxide Emissions from Agriculture', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')

pc1 <- pleather(dataframe = control.data.ghg, treat.id = 55, iterations = 100, subsample = .5, pred.vars = c(preds, 'GHG.Lag'), preds.op = 'mean', dep.var = 'GHG', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Total Greenhouse Gas Emissions', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pc2 <- pleather(dataframe = control.data.co2, treat.id = 55, iterations = 100, subsample = .5, pred.vars = c(preds, 'CO2.Lag'), preds.op = 'mean', dep.var = 'CO2', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Carbon Dioxide Emissions', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pc3 <- pleather(dataframe = control.data.ch4, treat.id = 55, iterations = 100, subsample = .5, pred.vars = c(preds, 'CH4.Lag'), preds.op = 'mean', dep.var = 'CH4', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Methane Emissions', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pc4 <- pleather(dataframe = control.data.no2, treat.id = 55, iterations = 100, subsample = .5, pred.vars = c(preds, 'NO2.Lag'), preds.op = 'mean', dep.var = 'NO2', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Nitrous Oxide Emissions', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pc5 <- pleather(dataframe = control.data.ag.ch4, treat.id = 55, iterations = 100, subsample = .5, pred.vars = c(preds, 'AG.CH4.Lag'), preds.op = 'mean', dep.var = 'AG.CH4', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Methane Emissions from Agriculture', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')
pc6 <- pleather(dataframe = control.data.ag.no2, treat.id = 55, iterations = 100, subsample = .5, pred.vars = c(preds, 'AG.NO2.Lag'), preds.op = 'mean', dep.var = 'AG.NO2', unit.var = 'ID', time.var = 'Year', pre.time = c(1990:2004), opt.time = c(1990:2004), unit.var.name = 'Country.Name', plot.time = c(1990:2019), synth.opt.method = 'All', plot.title = 'Nitrous Oxide Emissions from Agriculture', plot.y = 'Relative to 1990', plot.x = 'Year', plot.y.int = 1, plot.x.int = 2005, treated.name = 'United States', treated.color = 'red4')

png(paste(direc, 'figures/kp_boot.png', sep = ''))
par(mfrow = c(2,3))
pk1[[5]] + pk2[[5]] + pk3[[5]] + pk4[[5]] + pk5[[5]] + pk6[[5]] + plot_layout(ncol = 2)
dev.off()

png(paste(direc, 'figures/non_kp_boot.png', sep = ''))
par(mfrow = c(2,3))
pc1[[5]] + pc2[[5]] + pc3[[5]] + pc4[[5]] + pc5[[5]] + pc6[[5]] + plot_layout(ncol = 2)
dev.off()

pk1[[5]] + pk2[[5]] + pk3[[5]] + pk4[[5]] + pk5[[5]] + pk6[[5]] + plot_layout(ncol = 2)
pc1[[5]] + pc2[[5]] + pc3[[5]] + pc4[[5]] + pc5[[5]] + pc6[[5]] + plot_layout(ncol = 2)

# Writing significance test results to file

write.csv(pk1[[2]], paste0(direc, 'results/kp_ghg.txt'))
write.csv(pk2[[2]], paste0(direc, 'results/kp_co2.txt'))
write.csv(pk3[[2]], paste0(direc, 'results/kp_ch4.txt'))
write.csv(pk4[[2]], paste0(direc, 'results/kp_no2.txt'))
write.csv(pk5[[2]], paste0(direc, 'results/kp_ag_ch4.txt'))
write.csv(pk6[[2]], paste0(direc, 'results/kp_ag_no2.txt'))

write.csv(pc1[[2]], paste0(direc, 'results/non_kp_ghg.txt'))
write.csv(pc2[[2]], paste0(direc, 'results/non_kp_co2.txt'))
write.csv(pc3[[2]], paste0(direc, 'results/non_kp_ch4.txt'))
write.csv(pc4[[2]], paste0(direc, 'results/non_kp_no2.txt'))
write.csv(pc5[[2]], paste0(direc, 'results/non_kp_ag_ch4.txt'))
write.csv(pc6[[2]], paste0(direc, 'results/non_kp_ag_no2.txt'))

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

means.kp <- c()
means.nonkp <- c()
means.us <- c()

sds.kp <- c()
sds.nonkp <- c()
sds.us <- c()

for (c in colnames(kp.sumdata)) {

  means.kp <- c(means.kp, mean(kp.sumdata[,c], na.rm = TRUE))
  means.nonkp <- c(means.nonkp, mean(c.sumdata[,c], na.rm = TRUE))
  means.us <- c(means.us, mean(us.sumdata[,c], na.rm = TRUE))

  sds.kp <- c(sds.kp, sd(kp.sumdata[,c], na.rm = TRUE))
  sds.nonkp <- c(sds.nonkp, sd(c.sumdata[,c], na.rm = TRUE))
  sds.us <- c(sds.us, sd(us.sumdata[,c], na.rm = TRUE))

}

kpx <- as.data.frame(cbind(colnames(kp.sumdata), means.kp, means.nonkp, means.us, sds.kp, sds.nonkp, sds.us))

# Simulation to create a figure motiviating the method

m <- c(1,1.03,1.06,1.09,1.12,1.15,1.18,1.21,1.24,1.27,1.3,1.33,1.36,1.39,1.42,1.45,1.45,1.45,1.45,1.45,1.45,1.45,1.45,1.45,1.45,1.45,1.45,1.45,1.45,1.45)
s <- c(.05,.054,.056,.051,.044,.058,.06,.048,.055,.054,.062,.064,.06,.054,.05,.047,.06,.07,.075,.08,.086,.099,.12,.16,.2,.28,.28,.3,.33,.32)
t <- 1990:2019
a <- c(1,1.033,1.056,1.091,1.14,1.152,1.188,1.24,1.242,1.26,1.2666,1.302,1.34,1.382,1.4227,1.453,1.458,1.487,1.4974,1.498,1.527,1.512,1.544,1.561,1.573,1.598,1.627,1.614,1.633,1.65)

eg <- as.data.frame(cbind(m,s,a,t))

egf <- ggplot(data = eg, aes(x = t, y = a)) +
  ggtitle('Example of an Insigificant Deviation') +
  xlab('Year') +
  ylab('Relative to 1990') +
  geom_line(aes(y = m, col = '95% Confidence Interval'), size = 2, alpha = 1) +
  geom_ribbon(aes(ymin = m - 2*s, ymax = m + 2*s), fill = 'lightgray') +
  geom_line(aes(y = m, col = 'Synthetic Control'), size = 1, alpha = 1, linetype = 2) +
  geom_line(aes(y = a, col = '"Treated" Unit'), size = 1, alpha = 1) +
  scale_color_manual(name = '', breaks = c('"Treated" Unit', 'Synthetic Control', '95% Confidence Interval'), values = c('red4', 'black', 'lightgray')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 2005) +
  ylim(0,2.5)

png(paste(direc, 'figures/silly_willy.png', sep = ''))
egf
dev.off()

egf

