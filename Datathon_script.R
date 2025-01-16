#Rotman Datathon 2025
#### Libraries 
library(tidyverse)
library(funModeling)
library(Hmisc)
library(dplyr)
library(ggpubr)
library(tidyr)
library(knitr)
library(kableExtra)
library(readxl)
library(lubridate)
library(mice)
library(corrplot)
library(car)
library(ggplot2)
library(multcomp)
library(lme4)
library(forecast)
 
#### DATA READING ####

# Read raw data and metdata files
raw_data <- read_excel("Datathon_data-2025-Raw.xlsx", sheet = 1)
series_meta <- read_excel("Datathon_data-2025-Raw.xlsx", sheet = 2)
country_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 2)
country.series_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 3)
series.time_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 4)
footnote_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 5)

#### DATA PREPROCESSING AND CLEANING ####

# Saving in new df 
data <- raw_data

# NAs in data
anyNA(data)
# All in last 5 rows

# Removing last 5 rows as they contain no data/observations 
data <- data[-(3256: 3260), ]

# Recoding ".." as NAs
for (col in colnames(data)) {
  if (any(data[[col]] == "..", na.rm = TRUE)) {
    data[[col]][data[[col]] == ".."] <- NA
  }
}

dim(data)

# Using for loop to converts character format into numeric for relevant indicators
for (col in colnames(data)) {
  if (!col %in% c("Country Name", "Country Code", "Time", "Time Code")) {
    data[[col]] <- as.numeric(data[[col]])
  }
  else {
    data[[col]] <- data[[col]]
  }
}

#### VARIABLE SELECTION #####

# Renaming the relevant columns
selected_data <- data %>%
  rename(
    #consumer price 
    consumer_price = "Consumer price index (2010 = 100) [FP.CPI.TOTL]",
    # Country Identifiers 
    country_name = "Country Name",
    country_code = "Country Code",
    
    # Time identifiers 
    year = "Time",
    time_code = "Time Code",
    
    # Cost of living indicators
    # Inflationary pressure 
    inflation = "Inflation, consumer prices (annual %) [FP.CPI.TOTL.ZG]",
    inflation_deflator = "Inflation, GDP deflator (annual %) [NY.GDP.DEFL.KD.ZG]",
    
    # Cost of living index (indirect measure)
    net_nat_income_percapita_NY.ADJ.NNTY.PC.CD = "Adjusted net national income per capita (current US$) [NY.ADJ.NNTY.PC.CD]",
    net_nat_income_NY.ADJ.NNTY.CD = "Adjusted net national income (current US$) [NY.ADJ.NNTY.CD]",
    household_expenditure_GDP = "Households and NPISHs final consumption expenditure (% of GDP) [NE.CON.PRVT.ZS]",
    household_expenditure_per_capita = "Households and NPISHs Final consumption expenditure per capita growth (annual %) [NE.CON.PRVT.PC.KD.ZG]",
    household_expenditure_ppp ="Households and NPISHs Final consumption expenditure, PPP (current international $) [NE.CON.PRVT.PP.CD]",
    
    # Economic indicators (part of cost of living, measures income)
    gdp_percapita_NY.GDP.PCAP.PP.CD = "GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]",
    gdp_percapita_growth_NY.GDP.PCAP.KD.ZG = "GDP per capita growth (annual %) [NY.GDP.PCAP.KD.ZG]",
    gdp_deflator_NY.GDP.DEFL.ZS = "GDP deflator (base year varies by country) [NY.GDP.DEFL.ZS]",
    real_int_rate_FR.INR.RINR = "Real interest rate (%) [FR.INR.RINR]", 
    lending_rate_FR.INR.LEND = "Lending interest rate (%) [FR.INR.LEND]", 
    
    # Human Development Index (indirect measure) 
    human_capital_index_HD.HCI.OVRL = "Human capital index (HCI) (scale 0-1) [HD.HCI.OVRL]",
    education_expenditure_SE.XPD.CTOT.ZS = "Current education expenditure, total (% of total expenditure in public institutions) [SE.XPD.CTOT.ZS]",
    life_expectancy_SP.DYN.LE00.IN = "Life expectancy at birth, total (years) [SP.DYN.LE00.IN]",
    social_contributions_GC.REV.SOCL.ZS = "Social contributions (% of revenue) [GC.REV.SOCL.ZS]", 
    health_expenditure_SH.XPD.CHEX.PP.CD = "Current health expenditure (% of GDP) [SH.XPD.CHEX.GD.ZS]", 
    health_expenditure_per_cap = "Current health expenditure per capita, PPP (current international $) [SH.XPD.CHEX.PP.CD]", 
    electicity_EG.USE.ELEC.KH.PC = "Electric power consumption (kWh per capita) [EG.USE.ELEC.KH.PC]",
  
    # Inequality (part of HDI)
    gini_SI.POV.GINI ="Gini index [SI.POV.GINI]",
    
    # Unemployment (part of HDI)
    unemployment_SL.UEM.TOTL.NE.ZS = "Unemployment, total (% of total labor force) (national estimate) [SL.UEM.TOTL.NE.ZS]",
    unemployment_youth_SL.UEM.1524.NE.ZS = "Unemployment, youth total (% of total labor force ages 15-24) (national estimate) [SL.UEM.1524.NE.ZS]",
    
    # Supply chain indicators 
    # LPI 
    lpi_LP.LPI.INFR.XQ = "Logistics performance index: Quality of trade and transport-related infrastructure (1=low to 5=high) [LP.LPI.INFR.XQ]", 
    
    # Freight Rate Indices
    transport_comm_export_TX.VAL.TRAN.ZS.WT ="Transport services (% of commercial service exports) [TX.VAL.TRAN.ZS.WT]",
    transport_comm_import_TM.VAL.TRAN.ZS.WT = "Transport services (% of commercial service imports) [TM.VAL.TRAN.ZS.WT]",
    transport_export_bop_BX.GSR.TRAN.ZS = "Transport services (% of service exports, BoP) [BX.GSR.TRAN.ZS]",
    transport_import_bop_BM.GSR.TRAN.ZS = "Transport services (% of service imports, BoP) [BM.GSR.TRAN.ZS]",
    air_transport_IS.AIR.GOOD.MT.K1 = "Air transport, freight (million ton-km) [IS.AIR.GOOD.MT.K1]",
    
    # BOTH PPI and suuply chain variables 
    manufacturing_GDP_NV.IND.MANF.ZS = "Manufacturing, value added (% of GDP) [NV.IND.MANF.ZS]", 
    manufacturing_growth_NV.IND.MANF.KD.ZG = "Manufacturing, value added (annual % growth) [NV.IND.MANF.KD.ZG]",
    manufacturing_other_NV.MNF.OTHR.ZS.UN = "Other manufacturing (% of value added in manufacturing) [NV.MNF.OTHR.ZS.UN]", 
    machinery_NV.MNF.MTRN.ZS.UN = "Machinery and transport equipment (% of value added in manufacturing) [NV.MNF.MTRN.ZS.UN]", 
    manufacturer_exports_TX.VAL.MANF.ZS.UN = "Manufactures exports (% of merchandise exports) [TX.VAL.MANF.ZS.UN]", 
    manufacturer_imports_TM.VAL.MANF.ZS.UN = "Manufactures imports (% of merchandise imports) [TM.VAL.MANF.ZS.UN]",
    export_cost_IC.EXP.CSBC.CD = "Cost to export, border compliance (US$) [IC.EXP.CSBC.CD]",
    import_cost_IC.IMP.CSBC.CD = "Cost to import, border compliance (US$) [IC.IMP.CSBC.CD]",
    
    # Producer Price Index (PPI) 
    exports_GDP_NE.EXP.GNFS.ZS = "Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]",
    exports_growth_NE.EXP.GNFS.KD.ZG ="Exports of goods and services (annual % growth) [NE.EXP.GNFS.KD.ZG]",
    import_unit_index_TM.UVI.MRCH.XD.WD = "Import unit value index (2015 = 100) [TM.UVI.MRCH.XD.WD]", 
    import_value_index_TM.VAL.MRCH.XD.WD = "Import value index (2015 = 100) [TM.VAL.MRCH.XD.WD]", 
    import_volume_index_TM.QTY.MRCH.XD.WD = "Import volume index (2015 = 100) [TM.QTY.MRCH.XD.WD]",
    oil_rents_NY.GDP.PETR.RT.ZS = "Oil rents (% of GDP) [NY.GDP.PETR.RT.ZS]",
    ore_exports_TX.VAL.MMTL.ZS.UN = "Ores and metals exports (% of merchandise exports) [TX.VAL.MMTL.ZS.UN]", 
    ore_imports_TM.VAL.MMTL.ZS.UN = "Ores and metals imports (% of merchandise imports) [TM.VAL.MMTL.ZS.UN]",
    business_startup_cost_IC.REG.COST.PC.ZS = "Cost of business start-up procedures (% of GNI per capita) [IC.REG.COST.PC.ZS]",

    # Supply Chain Volatility Index 
    net_trade_goods_service_BN.GSR.GNFS.CD = "Net trade in goods and services (BoP, current US$) [BN.GSR.GNFS.CD]", 
    net_trade_goods_BN.GSR.MRCH.CD = "Net trade in goods (BoP, current US$) [BN.GSR.MRCH.CD]", 
    trade_GDP_NE.TRD.GNFS.ZS = "Trade (% of GDP) [NE.TRD.GNFS.ZS]", 
    trade_services_BG.GSR.NFSV.GD.ZS = "Trade in services (% of GDP) [BG.GSR.NFSV.GD.ZS]", 
    mechandise_exports_TX.VAL.MRCH.CD.WT = "Merchandise exports (current US$) [TX.VAL.MRCH.CD.WT]",
    mechandise_imports_TM.VAL.MRCH.CD.WT = "Merchandise imports (current US$) [TM.VAL.MRCH.CD.WT]",
    mechandise_trade_TG.VAL.TOTL.GD.ZS = "Merchandise trade (% of GDP) [TG.VAL.TOTL.GD.ZS]", 
    exports_GDP_NE.EXP.GNFS.ZS = "Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]", 
    exports_growth_NE.EXP.GNFS.KD.ZG = "Exports of goods and services (annual % growth) [NE.EXP.GNFS.KD.ZG]", 
    imports_GDP_NE.IMP.GNFS.ZS = "Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS]",
    imports_growth_NE.IMP.GNFS.KD.ZG = "Imports of goods and services (annual % growth) [NE.IMP.GNFS.KD.ZG]",
    current_account_GDP_BN.CAB.XOKA.GD.ZS = "Current account balance (% of GDP) [BN.CAB.XOKA.GD.ZS]", 
    current_account_USD_BN.CAB.XOKA.CD = "Current account balance (BoP, current US$) [BN.CAB.XOKA.CD]", 
    exchange_rate_PX.REX.REER = "Real effective exchange rate index (2010 = 100) [PX.REX.REER]"
  )

selected_data <- selected_data %>%
  dplyr::select(country_name, 
         country_code, 
         year, 
         time_code, 
         consumer_price,
         inflation,
         inflation_deflator,
         net_nat_income_percapita_NY.ADJ.NNTY.PC.CD, 
         net_nat_income_NY.ADJ.NNTY.CD, 
         household_expenditure_GDP, 
         household_expenditure_per_capita, 
         household_expenditure_ppp, 
         gdp_percapita_NY.GDP.PCAP.PP.CD, 
         gdp_percapita_growth_NY.GDP.PCAP.KD.ZG, 
         gdp_deflator_NY.GDP.DEFL.ZS, 
         real_int_rate_FR.INR.RINR, 
         lending_rate_FR.INR.LEND, 
         human_capital_index_HD.HCI.OVRL, 
         education_expenditure_SE.XPD.CTOT.ZS, 
         life_expectancy_SP.DYN.LE00.IN, 
         social_contributions_GC.REV.SOCL.ZS, 
         health_expenditure_SH.XPD.CHEX.PP.CD, 
         health_expenditure_per_cap, 
         electicity_EG.USE.ELEC.KH.PC, 
         gini_SI.POV.GINI, 
         unemployment_SL.UEM.TOTL.NE.ZS, 
         unemployment_youth_SL.UEM.1524.NE.ZS, 
         lpi_LP.LPI.INFR.XQ, 
         transport_comm_export_TX.VAL.TRAN.ZS.WT, 
         transport_comm_import_TM.VAL.TRAN.ZS.WT, 
         transport_export_bop_BX.GSR.TRAN.ZS, 
         transport_import_bop_BM.GSR.TRAN.ZS, 
         air_transport_IS.AIR.GOOD.MT.K1, 
         manufacturing_GDP_NV.IND.MANF.ZS,
         manufacturing_growth_NV.IND.MANF.KD.ZG, 
         manufacturing_other_NV.MNF.OTHR.ZS.UN, 
         machinery_NV.MNF.MTRN.ZS.UN,  
         manufacturer_exports_TX.VAL.MANF.ZS.UN, 
         manufacturer_imports_TM.VAL.MANF.ZS.UN, 
         export_cost_IC.EXP.CSBC.CD, 
         import_cost_IC.IMP.CSBC.CD, 
         exports_GDP_NE.EXP.GNFS.ZS, 
         exports_growth_NE.EXP.GNFS.KD.ZG, 
         import_unit_index_TM.UVI.MRCH.XD.WD, 
         import_value_index_TM.VAL.MRCH.XD.WD, 
         import_volume_index_TM.QTY.MRCH.XD.WD, 
         oil_rents_NY.GDP.PETR.RT.ZS, 
         ore_exports_TX.VAL.MMTL.ZS.UN, 
         ore_imports_TM.VAL.MMTL.ZS.UN, 
         business_startup_cost_IC.REG.COST.PC.ZS, 
         net_trade_goods_service_BN.GSR.GNFS.CD, 
         net_trade_goods_BN.GSR.MRCH.CD, 
         trade_GDP_NE.TRD.GNFS.ZS, 
         trade_services_BG.GSR.NFSV.GD.ZS, 
         mechandise_exports_TX.VAL.MRCH.CD.WT, 
         mechandise_imports_TM.VAL.MRCH.CD.WT, 
         mechandise_trade_TG.VAL.TOTL.GD.ZS, 
         exports_GDP_NE.EXP.GNFS.ZS, 
         exports_growth_NE.EXP.GNFS.KD.ZG, 
         imports_GDP_NE.IMP.GNFS.ZS, 
         imports_growth_NE.IMP.GNFS.KD.ZG, 
         current_account_GDP_BN.CAB.XOKA.GD.ZS, 
         current_account_USD_BN.CAB.XOKA.CD, 
         exchange_rate_PX.REX.REER, 
  )
         
colnames(selected_data)

dim(selected_data)

#### ADDRESSING MISSINGNESS 

# storing in new df
nonmiss_data <- selected_data 

# removing any variables with >30% missingness 
for (col in colnames(nonmiss_data)){
  pct_missing <- sum(is.na(nonmiss_data[[col]]))/length(nonmiss_data[[col]])
  
  # dropping the column if >30% missingness 
  if (pct_missing >= 0.3){
    cat(col, ":", pct_missing,"\n")
    nonmiss_data[[col]] <- NULL
  }
}

# Originally 283 variables
dim(selected_data)

# 147 variables removed due to high degree of missingness, left with 136 variables
dim(nonmiss_data)

# Key variables removed: unemployment (0.4267281), youth unemployment (0.4792627), and GINI coefficient (0.6746544)
# need to add back despite high missingness 
nonmiss_data$gini_SI.POV.GINI <- selected_data$gini_SI.POV.GINI

nonmiss_data$unemployment_SL.UEM.TOTL.NE.ZS <- selected_data$unemployment_SL.UEM.TOTL.NE.ZS

nonmiss_data$unemployment_youth_SL.UEM.1524.NE.ZS <- selected_data$unemployment_youth_SL.UEM.1524.NE.ZS

nonmiss_data$lpi_LP.LPI.INFR.XQ <- selected_data$lpi_LP.LPI.INFR.XQ 

dim(nonmiss_data)

#### DATA IMPUTATION ####

# grouping by country, using mean imputation 
imputed_data <- mice(
  data = nonmiss_data,
  method = 'mean', # mean imputation
  m = 1,           
  maxit = 10,     
  seed = 123,      # Set seed for reproducibility
  by = nonmiss_data$country_code  # Group imputation by country
)

# getting complete dataset 
completed_data <- complete(imputed_data, 1)  

#### PCA FOR COMPOSITE VARIABLES ####

### volatility index

# getting variables measuing supply chain
supply_chain_volatility <- completed_data %>%
  dplyr::select(
    # LPI 
    lpi_LP.LPI.INFR.XQ, # bigger is better 
    # Freight Rate Indices
    transport_comm_export_TX.VAL.TRAN.ZS.WT, # bigger is better 
    transport_comm_import_TM.VAL.TRAN.ZS.WT, # smaller is better 
    transport_export_bop_BX.GSR.TRAN.ZS, # bigger is better 
    transport_import_bop_BM.GSR.TRAN.ZS, # smaller is better 
    # BOTH PPI and supply chain variables 
    manufacturing_GDP_NV.IND.MANF.ZS, # bigger is better 
    manufacturing_growth_NV.IND.MANF.KD.ZG, # bigger is better 
    manufacturer_exports_TX.VAL.MANF.ZS.UN, # bigger is better 
    manufacturer_imports_TM.VAL.MANF.ZS.UN, # smaller is better 
    # Producer Price Index (PPI) 
    exports_GDP_NE.EXP.GNFS.ZS, # bigger is better 
    exports_growth_NE.EXP.GNFS.KD.ZG, # bigger is better 
    import_unit_index_TM.UVI.MRCH.XD.WD, # bigger is better 
    import_value_index_TM.VAL.MRCH.XD.WD, # bigger is better 
    import_volume_index_TM.QTY.MRCH.XD.WD, # bigger is better 
    oil_rents_NY.GDP.PETR.RT.ZS, # smaller is better 
    ore_exports_TX.VAL.MMTL.ZS.UN, # bigger is better 
    ore_imports_TM.VAL.MMTL.ZS.UN, # smaller is better 
    # Supply Chain Volatility Index 
    net_trade_goods_service_BN.GSR.GNFS.CD, # bigger is better 
    net_trade_goods_BN.GSR.MRCH.CD, # bigger is better 
    trade_GDP_NE.TRD.GNFS.ZS, # bigger is better 
    trade_services_BG.GSR.NFSV.GD.ZS, # bigger is better 
    mechandise_exports_TX.VAL.MRCH.CD.WT, # bigger is better 
    mechandise_imports_TM.VAL.MRCH.CD.WT, # smaller is better 
    mechandise_trade_TG.VAL.TOTL.GD.ZS, # bigger is better 
    exports_GDP_NE.EXP.GNFS.ZS, # bigger is better 
    exports_growth_NE.EXP.GNFS.KD.ZG, # bigger is better 
    imports_GDP_NE.IMP.GNFS.ZS, # smaller is better 
    imports_growth_NE.IMP.GNFS.KD.ZG, # smaller is better 
    current_account_GDP_BN.CAB.XOKA.GD.ZS, # bigger is better 
    current_account_USD_BN.CAB.XOKA.CD # bigger is better 
  )

# noting which variables are marked as smaller values is better 
smaller_vars <- c(
  "transport_comm_import_TM.VAL.TRAN.ZS.WT", 
  "transport_import_bop_BM.GSR.TRAN.ZS", 
  "manufacturer_imports_TM.VAL.MANF.ZS.UN", 
  "oil_rents_NY.GDP.PETR.RT.ZS", 
  "ore_imports_TM.VAL.MMTL.ZS.UN", 
  "mechandise_imports_TM.VAL.MRCH.CD.WT", 
  "imports_GDP_NE.IMP.GNFS.ZS", 
  "imports_growth_NE.IMP.GNFS.KD.ZG"
)

# inversing "smaller is better" variables by getting reciprocal
supply_chain_volatility <- supply_chain_volatility %>%
  mutate(across(all_of(smaller_vars), 
                ~ if_else(. != 0 & !is.na(.), 1 / ., .), 
                .names = "reciprocal_{.col}"))
# needed for PCA as everything should be scaled and directionally aligned for best measure of multi-dimensional variance 

# calculating mean and variance for variables 
apply(supply_chain_volatility, 2, mean)

apply(supply_chain_volatility, 2, var)

# creating PCA with scaling 
pr_out_volatility <- prcomp(supply_chain_volatility, scale=TRUE)

# getting loadings for PCA
loadings_volatility <- pr_out_volatility$rotation

# extracts the PCA scores
pca_scores <- pr_out_volatility$x

# selecting the first column of the PCA scores AKA first principal component (PC1),
volatility_variable <- pca_scores[, 1]

# computes a weighted sum of the original variables using first PCA loading as weights 
volatility_variable_weighted <- as.matrix(supply_chain_volatility) %*% loadings_volatility[, 1]

# seeing how much of the variance is explained by PCA
explained_variance_volatility <- (pr_out_volatility$sdev)^2
explained_variance_ratio_volatility <- explained_variance_volatility / sum(explained_variance_volatility)
explained_variance_ratio_volatility[1]

# getting linear combination, squaring to remove negatives
completed_data$volatility_variable <- (volatility_variable)^2

### Cost index

# doing same for cost index as was done for volatility index

# getting variables measuring supply chain
supply_chain_cost <- supply_chain_volatility %>%
  dplyr::select(
    # Freight Rate Indices
    transport_comm_export_TX.VAL.TRAN.ZS.WT, 
    transport_comm_import_TM.VAL.TRAN.ZS.WT, 
    transport_export_bop_BX.GSR.TRAN.ZS, 
    transport_import_bop_BM.GSR.TRAN.ZS, 
    # BOTH PPI and supply chain variables 
    manufacturer_exports_TX.VAL.MANF.ZS.UN, 
    manufacturer_imports_TM.VAL.MANF.ZS.UN, 
    # Producer Price Index (PPI) 
    oil_rents_NY.GDP.PETR.RT.ZS, 
    ore_exports_TX.VAL.MMTL.ZS.UN, 
    ore_imports_TM.VAL.MMTL.ZS.UN, 
    # Supply Chain Volatility Index 
    net_trade_goods_service_BN.GSR.GNFS.CD, 
    net_trade_goods_BN.GSR.MRCH.CD, 
    exports_GDP_NE.EXP.GNFS.ZS, 
    imports_GDP_NE.IMP.GNFS.ZS, 
  )

# calculating mean and variance for variables 
apply(supply_chain_cost, 2, mean)

apply(supply_chain_cost, 2, var)

# creating PCA with scaling 
pr_out_cost <- prcomp(supply_chain_cost, scale=TRUE)

# getting loadings for PCA
loadings_cost <- pr_out_cost$rotation

# extracts the PCA scores
pca_scores <- pr_out_cost$x

# selecting the first column of the PCA scores AKA first principal component (PC1),
cost_variable <- pca_scores[, 1]

# computes a weighted sum of the original variables using first PCA loading as weights 
cost_variable_weighted <- as.matrix(supply_chain_cost) %*% loadings_cost[, 1]

# seeing how much of the variance is explained by PCA
explained_variance_cost <- (pr_out_cost$sdev)^2
explained_variance_ratio_cost <- explained_variance_cost / sum(explained_variance_cost)
explained_variance_ratio_cost[1]

# getting linear combination, squaring to remove negatives
completed_data$cost_variable <- (cost_variable)^2

#### SUBSETTING FOR DIFFERENT COUNTRIES/REGIONS ####

#G7
g7_countries <- c("Canada","France", "Germany", "Italy", "Japan", "United Kingdom", "United States")
g7 <- subset(completed_data, country_name %in% g7_countries)


#OECD
oecd_countries <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic",
                    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                    "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands",
                    "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
                    "Switzerland", "Turkey", "United Kingdom", "United States")
oecd <- subset(completed_data, country_name %in% oecd_countries)


# Global South Countries
south_countries <- c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Anguilla", "Antigua and Barbuda",
  "Argentina", "Armenia", "Aruba", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados",
  "Belarus", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana",
  "Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon",
  "Cape Verde", "Central African Republic", "Chad", "Channel Islands", "Chile", "China",
  "Colombia", "Comoros", "Congo", "Costa Rica", "Côte d'Ivoire", "Croatia", "Cuba", "Cyprus",
  "Czech Republic", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador",
  "Equatorial Guinea", "Eritrea", "Ethiopia", "Fiji", "French Guiana", "French Polynesia", "Gabon",
  "Gambia", "Georgia", "Ghana", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guinea", "Guinea-Bissau",
  "Guyana", "Haiti", "Honduras", "Hong Kong", "India", "Indonesia", "Iran", "Iraq", "Jamaica", "Jordan",
  "Kazakhstan", "Kenya", "Kiribati", "Korea, Democratic People's Republic of", "Kuwait", "Kyrgyzstan",
  "Lao People's Democratic Republic", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi",
  "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius",
  "Mayotte", "Mexico", "Micronesia, Federated States of", "Mongolia", "Montenegro", "Morocco", "Mozambique",
  "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands Antilles", "New Caledonia", "Nicaragua", "Niger",
  "Nigeria", "Oman", "Pakistan", "Palau", "Palestinian Territory, Occupied", "Panama", "Papua New Guinea",
  "Paraguay", "Peru", "Philippines", "Puerto Rico", "Qatar", "Republic of Moldova", "Réunion", "Romania",
  "Russian Federation", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
  "Samoa", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone",
  "Singapore", "Slovakia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Sri Lanka", "Sudan",
  "Suriname", "Swaziland", "Syrian Arab Republic", "Taiwan", "Tajikistan", "Tanzania", "Thailand",
  "The former Yugoslav Republic of Macedonia", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago",
  "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "Uruguay",
  "Uzbekistan", "Vanuatu", "Venezuela", "Viet Nam", "Virgin Islands", "Western Sahara", "Yemen", "Zambia",
  "Zimbabwe"
)

global.south <- subset(completed_data, country_name %in% south_countries)

# Global North Countries
north_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Denmark", "Estonia", "Finland", "France", "Germany",
  "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea, Republic of", "Latvia",
  "Lithuania", "Liechtenstein", "Luxembourg", "Monaco", "Netherlands", "New Zealand", "Norway", "Poland",
  "Portugal", "San Marino", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"
)

global.north <- subset(completed_data, country_name %in% north_countries)


#Top 20 highest gdp
gdp_countries <- c("United States", "China", "Germany", "Japan", "India", 
                   "United Kingdom", "France", "Italy", "Brazil", "Canada", 
                   "Russian Federation", "Mexico", "Australia", "Korea, Rep.", 
                   "Spain", "Indonesia", "Netherlands", "Turkiye", 
                   "Saudi Arabia", "Switzerland") 
countries <- subset(completed_data, country_name %in% gdp_countries)

#### CORRELATION INSIGHTS ####

#Q1. What patterns emerge between cost-of-living increases and supply chain disruptions?

#visualize trend in inflation 
ggplot(countries, aes(x = year, y = inflation, color = country_name)) +
  geom_line() +
  labs(title = "Inflation Trend Over Years", x = "Year", y = "Inflation (annual %)", color = "Country") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(countries$year)) + # Ensure all years are labeled
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels if needed for readability
  )

#### SCALING PREDICTOR VARIABLES ####

# selecting numeric variables 
numeric_vars <- countries %>%
  dplyr::select(where(is.numeric)) %>%
  names()

# scaling numeric variables 
countries[numeric_vars] <- scale(countries[numeric_vars])

# adding back in country code as a factor
countries$country_code <- factor(countries$country_code)

# adding observation year 
countries$year <- countries$year

# adding composite cost index 
countries$cost_variable <- countries$cost_variable

# adding composite volatility index  
countries$volatility_variable <- countries$volatility_variable

#### DETERMINE CORRELATION BETWEEN COST OF LIVING INDICATORS AND SUPPLY CHAIN VOLATILITY ####

# Define groups of variables
living_cost <- countries %>%
  dplyr::select(consumer_price,inflation, inflation_deflator, gini_SI.POV.GINI, net_nat_income_percapita_NY.ADJ.NNTY.PC.CD, net_nat_income_NY.ADJ.NNTY.CD, 
         household_expenditure_GDP, household_expenditure_per_capita, household_expenditure_ppp)

supply_chain <- countries %>%
  dplyr::select(# LPI 
    lpi_LP.LPI.INFR.XQ, 
    # Freight Rate Indices
    transport_comm_export_TX.VAL.TRAN.ZS.WT, 
    transport_comm_import_TM.VAL.TRAN.ZS.WT, 
    transport_export_bop_BX.GSR.TRAN.ZS, 
    transport_import_bop_BM.GSR.TRAN.ZS, 
    # BOTH PPI and supply chain variables 
    manufacturing_GDP_NV.IND.MANF.ZS, 
    manufacturing_growth_NV.IND.MANF.KD.ZG, 
    manufacturer_exports_TX.VAL.MANF.ZS.UN, 
    manufacturer_imports_TM.VAL.MANF.ZS.UN, 
    # Producer Price Index (PPI) 
    exports_GDP_NE.EXP.GNFS.ZS, 
    exports_growth_NE.EXP.GNFS.KD.ZG, 
    import_unit_index_TM.UVI.MRCH.XD.WD, 
    import_value_index_TM.VAL.MRCH.XD.WD, 
    import_volume_index_TM.QTY.MRCH.XD.WD, 
    oil_rents_NY.GDP.PETR.RT.ZS, 
    ore_exports_TX.VAL.MMTL.ZS.UN, 
    ore_imports_TM.VAL.MMTL.ZS.UN, 
    # Supply Chain Volatility Index 
    net_trade_goods_service_BN.GSR.GNFS.CD, 
    net_trade_goods_BN.GSR.MRCH.CD, 
    trade_GDP_NE.TRD.GNFS.ZS, 
    trade_services_BG.GSR.NFSV.GD.ZS, 
    mechandise_exports_TX.VAL.MRCH.CD.WT, 
    mechandise_imports_TM.VAL.MRCH.CD.WT, 
    mechandise_trade_TG.VAL.TOTL.GD.ZS, 
    exports_GDP_NE.EXP.GNFS.ZS, 
    exports_growth_NE.EXP.GNFS.KD.ZG, 
    imports_GDP_NE.IMP.GNFS.ZS, 
    imports_growth_NE.IMP.GNFS.KD.ZG, 
    current_account_GDP_BN.CAB.XOKA.GD.ZS, 
    current_account_USD_BN.CAB.XOKA.CD)
    
    
composite_supply_chain <- countries %>%
  dplyr::select(volatility_variable, cost_variable)

###correlation of cost of living and  supply chain indicators
#' Compute correlation matrix between group1 and group2
cor_matrix <- cor(living_cost, supply_chain,  method = "spearman")

#' correlation heatmap of composite 
corrplot(cor_matrix, method = "color", is.corr = TRUE, 
         tl.cex = 0.8, number.cex = 0.7,
         title = "Correlation of Cost of Living indicators with \n Supply Chain Indicators",
         mar = c(0, 0, 1, 0))
#store as df
cor_df <- as.data.frame(as.table(cor_matrix))

# Rename the columns for clarity
colnames(cor_df) <- c("Variable1", "Variable2", "Correlation")

# Filter for high correlation pairs (absolute correlation > 0.7)
high_cor_pairs <- subset(cor_df, abs(Correlation) > 0.7)

# Remove duplicate pairs (keeping one of each pair)
high_cor_pairs <- high_cor_pairs[!duplicated(t(apply(high_cor_pairs, 1, sort))), ]

# Output the high correlation pairs
print(high_cor_pairs)

# Cost of living predictors highly correlated with supply chain indicators
#household_expenditure_GDP WITH net_trade_goods_service_BN.GSR.GNFS.CD: (-0.7357246)
#household_expenditure_GDP WITH net_trade_goods_BN.GSR.MRCH.CD: (-0.7372171)
#household_expenditure_GDP WITH current_account_USD_BN.CAB.XOKA.CD: (-0.7065403)

###correlation of cost of living and composite supply chain indicators
#' Compute correlation matrix between group1 and group2
cor_matrix_composite <- cor(living_cost, composite_supply_chain,  method = "spearman")

#' correlation heatmap of composite 
corrplot(cor_matrix_composite, method = "color", is.corr = TRUE, 
         tl.cex = 0.8, number.cex = 0.7,
         title = "Correlation of Cost of Living indicators with \n Composite Supply Chain Volatility and Costs Indicators",
         mar = c(0, 0, 1, 0))
# nothing with huge correlation

#### REGRESSION MODELS ####
countries$net_nat_income_percapita_NY.ADJ.NNTY.PC.CD
#lm to test relationship between cost of living indicators and composite supply chain costs/volatility
model_cost <- lm(cost_variable ~ inflation+
                   inflation_deflator+
                   consumer_price+
                   net_nat_income_percapita_NY.ADJ.NNTY.PC.CD+
                   net_nat_income_NY.ADJ.NNTY.CD+
                   household_expenditure_GDP+
                   household_expenditure_per_capita+
                   household_expenditure_ppp+
                   gdp_percapita_NY.GDP.PCAP.PP.CD+ 
                   gdp_percapita_growth_NY.GDP.PCAP.KD.ZG+
                   gdp_deflator_NY.GDP.DEFL.ZS+   
                   life_expectancy_SP.DYN.LE00.IN+
                   health_expenditure_SH.XPD.CHEX.PP.CD+
                   health_expenditure_per_cap+
                   gini_SI.POV.GINI+
                   unemployment_SL.UEM.TOTL.NE.ZS+
                   unemployment_youth_SL.UEM.1524.NE.ZS, 
                 data = countries)

summary(model_cost)
# signficant: consumer_price, net_nat_income_percapita_NY.ADJ.NNTY.PC.CD, net_nat_income_NY.ADJ.NNTY.CD
# household_expenditure_GDP, gdp_percapita_NY.GDP.PCAP.PP.CD, gdp_deflator_NY.GDP.DEFL.ZS, gini_SI.POV.GINI    
# unemployment_youth_SL.UEM.1524.NE.ZS

model_volatility <- lm(volatility_variable ~ inflation+
                         inflation_deflator+
                         consumer_price+
                         net_nat_income_percapita_NY.ADJ.NNTY.PC.CD+
                         net_nat_income_NY.ADJ.NNTY.CD+
                         household_expenditure_GDP+
                         household_expenditure_per_capita+
                         household_expenditure_ppp+
                         gdp_percapita_NY.GDP.PCAP.PP.CD+ 
                         gdp_percapita_growth_NY.GDP.PCAP.KD.ZG+
                         gdp_deflator_NY.GDP.DEFL.ZS+   
                         life_expectancy_SP.DYN.LE00.IN+
                         health_expenditure_SH.XPD.CHEX.PP.CD+
                         health_expenditure_per_cap+
                         gini_SI.POV.GINI+
                         unemployment_SL.UEM.TOTL.NE.ZS+
                         unemployment_youth_SL.UEM.1524.NE.ZS, 
                       data = countries)

summary(model_volatility)
# significant: consumer_price, net_nat_income_NY.ADJ.NNTY.CD
# household_expenditure_ppp, gdp_percapita_NY.GDP.PCAP.PP.CD, 
# gdp_deflator_NY.GDP.DEFL.ZS, life_expectancy_SP.DYN.LE00.IN 
# unemployment_SL.UEM.TOTL.NE.ZS, unemployment_youth_SL.UEM.1524.NE.ZS

#also use variables with high correlation as response
model_netservice <- lm(net_trade_goods_service_BN.GSR.GNFS.CD ~ inflation+
                         inflation_deflator+
                         consumer_price+
                         net_nat_income_percapita_NY.ADJ.NNTY.PC.CD+
                         net_nat_income_NY.ADJ.NNTY.CD+
                         household_expenditure_GDP+
                         household_expenditure_per_capita+
                         household_expenditure_ppp+
                         gdp_percapita_NY.GDP.PCAP.PP.CD+ 
                         gdp_percapita_growth_NY.GDP.PCAP.KD.ZG+
                         gdp_deflator_NY.GDP.DEFL.ZS+   
                         life_expectancy_SP.DYN.LE00.IN+
                         health_expenditure_SH.XPD.CHEX.PP.CD+
                         health_expenditure_per_cap+
                         gini_SI.POV.GINI+
                         unemployment_SL.UEM.TOTL.NE.ZS+
                         unemployment_youth_SL.UEM.1524.NE.ZS, 
                       data = countries)

summary(model_netservice)


model_netgoods <- lm(net_trade_goods_BN.GSR.MRCH.CD ~ inflation+
                       inflation_deflator+
                       consumer_price+
                       net_nat_income_percapita_NY.ADJ.NNTY.PC.CD+
                       net_nat_income_NY.ADJ.NNTY.CD+
                       household_expenditure_GDP+
                       household_expenditure_per_capita+
                       household_expenditure_ppp+
                       gdp_percapita_NY.GDP.PCAP.PP.CD+ 
                       gdp_percapita_growth_NY.GDP.PCAP.KD.ZG+
                       gdp_deflator_NY.GDP.DEFL.ZS+   
                       life_expectancy_SP.DYN.LE00.IN+
                       health_expenditure_SH.XPD.CHEX.PP.CD+
                       health_expenditure_per_cap+
                       gini_SI.POV.GINI+
                       unemployment_SL.UEM.TOTL.NE.ZS+
                       unemployment_youth_SL.UEM.1524.NE.ZS, 
                     data = countries)

summary(model_netgoods)

model_current <- lm(current_account_USD_BN.CAB.XOKA.CD ~ inflation+
                         inflation_deflator+
                         consumer_price+
                         net_nat_income_percapita_NY.ADJ.NNTY.PC.CD+
                         net_nat_income_NY.ADJ.NNTY.CD+
                         household_expenditure_GDP+
                         household_expenditure_per_capita+
                         household_expenditure_ppp+
                         gdp_percapita_NY.GDP.PCAP.PP.CD+ 
                         gdp_percapita_growth_NY.GDP.PCAP.KD.ZG+
                         gdp_deflator_NY.GDP.DEFL.ZS+   
                         life_expectancy_SP.DYN.LE00.IN+
                         health_expenditure_SH.XPD.CHEX.PP.CD+
                         health_expenditure_per_cap+
                         gini_SI.POV.GINI+
                         unemployment_SL.UEM.TOTL.NE.ZS+
                         unemployment_youth_SL.UEM.1524.NE.ZS, 
                       data = countries)

summary(model_current)

# store predictors that were significant across all the models

# Store the models in a list
models <- list(model1 = model_cost, model2 = model_volatility, model3 = model_netservice, model4 = model_netgoods, 
               model5 = model_current)

# Initialize a list to store significant predictors for each model
significant_predictors_list <- list()

# Loop through each model to extract significant predictors
for (model_name in names(models)) {
  # Extract summary of the model
  model_summary <- summary(models[[model_name]])
  
  # Extract coefficients table
  coefficients_table <- model_summary$coefficients
  
  # Identify significant predictors (p-value < 0.05)
  significant <- rownames(coefficients_table)[coefficients_table[, "Pr(>|t|)"] < 0.05]
  
  # Remove intercept from the list of significant predictors
  significant <- significant[significant != "(Intercept)"]
  
  # Store significant predictors in the list
  significant_predictors_list[[model_name]] <- significant
}

#significant predictors across all models
print(significant_predictors_list)

# Find common significant predictors across all models
common_significant_predictors <- Reduce(intersect, significant_predictors_list)

# View the common significant predictors
print(common_significant_predictors) 

#6 significant across all models
# "consumer_price"                       "household_expenditure_GDP"           
# "gdp_percapita_NY.GDP.PCAP.PP.CD"      "unemployment_youth_SL.UEM.1524.NE.ZS"

#### MIXED-EFFECT MODELS ####

# Creating dataset for regression modeling 

# Constructing mixed effect model for supply chain volatility
mixed_model_volatility <- lmer(volatility_variable ~ inflation+
                                 inflation_deflator+
                                 consumer_price+
                                 net_nat_income_percapita_NY.ADJ.NNTY.PC.CD+
                                 net_nat_income_NY.ADJ.NNTY.CD+
                                 household_expenditure_GDP+
                                 household_expenditure_per_capita+
                                 household_expenditure_ppp+
                                 gdp_percapita_NY.GDP.PCAP.PP.CD+ 
                                 gdp_percapita_growth_NY.GDP.PCAP.KD.ZG+
                                 gdp_deflator_NY.GDP.DEFL.ZS+   
                                 life_expectancy_SP.DYN.LE00.IN+
                                 health_expenditure_SH.XPD.CHEX.PP.CD+
                                 health_expenditure_per_cap+
                                 gini_SI.POV.GINI+
                                 unemployment_SL.UEM.TOTL.NE.ZS+
                                 unemployment_youth_SL.UEM.1524.NE.ZS+
                                 (1|country_code), 
                               data = countries)

summary(mixed_model_volatility)
# household_expenditure_ppp, gdp_percapita_NY.GDP.PCAP.PP.CD is SIGNIFICANT, 
# consumer_price is kinda close 

# Testing assumptions for mixed effect volatility model 
plot(mixed_model_volatility)
# some uneven spread in residuals 

qqnorm(resid(mixed_model_volatility))

qqline(resid(mixed_model_volatility))
# some concerns of normality w/ qq line 

# Constructing mixed effect model for supply chain costs
mixed_model_cost<- lmer(cost_variable ~ inflation+
                          inflation_deflator+
                          consumer_price+
                          net_nat_income_percapita_NY.ADJ.NNTY.PC.CD+
                          net_nat_income_NY.ADJ.NNTY.CD+
                          household_expenditure_GDP+
                          household_expenditure_per_capita+
                          household_expenditure_ppp+
                          gdp_percapita_NY.GDP.PCAP.PP.CD+ 
                          gdp_percapita_growth_NY.GDP.PCAP.KD.ZG+
                          gdp_deflator_NY.GDP.DEFL.ZS+   
                          life_expectancy_SP.DYN.LE00.IN+
                          health_expenditure_SH.XPD.CHEX.PP.CD+
                          health_expenditure_per_cap+
                          gini_SI.POV.GINI+
                          unemployment_SL.UEM.TOTL.NE.ZS+
                          unemployment_youth_SL.UEM.1524.NE.ZS+
                          (1|country_code), 
                        data = countries)

summary(mixed_model_cost)
# gdp_percapita_NY.GDP.PCAP.PP.CD is closest but nothing is statistically significant
# second is household_expenditure_ppp, health_expenditure_per_cap health_expenditure_SH.XPD.CHEX.PP.CD 

# Testing assumptions for mixed effect cost model 
plot(mixed_model_cost)
qqnorm(resid(mixed_model_cost))
qqline(resid(mixed_model_cost))

#### GG PLOTS ####

# Plotting the relationship of net_nat_income and supply chain volatility
ggplot(countries, aes(x = year)) +
  geom_line(aes(y = consumer_price, color = "Net nat")) + # Line for Inflation
  geom_line(aes(y = mechandise_exports_TX.VAL.MRCH.CD.WT, color = "Supply Chain Volatility")) + # Line for Supply Chain Indicator
  scale_y_continuous(
    name = "Net nat",
    sec.axis = sec_axis(~., name = "Supply Chain Volatility")
  ) +
  labs(title = "net_nat_income vs. Supply Chain Volatility Over Years", x = "Year") +
  scale_color_manual(values = c("Net nat" = "blue", "Supply Chain Volatility" = "red")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  ) + 
  facet_wrap(~ country_name, scales = "free_y") + # Creates a separate plot for each country
  theme(
    strip.text = element_text(size = 10), # Customize facet labels
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for readability
  ) 

# Plotting the relationship of inflatoin and supply chain costs
ggplot(countries, aes(x = year)) +
  geom_line(aes(y = inflation, color = "Inflation Rate")) + # Line for Inflation
  geom_line(aes(y = cost_variable, color = "Supply Chain Costs")) + # Line for Supply Chain Indicator
  scale_y_continuous(
    name = "Inflation Rate (%)",
    sec.axis = sec_axis(~., name = "Supply Chain Costs")
  ) +
  labs(title = "Inflation vs. Supply Chain Costs Over Years", x = "Year") +
  scale_color_manual(values = c("Inflation Rate" = "blue", "Supply Chain Costs" = "red")) +
  theme_minimal() + scale_x_continuous(breaks = unique(countries$year)) +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  ) +
  facet_wrap(~ country_name, scales = "free_y") + # Creates a separate plot for each country
  theme(
    strip.text = element_text(size = 10), # Customize facet labels
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for readability
  )

ggplot(countries, aes(x = year)) +
  geom_line(aes(y = household_expenditure_ppp, color = "Net nat")) + # Line for Inflation
  geom_line(aes(y = mechandise_exports_TX.VAL.MRCH.CD.WT, color = "Supply Chain Volatility")) + # Line for Supply Chain Indicator
  scale_y_continuous(
    name = "Net nat",
    sec.axis = sec_axis(~., name = "Supply Chain Volatility")
  ) +
  labs(title = "net_nat_income vs. Supply Chain Volatility Over Years", x = "Year") +
  scale_color_manual(values = c("Net nat" = "blue", "Supply Chain Volatility" = "red")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  ) + 
  facet_wrap(~ country_name, scales = "free_y") + # Creates a separate plot for each country
  theme(
    strip.text = element_text(size = 10), # Customize facet labels
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for readability
  ) 

#### Forecasting #### 
forecasting_data <- subset(completed_data, country_name %in% gdp_countries)
forecasting_data <- forecasting_data %>%
  dplyr::select(country_name,
                country_code,
                year,
                time_code,
                gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
                household_expenditure_per_capita,
                gdp_deflator_NY.GDP.DEFL.ZS,
                unemployment_SL.UEM.TOTL.NE.ZS,
                gini_SI.POV.GINI,
                cost_variable)
# Convert to date, assume the data is from the beginning of the year
forecasting_data <- forecasting_data %>% mutate(year = as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"))

str(forecasting_data)


###### Australia
australia_data <- forecasting_data %>%
  filter(country_name == "Australia")

# GDP Growth
aus_gdp_growth_ts <- ts(
  australia_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(australia_data$year), "%Y")),
  frequency = 1
)

aus_arima_model_gdp <- auto.arima(aus_gdp_growth_ts)
summary(aus_arima_model_gdp)

forecasted_gdp_growth <- forecast(aus_arima_model_gdp, h = 5)

plot(
  forecasted_gdp_growth,
  main = "Forecasted GDP Growth for Australia",
  ylab = "GDP Growth (%)",
  xlab = "Year"
)

# Household Expenditure
aus_household_ts <- ts(
  australia_data$household_expenditure_per_capita,
  start = as.numeric(format(min(australia_data$year), "%Y")),
  frequency = 1
)

aus_arima_model_household <- auto.arima(aus_household_ts)
summary(aus_arima_model_household)

forecasted_household <- forecast(aus_arima_model_household, h = 5)

plot(
  forecasted_household,
  main = "Forecasted Household Expenditure for Australia",
  ylab = "Household Expenditure per Capita (US$)",
  xlab = "Year"
)

# GDP Deflator
aus_gdp_deflator_ts <- ts(
  australia_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(australia_data$year), "%Y")),
  frequency = 1
)

aus_arima_model_deflator <- auto.arima(aus_gdp_deflator_ts)
summary(aus_arima_model_deflator)

forecasted_deflator <- forecast(aus_arima_model_deflator, h = 5)

plot(
  forecasted_deflator,
  main = "Forecasted GDP Deflator for Australia",
  ylab = "GDP Deflator (%)",
  xlab = "Year"
)

# Unemployment
aus_unemployment_ts <- ts(
  australia_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(australia_data$year), "%Y")),
  frequency = 1
)

aus_arima_model_unemployment <- auto.arima(aus_unemployment_ts)
summary(aus_arima_model_unemployment)

forecasted_unemployment <- forecast(aus_arima_model_unemployment, h = 5)

plot(
  forecasted_unemployment,
  main = "Forecasted Unemployment for Australia",
  ylab = "Unemployment (%)",
  xlab = "Year"
)

# Gini Index
aus_gini_ts <- ts(
  australia_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(australia_data$year), "%Y")),
  frequency = 1
)

aus_arima_model_gini <- auto.arima(aus_gini_ts)
summary(aus_arima_model_gini)

forecasted_gini <- forecast(aus_arima_model_gini, h = 5)

plot(
  forecasted_gini,
  main = "Forecasted Gini Index for Australia",
  ylab = "Gini Index",
  xlab = "Year"
)


# Extract forecasted predictor values
future_gdp_growth <- as.numeric(forecasted_gdp_growth$mean)
future_household <- as.numeric(forecasted_household$mean)
future_gdp_deflator <- as.numeric(forecasted_deflator$mean)
future_unemployment <- as.numeric(forecasted_unemployment$mean)
future_gini <- as.numeric(forecasted_gini$mean)

# Combine historical data and forecasted predictors into a dataframe
historical_predictors <- data.frame(
  gdp_growth = as.numeric(aus_gdp_growth_ts),
  household = as.numeric(aus_household_ts),
  gdp_deflator = as.numeric(aus_gdp_deflator_ts),
  unemployment = as.numeric(aus_unemployment_ts),
  gini = as.numeric(aus_gini_ts)
)

# Align historical predictors with the cost variable
aus_cost_variable_ts <- ts(
  australia_data$cost_variable,
  start = as.numeric(format(min(australia_data$year), "%Y")),
  frequency = 1
)

# Fit a regression model using tslm
tslm_model <- tslm(aus_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_predictors)
summary(tslm_model)

# Create future predictors for the forecast
future_predictors <- data.frame(
  gdp_growth = future_gdp_growth,
  household = future_household,
  gdp_deflator = future_gdp_deflator,
  unemployment = future_unemployment,
  gini = future_gini
)

# Forecast cost_variable using the regression model
forecasted_cost_variable <- forecast(tslm_model, newdata = future_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_cost_variable,
  main = "Forecasted Cost Variable for Australia",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Extract historical data for predictors
historical_gdp_growth <- as.numeric(aus_gdp_growth_ts)
historical_household <- as.numeric(aus_household_ts)
historical_gdp_deflator <- as.numeric(aus_gdp_deflator_ts)
historical_unemployment <- as.numeric(aus_unemployment_ts)
historical_gini <- as.numeric(aus_gini_ts)
historical_cost_variable <- as.numeric(aus_cost_variable_ts)

# Combine all historical predictors and forecasted predictors
all_gdp_growth <- c(historical_gdp_growth, future_gdp_growth)
all_household <- c(historical_household, future_household)
all_gdp_deflator <- c(historical_gdp_deflator, future_gdp_deflator)
all_unemployment <- c(historical_unemployment, future_unemployment)
all_gini <- c(historical_gini, future_gini)

# Combine historical and forecasted cost_variable
forecasted_cost_variable_values <- as.numeric(forecasted_cost_variable$mean)
all_cost_variable <- c(historical_cost_variable, forecasted_cost_variable_values)

# Create a new year column to align historical and forecasted years
years <- seq(from = as.numeric(format(min(australia_data$year), "%Y")),
             to = as.numeric(format(min(australia_data$year), "%Y")) + length(all_gdp_growth) - 1)

# Create a new dataframe combining historical and forecasted values
australia_data_updated <- data.frame(
  year = years,
  gdp_growth = all_gdp_growth,
  household = all_household,
  gdp_deflator = all_gdp_deflator,
  unemployment = all_unemployment,
  gini = all_gini,
  cost_variable = all_cost_variable
)

# Print the updated dataframe to ensure correctness
print(head(australia_data_updated))  # View the first few rows
print(tail(australia_data_updated))  # View the last few rows



###### Brazil
brazil_data <- forecasting_data %>%
  filter(country_name == "Brazil")

# GDP Growth
brazil_gdp_growth_ts <- ts(
  brazil_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(brazil_data$year), "%Y")),
  frequency = 1
)

brazil_arima_model_gdp <- auto.arima(brazil_gdp_growth_ts)
forecasted_brazil_gdp_growth <- forecast(brazil_arima_model_gdp, h = 5)

# Household Expenditure
brazil_household_ts <- ts(
  brazil_data$household_expenditure_per_capita,
  start = as.numeric(format(min(brazil_data$year), "%Y")),
  frequency = 1
)

brazil_arima_model_household <- auto.arima(brazil_household_ts)
forecasted_brazil_household <- forecast(brazil_arima_model_household, h = 5)

# GDP Deflator
brazil_gdp_deflator_ts <- ts(
  brazil_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(brazil_data$year), "%Y")),
  frequency = 1
)

brazil_arima_model_deflator <- auto.arima(brazil_gdp_deflator_ts)
forecasted_brazil_deflator <- forecast(brazil_arima_model_deflator, h = 5)

# Unemployment
brazil_unemployment_ts <- ts(
  brazil_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(brazil_data$year), "%Y")),
  frequency = 1
)

brazil_arima_model_unemployment <- auto.arima(brazil_unemployment_ts)
forecasted_brazil_unemployment <- forecast(brazil_arima_model_unemployment, h = 5)

# Gini Index
brazil_gini_ts <- ts(
  brazil_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(brazil_data$year), "%Y")),
  frequency = 1
)

brazil_arima_model_gini <- auto.arima(brazil_gini_ts)
forecasted_brazil_gini <- forecast(brazil_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_brazil_gdp_growth <- as.numeric(forecasted_brazil_gdp_growth$mean)
future_brazil_household <- as.numeric(forecasted_brazil_household$mean)
future_brazil_gdp_deflator <- as.numeric(forecasted_brazil_deflator$mean)
future_brazil_unemployment <- as.numeric(forecasted_brazil_unemployment$mean)
future_brazil_gini <- as.numeric(forecasted_brazil_gini$mean)

# Combine historical data and forecasted predictors into a dataframe
historical_brazil_predictors <- data.frame(
  gdp_growth = as.numeric(brazil_gdp_growth_ts),
  household = as.numeric(brazil_household_ts),
  gdp_deflator = as.numeric(brazil_gdp_deflator_ts),
  unemployment = as.numeric(brazil_unemployment_ts),
  gini = as.numeric(brazil_gini_ts)
)

# Align historical predictors with the cost variable
brazil_cost_variable_ts <- ts(
  brazil_data$cost_variable,
  start = as.numeric(format(min(brazil_data$year), "%Y")),
  frequency = 1
)

# Fit a regression model using tslm
brazil_tslm_model <- tslm(brazil_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_brazil_predictors)
summary(brazil_tslm_model)

# Create future predictors for the forecast
future_brazil_predictors <- data.frame(
  gdp_growth = future_brazil_gdp_growth,
  household = future_brazil_household,
  gdp_deflator = future_brazil_gdp_deflator,
  unemployment = future_brazil_unemployment,
  gini = future_brazil_gini
)

# Forecast cost_variable using the regression model
forecasted_brazil_cost_variable <- forecast(brazil_tslm_model, newdata = future_brazil_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_brazil_cost_variable,
  main = "Forecasted Cost Variable for Brazil",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Extract historical data for predictors
historical_brazil_gdp_growth <- as.numeric(brazil_gdp_growth_ts)
historical_brazil_household <- as.numeric(brazil_household_ts)
historical_brazil_gdp_deflator <- as.numeric(brazil_gdp_deflator_ts)
historical_brazil_unemployment <- as.numeric(brazil_unemployment_ts)
historical_brazil_gini <- as.numeric(brazil_gini_ts)
historical_brazil_cost_variable <- as.numeric(brazil_cost_variable_ts)

# Combine all historical predictors and forecasted predictors
all_brazil_gdp_growth <- c(historical_brazil_gdp_growth, future_brazil_gdp_growth)
all_brazil_household <- c(historical_brazil_household, future_brazil_household)
all_brazil_gdp_deflator <- c(historical_brazil_gdp_deflator, future_brazil_gdp_deflator)
all_brazil_unemployment <- c(historical_brazil_unemployment, future_brazil_unemployment)
all_brazil_gini <- c(historical_brazil_gini, future_brazil_gini)

# Combine historical and forecasted cost_variable
forecasted_brazil_cost_variable_values <- as.numeric(forecasted_brazil_cost_variable$mean)
all_brazil_cost_variable <- c(historical_brazil_cost_variable, forecasted_brazil_cost_variable_values)

# Create a new year column to align historical and forecasted years
brazil_years <- seq(from = as.numeric(format(min(brazil_data$year), "%Y")),
                    to = as.numeric(format(min(brazil_data$year), "%Y")) + length(all_brazil_gdp_growth) - 1)

# Create a new dataframe combining historical and forecasted values
brazil_data_updated <- data.frame(
  year = brazil_years,
  gdp_growth = all_brazil_gdp_growth,
  household = all_brazil_household,
  gdp_deflator = all_brazil_gdp_deflator,
  unemployment = all_brazil_unemployment,
  gini = all_brazil_gini,
  cost_variable = all_brazil_cost_variable
)


###### Canada
canada_data <- forecasting_data %>%
  filter(country_name == "Canada")

# GDP Growth
canada_gdp_growth_ts <- ts(
  canada_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(canada_data$year), "%Y")),
  frequency = 1
)

canada_arima_model_gdp <- auto.arima(canada_gdp_growth_ts)
forecasted_canada_gdp_growth <- forecast(canada_arima_model_gdp, h = 5)

# Household Expenditure
canada_household_ts <- ts(
  canada_data$household_expenditure_per_capita,
  start = as.numeric(format(min(canada_data$year), "%Y")),
  frequency = 1
)

canada_arima_model_household <- auto.arima(canada_household_ts)
forecasted_canada_household <- forecast(canada_arima_model_household, h = 5)

# GDP Deflator
canada_gdp_deflator_ts <- ts(
  canada_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(canada_data$year), "%Y")),
  frequency = 1
)

canada_arima_model_deflator <- auto.arima(canada_gdp_deflator_ts)
forecasted_canada_deflator <- forecast(canada_arima_model_deflator, h = 5)

# Unemployment
canada_unemployment_ts <- ts(
  canada_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(canada_data$year), "%Y")),
  frequency = 1
)

canada_arima_model_unemployment <- auto.arima(canada_unemployment_ts)
forecasted_canada_unemployment <- forecast(canada_arima_model_unemployment, h = 5)

# Gini Index
canada_gini_ts <- ts(
  canada_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(canada_data$year), "%Y")),
  frequency = 1
)

canada_arima_model_gini <- auto.arima(canada_gini_ts)
forecasted_canada_gini <- forecast(canada_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_canada_gdp_growth <- as.numeric(forecasted_canada_gdp_growth$mean)
future_canada_household <- as.numeric(forecasted_canada_household$mean)
future_canada_gdp_deflator <- as.numeric(forecasted_canada_deflator$mean)
future_canada_unemployment <- as.numeric(forecasted_canada_unemployment$mean)
future_canada_gini <- as.numeric(forecasted_canada_gini$mean)

# Combine historical data and forecasted predictors into a dataframe
historical_canada_predictors <- data.frame(
  gdp_growth = as.numeric(canada_gdp_growth_ts),
  household = as.numeric(canada_household_ts),
  gdp_deflator = as.numeric(canada_gdp_deflator_ts),
  unemployment = as.numeric(canada_unemployment_ts),
  gini = as.numeric(canada_gini_ts)
)

# Align historical predictors with the cost variable
canada_cost_variable_ts <- ts(
  canada_data$cost_variable,
  start = as.numeric(format(min(canada_data$year), "%Y")),
  frequency = 1
)

# Fit a regression model using tslm
canada_tslm_model <- tslm(canada_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_canada_predictors)
summary(canada_tslm_model)

# Create future predictors for the forecast
future_canada_predictors <- data.frame(
  gdp_growth = future_canada_gdp_growth,
  household = future_canada_household,
  gdp_deflator = future_canada_gdp_deflator,
  unemployment = future_canada_unemployment,
  gini = future_canada_gini
)

# Forecast cost_variable using the regression model
forecasted_canada_cost_variable <- forecast(canada_tslm_model, newdata = future_canada_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_canada_cost_variable,
  main = "Forecasted Cost Variable for Canada",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine all historical predictors and forecasted predictors
all_canada_gdp_growth <- c(as.numeric(canada_gdp_growth_ts), future_canada_gdp_growth)
all_canada_household <- c(as.numeric(canada_household_ts), future_canada_household)
all_canada_gdp_deflator <- c(as.numeric(canada_gdp_deflator_ts), future_canada_gdp_deflator)
all_canada_unemployment <- c(as.numeric(canada_unemployment_ts), future_canada_unemployment)
all_canada_gini <- c(as.numeric(canada_gini_ts), future_canada_gini)

# Combine historical and forecasted cost_variable
forecasted_canada_cost_variable_values <- as.numeric(forecasted_canada_cost_variable$mean)
all_canada_cost_variable <- c(as.numeric(canada_cost_variable_ts), forecasted_canada_cost_variable_values)

# Create a new year column to align historical and forecasted years
canada_years <- seq(from = as.numeric(format(min(canada_data$year), "%Y")),
                    to = as.numeric(format(min(canada_data$year), "%Y")) + length(all_canada_gdp_growth) - 1)

# Create a new dataframe combining historical and forecasted values
canada_data_updated <- data.frame(
  year = canada_years,
  gdp_growth = all_canada_gdp_growth,
  household = all_canada_household,
  gdp_deflator = all_canada_gdp_deflator,
  unemployment = all_canada_unemployment,
  gini = all_canada_gini,
  cost_variable = all_canada_cost_variable
)

###### China
china_data <- forecasting_data %>%
  filter(country_name == "China")

# GDP Growth
china_gdp_growth_ts <- ts(
  china_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(china_data$year), "%Y")),
  frequency = 1
)

china_arima_model_gdp <- auto.arima(china_gdp_growth_ts)
forecasted_china_gdp_growth <- forecast(china_arima_model_gdp, h = 5)

# Household Expenditure
china_household_ts <- ts(
  china_data$household_expenditure_per_capita,
  start = as.numeric(format(min(china_data$year), "%Y")),
  frequency = 1
)

china_arima_model_household <- auto.arima(china_household_ts)
forecasted_china_household <- forecast(china_arima_model_household, h = 5)

# GDP Deflator
china_gdp_deflator_ts <- ts(
  china_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(china_data$year), "%Y")),
  frequency = 1
)

china_arima_model_deflator <- auto.arima(china_gdp_deflator_ts)
forecasted_china_deflator <- forecast(china_arima_model_deflator, h = 5)

# Unemployment
china_unemployment_ts <- ts(
  china_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(china_data$year), "%Y")),
  frequency = 1
)

china_arima_model_unemployment <- auto.arima(china_unemployment_ts)
forecasted_china_unemployment <- forecast(china_arima_model_unemployment, h = 5)

# Gini Index
china_gini_ts <- ts(
  china_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(china_data$year), "%Y")),
  frequency = 1
)

china_arima_model_gini <- auto.arima(china_gini_ts)
forecasted_china_gini <- forecast(china_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_china_gdp_growth <- as.numeric(forecasted_china_gdp_growth$mean)
future_china_household <- as.numeric(forecasted_china_household$mean)
future_china_gdp_deflator <- as.numeric(forecasted_china_deflator$mean)
future_china_unemployment <- as.numeric(forecasted_china_unemployment$mean)
future_china_gini <- as.numeric(forecasted_china_gini$mean)

# Combine historical data and forecasted predictors 
historical_china_predictors <- data.frame(
  gdp_growth = as.numeric(china_gdp_growth_ts),
  household = as.numeric(china_household_ts),
  gdp_deflator = as.numeric(china_gdp_deflator_ts),
  unemployment = as.numeric(china_unemployment_ts),
  gini = as.numeric(china_gini_ts)
)

# Create a time series for cost_variable
china_cost_variable_ts <- ts(
  china_data$cost_variable,
  start = as.numeric(format(min(china_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
china_tslm_model <- tslm(china_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_china_predictors)
summary(china_tslm_model)

# Create future predictors for the forecast
future_china_predictors <- data.frame(
  gdp_growth = future_china_gdp_growth,
  household = future_china_household,
  gdp_deflator = future_china_gdp_deflator,
  unemployment = future_china_unemployment,
  gini = future_china_gini
)

# Forecast cost_variable 
forecasted_china_cost_variable <- forecast(china_tslm_model, newdata = future_china_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_china_cost_variable,
  main = "Forecasted Cost Variable for China",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_china_gdp_growth <- c(as.numeric(china_gdp_growth_ts), future_china_gdp_growth)
all_china_household <- c(as.numeric(china_household_ts), future_china_household)
all_china_gdp_deflator <- c(as.numeric(china_gdp_deflator_ts), future_china_gdp_deflator)
all_china_unemployment <- c(as.numeric(china_unemployment_ts), future_china_unemployment)
all_china_gini <- c(as.numeric(china_gini_ts), future_china_gini)

# Combine historical and forecasted cost_variable
forecasted_china_cost_variable_values <- as.numeric(forecasted_china_cost_variable$mean)
all_china_cost_variable <- c(as.numeric(china_cost_variable_ts), forecasted_china_cost_variable_values)

# Create year column
china_years <- seq(from = as.numeric(format(min(china_data$year), "%Y")),
                   to = as.numeric(format(min(china_data$year), "%Y")) + length(all_china_gdp_growth) - 1)

# Get the historical cost variable
historical_china_cost_variable <- as.numeric(china_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_china_cost_variable_lo80 <- c(rep(NA, length(historical_china_cost_variable)),
                                         forecasted_china_cost_variable$lower[, 1])
forecasted_china_cost_variable_hi80 <- c(rep(NA, length(historical_china_cost_variable)),
                                         forecasted_china_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
china_data_updated <- data.frame(
  year = china_years,
  gdp_growth = all_china_gdp_growth,
  household = all_china_household,
  gdp_deflator = all_china_gdp_deflator,
  unemployment = all_china_unemployment,
  gini = all_china_gini,
  cost_variable = all_china_cost_variable,
  cost_variable_lo80 = forecasted_china_cost_variable_lo80,
  cost_variable_hi80 = forecasted_china_cost_variable_hi80
)

###### France
# Filter the data for France
france_data <- forecasting_data %>%
  filter(country_name == "France")

# GDP Growth
france_gdp_growth_ts <- ts(
  france_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(france_data$year), "%Y")),
  frequency = 1
)

france_arima_model_gdp <- auto.arima(france_gdp_growth_ts)
forecasted_france_gdp_growth <- forecast(france_arima_model_gdp, h = 5)

# Household Expenditure
france_household_ts <- ts(
  france_data$household_expenditure_per_capita,
  start = as.numeric(format(min(france_data$year), "%Y")),
  frequency = 1
)

france_arima_model_household <- auto.arima(france_household_ts)
forecasted_france_household <- forecast(france_arima_model_household, h = 5)

# GDP Deflator
france_gdp_deflator_ts <- ts(
  france_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(france_data$year), "%Y")),
  frequency = 1
)

france_arima_model_deflator <- auto.arima(france_gdp_deflator_ts)
forecasted_france_deflator <- forecast(france_arima_model_deflator, h = 5)

# Unemployment
france_unemployment_ts <- ts(
  france_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(france_data$year), "%Y")),
  frequency = 1
)

france_arima_model_unemployment <- auto.arima(france_unemployment_ts)
forecasted_france_unemployment <- forecast(france_arima_model_unemployment, h = 5)

# Gini Index
france_gini_ts <- ts(
  france_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(france_data$year), "%Y")),
  frequency = 1
)

france_arima_model_gini <- auto.arima(france_gini_ts)
forecasted_france_gini <- forecast(france_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_france_gdp_growth <- as.numeric(forecasted_france_gdp_growth$mean)
future_france_household <- as.numeric(forecasted_france_household$mean)
future_france_gdp_deflator <- as.numeric(forecasted_france_deflator$mean)
future_france_unemployment <- as.numeric(forecasted_france_unemployment$mean)
future_france_gini <- as.numeric(forecasted_france_gini$mean)

# Combine historical data and forecasted predictors 
historical_france_predictors <- data.frame(
  gdp_growth = as.numeric(france_gdp_growth_ts),
  household = as.numeric(france_household_ts),
  gdp_deflator = as.numeric(france_gdp_deflator_ts),
  unemployment = as.numeric(france_unemployment_ts),
  gini = as.numeric(france_gini_ts)
)

# Create a time series for cost_variable
france_cost_variable_ts <- ts(
  france_data$cost_variable,
  start = as.numeric(format(min(france_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
france_tslm_model <- tslm(france_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_france_predictors)
summary(france_tslm_model)

# Create future predictors for the forecast
future_france_predictors <- data.frame(
  gdp_growth = future_france_gdp_growth,
  household = future_france_household,
  gdp_deflator = future_france_gdp_deflator,
  unemployment = future_france_unemployment,
  gini = future_france_gini
)

# Forecast cost_variable 
forecasted_france_cost_variable <- forecast(france_tslm_model, newdata = future_france_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_france_cost_variable,
  main = "Forecasted Cost Variable for France",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_france_gdp_growth <- c(as.numeric(france_gdp_growth_ts), future_france_gdp_growth)
all_france_household <- c(as.numeric(france_household_ts), future_france_household)
all_france_gdp_deflator <- c(as.numeric(france_gdp_deflator_ts), future_france_gdp_deflator)
all_france_unemployment <- c(as.numeric(france_unemployment_ts), future_france_unemployment)
all_france_gini <- c(as.numeric(france_gini_ts), future_france_gini)

# Combine historical and forecasted cost_variable
forecasted_france_cost_variable_values <- as.numeric(forecasted_france_cost_variable$mean)
all_france_cost_variable <- c(as.numeric(france_cost_variable_ts), forecasted_france_cost_variable_values)

# Create year column
france_years <- seq(from = as.numeric(format(min(france_data$year), "%Y")),
                    to = as.numeric(format(min(france_data$year), "%Y")) + length(all_france_gdp_growth) - 1)

# Get the historical cost variable
historical_france_cost_variable <- as.numeric(france_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_france_cost_variable_lo80 <- c(rep(NA, length(historical_france_cost_variable)),
                                          forecasted_france_cost_variable$lower[, 1])
forecasted_france_cost_variable_hi80 <- c(rep(NA, length(historical_france_cost_variable)),
                                          forecasted_france_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
france_data_updated <- data.frame(
  year = france_years,
  gdp_growth = all_france_gdp_growth,
  household = all_france_household,
  gdp_deflator = all_france_gdp_deflator,
  unemployment = all_france_unemployment,
  gini = all_france_gini,
  cost_variable = all_france_cost_variable,
  cost_variable_lo80 = forecasted_france_cost_variable_lo80,
  cost_variable_hi80 = forecasted_france_cost_variable_hi80
)

###### Germany
# Filter the data for Germany
germany_data <- forecasting_data %>%
  filter(country_name == "Germany")

# GDP Growth
germany_gdp_growth_ts <- ts(
  germany_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(germany_data$year), "%Y")),
  frequency = 1
)

germany_arima_model_gdp <- auto.arima(germany_gdp_growth_ts)
forecasted_germany_gdp_growth <- forecast(germany_arima_model_gdp, h = 5)

# Household Expenditure
germany_household_ts <- ts(
  germany_data$household_expenditure_per_capita,
  start = as.numeric(format(min(germany_data$year), "%Y")),
  frequency = 1
)

germany_arima_model_household <- auto.arima(germany_household_ts)
forecasted_germany_household <- forecast(germany_arima_model_household, h = 5)

# GDP Deflator
germany_gdp_deflator_ts <- ts(
  germany_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(germany_data$year), "%Y")),
  frequency = 1
)

germany_arima_model_deflator <- auto.arima(germany_gdp_deflator_ts)
forecasted_germany_deflator <- forecast(germany_arima_model_deflator, h = 5)

# Unemployment
germany_unemployment_ts <- ts(
  germany_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(germany_data$year), "%Y")),
  frequency = 1
)

germany_arima_model_unemployment <- auto.arima(germany_unemployment_ts)
forecasted_germany_unemployment <- forecast(germany_arima_model_unemployment, h = 5)

# Gini Index
germany_gini_ts <- ts(
  germany_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(germany_data$year), "%Y")),
  frequency = 1
)

germany_arima_model_gini <- auto.arima(germany_gini_ts)
forecasted_germany_gini <- forecast(germany_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_germany_gdp_growth <- as.numeric(forecasted_germany_gdp_growth$mean)
future_germany_household <- as.numeric(forecasted_germany_household$mean)
future_germany_gdp_deflator <- as.numeric(forecasted_germany_deflator$mean)
future_germany_unemployment <- as.numeric(forecasted_germany_unemployment$mean)
future_germany_gini <- as.numeric(forecasted_germany_gini$mean)

# Combine historical data and forecasted predictors 
historical_germany_predictors <- data.frame(
  gdp_growth = as.numeric(germany_gdp_growth_ts),
  household = as.numeric(germany_household_ts),
  gdp_deflator = as.numeric(germany_gdp_deflator_ts),
  unemployment = as.numeric(germany_unemployment_ts),
  gini = as.numeric(germany_gini_ts)
)

# Create a time series for cost_variable
germany_cost_variable_ts <- ts(
  germany_data$cost_variable,
  start = as.numeric(format(min(germany_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
germany_tslm_model <- tslm(germany_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_germany_predictors)
summary(germany_tslm_model)

# Create future predictors for the forecast
future_germany_predictors <- data.frame(
  gdp_growth = future_germany_gdp_growth,
  household = future_germany_household,
  gdp_deflator = future_germany_gdp_deflator,
  unemployment = future_germany_unemployment,
  gini = future_germany_gini
)

# Forecast cost_variable 
forecasted_germany_cost_variable <- forecast(germany_tslm_model, newdata = future_germany_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_germany_cost_variable,
  main = "Forecasted Cost Variable for Germany",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_germany_gdp_growth <- c(as.numeric(germany_gdp_growth_ts), future_germany_gdp_growth)
all_germany_household <- c(as.numeric(germany_household_ts), future_germany_household)
all_germany_gdp_deflator <- c(as.numeric(germany_gdp_deflator_ts), future_germany_gdp_deflator)
all_germany_unemployment <- c(as.numeric(germany_unemployment_ts), future_germany_unemployment)
all_germany_gini <- c(as.numeric(germany_gini_ts), future_germany_gini)

# Combine historical and forecasted cost_variable
forecasted_germany_cost_variable_values <- as.numeric(forecasted_germany_cost_variable$mean)
all_germany_cost_variable <- c(as.numeric(germany_cost_variable_ts), forecasted_germany_cost_variable_values)

# Create year column
germany_years <- seq(from = as.numeric(format(min(germany_data$year), "%Y")),
                     to = as.numeric(format(min(germany_data$year), "%Y")) + length(all_germany_gdp_growth) - 1)

# Get the historical cost variable
historical_germany_cost_variable <- as.numeric(germany_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_germany_cost_variable_lo80 <- c(rep(NA, length(historical_germany_cost_variable)),
                                           forecasted_germany_cost_variable$lower[, 1])
forecasted_germany_cost_variable_hi80 <- c(rep(NA, length(historical_germany_cost_variable)),
                                           forecasted_germany_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
germany_data_updated <- data.frame(
  year = germany_years,
  gdp_growth = all_germany_gdp_growth,
  household = all_germany_household,
  gdp_deflator = all_germany_gdp_deflator,
  unemployment = all_germany_unemployment,
  gini = all_germany_gini,
  cost_variable = all_germany_cost_variable,
  cost_variable_lo80 = forecasted_germany_cost_variable_lo80,
  cost_variable_hi80 = forecasted_germany_cost_variable_hi80
)

###### India
# Filter the data for India
india_data <- forecasting_data %>%
  filter(country_name == "India")

# GDP Growth
india_gdp_growth_ts <- ts(
  india_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(india_data$year), "%Y")),
  frequency = 1
)

india_arima_model_gdp <- auto.arima(india_gdp_growth_ts)
forecasted_india_gdp_growth <- forecast(india_arima_model_gdp, h = 5)

# Household Expenditure
india_household_ts <- ts(
  india_data$household_expenditure_per_capita,
  start = as.numeric(format(min(india_data$year), "%Y")),
  frequency = 1
)

india_arima_model_household <- auto.arima(india_household_ts)
forecasted_india_household <- forecast(india_arima_model_household, h = 5)

# GDP Deflator
india_gdp_deflator_ts <- ts(
  india_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(india_data$year), "%Y")),
  frequency = 1
)

india_arima_model_deflator <- auto.arima(india_gdp_deflator_ts)
forecasted_india_deflator <- forecast(india_arima_model_deflator, h = 5)

# Unemployment
india_unemployment_ts <- ts(
  india_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(india_data$year), "%Y")),
  frequency = 1
)

india_arima_model_unemployment <- auto.arima(india_unemployment_ts)
forecasted_india_unemployment <- forecast(india_arima_model_unemployment, h = 5)

# Gini Index
india_gini_ts <- ts(
  india_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(india_data$year), "%Y")),
  frequency = 1
)

india_arima_model_gini <- auto.arima(india_gini_ts)
forecasted_india_gini <- forecast(india_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_india_gdp_growth <- as.numeric(forecasted_india_gdp_growth$mean)
future_india_household <- as.numeric(forecasted_india_household$mean)
future_india_gdp_deflator <- as.numeric(forecasted_india_deflator$mean)
future_india_unemployment <- as.numeric(forecasted_india_unemployment$mean)
future_india_gini <- as.numeric(forecasted_india_gini$mean)

# Combine historical data and forecasted predictors 
historical_india_predictors <- data.frame(
  gdp_growth = as.numeric(india_gdp_growth_ts),
  household = as.numeric(india_household_ts),
  gdp_deflator = as.numeric(india_gdp_deflator_ts),
  unemployment = as.numeric(india_unemployment_ts),
  gini = as.numeric(india_gini_ts)
)

# Create a time series for cost_variable
india_cost_variable_ts <- ts(
  india_data$cost_variable,
  start = as.numeric(format(min(india_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
india_tslm_model <- tslm(india_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_india_predictors)
summary(india_tslm_model)

# Create future predictors for the forecast
future_india_predictors <- data.frame(
  gdp_growth = future_india_gdp_growth,
  household = future_india_household,
  gdp_deflator = future_india_gdp_deflator,
  unemployment = future_india_unemployment,
  gini = future_india_gini
)

# Forecast cost_variable 
forecasted_india_cost_variable <- forecast(india_tslm_model, newdata = future_india_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_india_cost_variable,
  main = "Forecasted Cost Variable for India",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_india_gdp_growth <- c(as.numeric(india_gdp_growth_ts), future_india_gdp_growth)
all_india_household <- c(as.numeric(india_household_ts), future_india_household)
all_india_gdp_deflator <- c(as.numeric(india_gdp_deflator_ts), future_india_gdp_deflator)
all_india_unemployment <- c(as.numeric(india_unemployment_ts), future_india_unemployment)
all_india_gini <- c(as.numeric(india_gini_ts), future_india_gini)

# Combine historical and forecasted cost_variable
forecasted_india_cost_variable_values <- as.numeric(forecasted_india_cost_variable$mean)
all_india_cost_variable <- c(as.numeric(india_cost_variable_ts), forecasted_india_cost_variable_values)

# Create year column
india_years <- seq(from = as.numeric(format(min(india_data$year), "%Y")),
                   to = as.numeric(format(min(india_data$year), "%Y")) + length(all_india_gdp_growth) - 1)

# Get the historical cost variable
historical_india_cost_variable <- as.numeric(india_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_india_cost_variable_lo80 <- c(rep(NA, length(historical_india_cost_variable)),
                                         forecasted_india_cost_variable$lower[, 1])
forecasted_india_cost_variable_hi80 <- c(rep(NA, length(historical_india_cost_variable)),
                                         forecasted_india_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
india_data_updated <- data.frame(
  year = india_years,
  gdp_growth = all_india_gdp_growth,
  household = all_india_household,
  gdp_deflator = all_india_gdp_deflator,
  unemployment = all_india_unemployment,
  gini = all_india_gini,
  cost_variable = all_india_cost_variable,
  cost_variable_lo80 = forecasted_india_cost_variable_lo80,
  cost_variable_hi80 = forecasted_india_cost_variable_hi80
)

###### Indonesia
# Filter the data for Indonesia
indonesia_data <- forecasting_data %>%
  filter(country_name == "Indonesia")

# GDP Growth
indonesia_gdp_growth_ts <- ts(
  indonesia_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(indonesia_data$year), "%Y")),
  frequency = 1
)

indonesia_arima_model_gdp <- auto.arima(indonesia_gdp_growth_ts)
forecasted_indonesia_gdp_growth <- forecast(indonesia_arima_model_gdp, h = 5)

# Household Expenditure
indonesia_household_ts <- ts(
  indonesia_data$household_expenditure_per_capita,
  start = as.numeric(format(min(indonesia_data$year), "%Y")),
  frequency = 1
)

indonesia_arima_model_household <- auto.arima(indonesia_household_ts)
forecasted_indonesia_household <- forecast(indonesia_arima_model_household, h = 5)

# GDP Deflator
indonesia_gdp_deflator_ts <- ts(
  indonesia_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(indonesia_data$year), "%Y")),
  frequency = 1
)

indonesia_arima_model_deflator <- auto.arima(indonesia_gdp_deflator_ts)
forecasted_indonesia_deflator <- forecast(indonesia_arima_model_deflator, h = 5)

# Unemployment
indonesia_unemployment_ts <- ts(
  indonesia_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(indonesia_data$year), "%Y")),
  frequency = 1
)

indonesia_arima_model_unemployment <- auto.arima(indonesia_unemployment_ts)
forecasted_indonesia_unemployment <- forecast(indonesia_arima_model_unemployment, h = 5)

# Gini Index
indonesia_gini_ts <- ts(
  indonesia_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(indonesia_data$year), "%Y")),
  frequency = 1
)

indonesia_arima_model_gini <- auto.arima(indonesia_gini_ts)
forecasted_indonesia_gini <- forecast(indonesia_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_indonesia_gdp_growth <- as.numeric(forecasted_indonesia_gdp_growth$mean)
future_indonesia_household <- as.numeric(forecasted_indonesia_household$mean)
future_indonesia_gdp_deflator <- as.numeric(forecasted_indonesia_deflator$mean)
future_indonesia_unemployment <- as.numeric(forecasted_indonesia_unemployment$mean)
future_indonesia_gini <- as.numeric(forecasted_indonesia_gini$mean)

# Combine historical data and forecasted predictors 
historical_indonesia_predictors <- data.frame(
  gdp_growth = as.numeric(indonesia_gdp_growth_ts),
  household = as.numeric(indonesia_household_ts),
  gdp_deflator = as.numeric(indonesia_gdp_deflator_ts),
  unemployment = as.numeric(indonesia_unemployment_ts),
  gini = as.numeric(indonesia_gini_ts)
)

# Create a time series for cost_variable
indonesia_cost_variable_ts <- ts(
  indonesia_data$cost_variable,
  start = as.numeric(format(min(indonesia_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
indonesia_tslm_model <- tslm(indonesia_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_indonesia_predictors)
summary(indonesia_tslm_model)

# Create future predictors for the forecast
future_indonesia_predictors <- data.frame(
  gdp_growth = future_indonesia_gdp_growth,
  household = future_indonesia_household,
  gdp_deflator = future_indonesia_gdp_deflator,
  unemployment = future_indonesia_unemployment,
  gini = future_indonesia_gini
)

# Forecast cost_variable 
forecasted_indonesia_cost_variable <- forecast(indonesia_tslm_model, newdata = future_indonesia_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_indonesia_cost_variable,
  main = "Forecasted Cost Variable for Indonesia",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_indonesia_gdp_growth <- c(as.numeric(indonesia_gdp_growth_ts), future_indonesia_gdp_growth)
all_indonesia_household <- c(as.numeric(indonesia_household_ts), future_indonesia_household)
all_indonesia_gdp_deflator <- c(as.numeric(indonesia_gdp_deflator_ts), future_indonesia_gdp_deflator)
all_indonesia_unemployment <- c(as.numeric(indonesia_unemployment_ts), future_indonesia_unemployment)
all_indonesia_gini <- c(as.numeric(indonesia_gini_ts), future_indonesia_gini)

# Combine historical and forecasted cost_variable
forecasted_indonesia_cost_variable_values <- as.numeric(forecasted_indonesia_cost_variable$mean)
all_indonesia_cost_variable <- c(as.numeric(indonesia_cost_variable_ts), forecasted_indonesia_cost_variable_values)

# Create year column
indonesia_years <- seq(from = as.numeric(format(min(indonesia_data$year), "%Y")),
                       to = as.numeric(format(min(indonesia_data$year), "%Y")) + length(all_indonesia_gdp_growth) - 1)

# Get the historical cost variable
historical_indonesia_cost_variable <- as.numeric(indonesia_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_indonesia_cost_variable_lo80 <- c(rep(NA, length(historical_indonesia_cost_variable)),
                                             forecasted_indonesia_cost_variable$lower[, 1])
forecasted_indonesia_cost_variable_hi80 <- c(rep(NA, length(historical_indonesia_cost_variable)),
                                             forecasted_indonesia_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
indonesia_data_updated <- data.frame(
  year = indonesia_years,
  gdp_growth = all_indonesia_gdp_growth,
  household = all_indonesia_household,
  gdp_deflator = all_indonesia_gdp_deflator,
  unemployment = all_indonesia_unemployment,
  gini = all_indonesia_gini,
  cost_variable = all_indonesia_cost_variable,
  cost_variable_lo80 = forecasted_indonesia_cost_variable_lo80,
  cost_variable_hi80 = forecasted_indonesia_cost_variable_hi80
)

###### Italy
# Filter the data for Italy
italy_data <- forecasting_data %>%
  filter(country_name == "Italy")

# GDP Growth
italy_gdp_growth_ts <- ts(
  italy_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(italy_data$year), "%Y")),
  frequency = 1
)

italy_arima_model_gdp <- auto.arima(italy_gdp_growth_ts)
forecasted_italy_gdp_growth <- forecast(italy_arima_model_gdp, h = 5)

# Household Expenditure
italy_household_ts <- ts(
  italy_data$household_expenditure_per_capita,
  start = as.numeric(format(min(italy_data$year), "%Y")),
  frequency = 1
)

italy_arima_model_household <- auto.arima(italy_household_ts)
forecasted_italy_household <- forecast(italy_arima_model_household, h = 5)

# GDP Deflator
italy_gdp_deflator_ts <- ts(
  italy_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(italy_data$year), "%Y")),
  frequency = 1
)

italy_arima_model_deflator <- auto.arima(italy_gdp_deflator_ts)
forecasted_italy_deflator <- forecast(italy_arima_model_deflator, h = 5)

# Unemployment
italy_unemployment_ts <- ts(
  italy_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(italy_data$year), "%Y")),
  frequency = 1
)

italy_arima_model_unemployment <- auto.arima(italy_unemployment_ts)
forecasted_italy_unemployment <- forecast(italy_arima_model_unemployment, h = 5)

# Gini Index
italy_gini_ts <- ts(
  italy_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(italy_data$year), "%Y")),
  frequency = 1
)

italy_arima_model_gini <- auto.arima(italy_gini_ts)
forecasted_italy_gini <- forecast(italy_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_italy_gdp_growth <- as.numeric(forecasted_italy_gdp_growth$mean)
future_italy_household <- as.numeric(forecasted_italy_household$mean)
future_italy_gdp_deflator <- as.numeric(forecasted_italy_deflator$mean)
future_italy_unemployment <- as.numeric(forecasted_italy_unemployment$mean)
future_italy_gini <- as.numeric(forecasted_italy_gini$mean)

# Combine historical data and forecasted predictors 
historical_italy_predictors <- data.frame(
  gdp_growth = as.numeric(italy_gdp_growth_ts),
  household = as.numeric(italy_household_ts),
  gdp_deflator = as.numeric(italy_gdp_deflator_ts),
  unemployment = as.numeric(italy_unemployment_ts),
  gini = as.numeric(italy_gini_ts)
)

# Create a time series for cost_variable
italy_cost_variable_ts <- ts(
  italy_data$cost_variable,
  start = as.numeric(format(min(italy_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
italy_tslm_model <- tslm(italy_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_italy_predictors)
summary(italy_tslm_model)

# Create future predictors for the forecast
future_italy_predictors <- data.frame(
  gdp_growth = future_italy_gdp_growth,
  household = future_italy_household,
  gdp_deflator = future_italy_gdp_deflator,
  unemployment = future_italy_unemployment,
  gini = future_italy_gini
)

# Forecast cost_variable 
forecasted_italy_cost_variable <- forecast(italy_tslm_model, newdata = future_italy_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_italy_cost_variable,
  main = "Forecasted Cost Variable for Italy",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_italy_gdp_growth <- c(as.numeric(italy_gdp_growth_ts), future_italy_gdp_growth)
all_italy_household <- c(as.numeric(italy_household_ts), future_italy_household)
all_italy_gdp_deflator <- c(as.numeric(italy_gdp_deflator_ts), future_italy_gdp_deflator)
all_italy_unemployment <- c(as.numeric(italy_unemployment_ts), future_italy_unemployment)
all_italy_gini <- c(as.numeric(italy_gini_ts), future_italy_gini)

# Combine historical and forecasted cost_variable
forecasted_italy_cost_variable_values <- as.numeric(forecasted_italy_cost_variable$mean)
all_italy_cost_variable <- c(as.numeric(italy_cost_variable_ts), forecasted_italy_cost_variable_values)

# Create year column
italy_years <- seq(from = as.numeric(format(min(italy_data$year), "%Y")),
                   to = as.numeric(format(min(italy_data$year), "%Y")) + length(all_italy_gdp_growth) - 1)

# Get the historical cost variable
historical_italy_cost_variable <- as.numeric(italy_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_italy_cost_variable_lo80 <- c(rep(NA, length(historical_italy_cost_variable)),
                                         forecasted_italy_cost_variable$lower[, 1])
forecasted_italy_cost_variable_hi80 <- c(rep(NA, length(historical_italy_cost_variable)),
                                         forecasted_italy_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
italy_data_updated <- data.frame(
  year = italy_years,
  gdp_growth = all_italy_gdp_growth,
  household = all_italy_household,
  gdp_deflator = all_italy_gdp_deflator,
  unemployment = all_italy_unemployment,
  gini = all_italy_gini,
  cost_variable = all_italy_cost_variable,
  cost_variable_lo80 = forecasted_italy_cost_variable_lo80,
  cost_variable_hi80 = forecasted_italy_cost_variable_hi80
)

###### Japan
# Filter the data for Japan
japan_data <- forecasting_data %>%
  filter(country_name == "Japan")

# GDP Growth
japan_gdp_growth_ts <- ts(
  japan_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(japan_data$year), "%Y")),
  frequency = 1
)

japan_arima_model_gdp <- auto.arima(japan_gdp_growth_ts)
forecasted_japan_gdp_growth <- forecast(japan_arima_model_gdp, h = 5)

# Household Expenditure
japan_household_ts <- ts(
  japan_data$household_expenditure_per_capita,
  start = as.numeric(format(min(japan_data$year), "%Y")),
  frequency = 1
)

japan_arima_model_household <- auto.arima(japan_household_ts)
forecasted_japan_household <- forecast(japan_arima_model_household, h = 5)

# GDP Deflator
japan_gdp_deflator_ts <- ts(
  japan_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(japan_data$year), "%Y")),
  frequency = 1
)

japan_arima_model_deflator <- auto.arima(japan_gdp_deflator_ts)
forecasted_japan_deflator <- forecast(japan_arima_model_deflator, h = 5)

# Unemployment
japan_unemployment_ts <- ts(
  japan_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(japan_data$year), "%Y")),
  frequency = 1
)

japan_arima_model_unemployment <- auto.arima(japan_unemployment_ts)
forecasted_japan_unemployment <- forecast(japan_arima_model_unemployment, h = 5)

# Gini Index
japan_gini_ts <- ts(
  japan_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(japan_data$year), "%Y")),
  frequency = 1
)

japan_arima_model_gini <- auto.arima(japan_gini_ts)
forecasted_japan_gini <- forecast(japan_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_japan_gdp_growth <- as.numeric(forecasted_japan_gdp_growth$mean)
future_japan_household <- as.numeric(forecasted_japan_household$mean)
future_japan_gdp_deflator <- as.numeric(forecasted_japan_deflator$mean)
future_japan_unemployment <- as.numeric(forecasted_japan_unemployment$mean)
future_japan_gini <- as.numeric(forecasted_japan_gini$mean)

# Combine historical data and forecasted predictors 
historical_japan_predictors <- data.frame(
  gdp_growth = as.numeric(japan_gdp_growth_ts),
  household = as.numeric(japan_household_ts),
  gdp_deflator = as.numeric(japan_gdp_deflator_ts),
  unemployment = as.numeric(japan_unemployment_ts),
  gini = as.numeric(japan_gini_ts)
)

# Create a time series for cost_variable
japan_cost_variable_ts <- ts(
  japan_data$cost_variable,
  start = as.numeric(format(min(japan_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
japan_tslm_model <- tslm(japan_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_japan_predictors)
summary(japan_tslm_model)

# Create future predictors for the forecast
future_japan_predictors <- data.frame(
  gdp_growth = future_japan_gdp_growth,
  household = future_japan_household,
  gdp_deflator = future_japan_gdp_deflator,
  unemployment = future_japan_unemployment,
  gini = future_japan_gini
)

# Forecast cost_variable 
forecasted_japan_cost_variable <- forecast(japan_tslm_model, newdata = future_japan_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_japan_cost_variable,
  main = "Forecasted Cost Variable for Japan",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_japan_gdp_growth <- c(as.numeric(japan_gdp_growth_ts), future_japan_gdp_growth)
all_japan_household <- c(as.numeric(japan_household_ts), future_japan_household)
all_japan_gdp_deflator <- c(as.numeric(japan_gdp_deflator_ts), future_japan_gdp_deflator)
all_japan_unemployment <- c(as.numeric(japan_unemployment_ts), future_japan_unemployment)
all_japan_gini <- c(as.numeric(japan_gini_ts), future_japan_gini)

# Combine historical and forecasted cost_variable
forecasted_japan_cost_variable_values <- as.numeric(forecasted_japan_cost_variable$mean)
all_japan_cost_variable <- c(as.numeric(japan_cost_variable_ts), forecasted_japan_cost_variable_values)

# Create year column
japan_years <- seq(from = as.numeric(format(min(japan_data$year), "%Y")),
                   to = as.numeric(format(min(japan_data$year), "%Y")) + length(all_japan_gdp_growth) - 1)

# Get the historical cost variable
historical_japan_cost_variable <- as.numeric(japan_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_japan_cost_variable_lo80 <- c(rep(NA, length(historical_japan_cost_variable)),
                                         forecasted_japan_cost_variable$lower[, 1])
forecasted_japan_cost_variable_hi80 <- c(rep(NA, length(historical_japan_cost_variable)),
                                         forecasted_japan_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
japan_data_updated <- data.frame(
  year = japan_years,
  gdp_growth = all_japan_gdp_growth,
  household = all_japan_household,
  gdp_deflator = all_japan_gdp_deflator,
  unemployment = all_japan_unemployment,
  gini = all_japan_gini,
  cost_variable = all_japan_cost_variable,
  cost_variable_lo80 = forecasted_japan_cost_variable_lo80,
  cost_variable_hi80 = forecasted_japan_cost_variable_hi80
)

###### Korea, Rep.
# Filter the data for Korea, Rep.
korea_rep_data <- forecasting_data %>%
  filter(country_name == "Korea, Rep.")

# GDP Growth
korea_rep_gdp_growth_ts <- ts(
  korea_rep_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(korea_rep_data$year), "%Y")),
  frequency = 1
)

korea_rep_arima_model_gdp <- auto.arima(korea_rep_gdp_growth_ts)
forecasted_korea_rep_gdp_growth <- forecast(korea_rep_arima_model_gdp, h = 5)

# Household Expenditure
korea_rep_household_ts <- ts(
  korea_rep_data$household_expenditure_per_capita,
  start = as.numeric(format(min(korea_rep_data$year), "%Y")),
  frequency = 1
)

korea_rep_arima_model_household <- auto.arima(korea_rep_household_ts)
forecasted_korea_rep_household <- forecast(korea_rep_arima_model_household, h = 5)

# GDP Deflator
korea_rep_gdp_deflator_ts <- ts(
  korea_rep_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(korea_rep_data$year), "%Y")),
  frequency = 1
)

korea_rep_arima_model_deflator <- auto.arima(korea_rep_gdp_deflator_ts)
forecasted_korea_rep_deflator <- forecast(korea_rep_arima_model_deflator, h = 5)

# Unemployment
korea_rep_unemployment_ts <- ts(
  korea_rep_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(korea_rep_data$year), "%Y")),
  frequency = 1
)

korea_rep_arima_model_unemployment <- auto.arima(korea_rep_unemployment_ts)
forecasted_korea_rep_unemployment <- forecast(korea_rep_arima_model_unemployment, h = 5)

# Gini Index
korea_rep_gini_ts <- ts(
  korea_rep_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(korea_rep_data$year), "%Y")),
  frequency = 1
)

korea_rep_arima_model_gini <- auto.arima(korea_rep_gini_ts)
forecasted_korea_rep_gini <- forecast(korea_rep_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_korea_rep_gdp_growth <- as.numeric(forecasted_korea_rep_gdp_growth$mean)
future_korea_rep_household <- as.numeric(forecasted_korea_rep_household$mean)
future_korea_rep_gdp_deflator <- as.numeric(forecasted_korea_rep_deflator$mean)
future_korea_rep_unemployment <- as.numeric(forecasted_korea_rep_unemployment$mean)
future_korea_rep_gini <- as.numeric(forecasted_korea_rep_gini$mean)

# Combine historical data and forecasted predictors 
historical_korea_rep_predictors <- data.frame(
  gdp_growth = as.numeric(korea_rep_gdp_growth_ts),
  household = as.numeric(korea_rep_household_ts),
  gdp_deflator = as.numeric(korea_rep_gdp_deflator_ts),
  unemployment = as.numeric(korea_rep_unemployment_ts),
  gini = as.numeric(korea_rep_gini_ts)
)

# Create a time series for cost_variable
korea_rep_cost_variable_ts <- ts(
  korea_rep_data$cost_variable,
  start = as.numeric(format(min(korea_rep_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
korea_rep_tslm_model <- tslm(korea_rep_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_korea_rep_predictors)
summary(korea_rep_tslm_model)

# Create future predictors for the forecast
future_korea_rep_predictors <- data.frame(
  gdp_growth = future_korea_rep_gdp_growth,
  household = future_korea_rep_household,
  gdp_deflator = future_korea_rep_gdp_deflator,
  unemployment = future_korea_rep_unemployment,
  gini = future_korea_rep_gini
)

# Forecast cost_variable 
forecasted_korea_rep_cost_variable <- forecast(korea_rep_tslm_model, newdata = future_korea_rep_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_korea_rep_cost_variable,
  main = "Forecasted Cost Variable for Korea, Rep.",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_korea_rep_gdp_growth <- c(as.numeric(korea_rep_gdp_growth_ts), future_korea_rep_gdp_growth)
all_korea_rep_household <- c(as.numeric(korea_rep_household_ts), future_korea_rep_household)
all_korea_rep_gdp_deflator <- c(as.numeric(korea_rep_gdp_deflator_ts), future_korea_rep_gdp_deflator)
all_korea_rep_unemployment <- c(as.numeric(korea_rep_unemployment_ts), future_korea_rep_unemployment)
all_korea_rep_gini <- c(as.numeric(korea_rep_gini_ts), future_korea_rep_gini)

# Combine historical and forecasted cost_variable
forecasted_korea_rep_cost_variable_values <- as.numeric(forecasted_korea_rep_cost_variable$mean)
all_korea_rep_cost_variable <- c(as.numeric(korea_rep_cost_variable_ts), forecasted_korea_rep_cost_variable_values)

# Create year column
korea_rep_years <- seq(from = as.numeric(format(min(korea_rep_data$year), "%Y")),
                       to = as.numeric(format(min(korea_rep_data$year), "%Y")) + length(all_korea_rep_gdp_growth) - 1)

# Get the historical cost variable
historical_korea_rep_cost_variable <- as.numeric(korea_rep_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_korea_rep_cost_variable_lo80 <- c(rep(NA, length(historical_korea_rep_cost_variable)),
                                             forecasted_korea_rep_cost_variable$lower[, 1])
forecasted_korea_rep_cost_variable_hi80 <- c(rep(NA, length(historical_korea_rep_cost_variable)),
                                             forecasted_korea_rep_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
korea_rep_data_updated <- data.frame(
  year = korea_rep_years,
  gdp_growth = all_korea_rep_gdp_growth,
  household = all_korea_rep_household,
  gdp_deflator = all_korea_rep_gdp_deflator,
  unemployment = all_korea_rep_unemployment,
  gini = all_korea_rep_gini,
  cost_variable = all_korea_rep_cost_variable,
  cost_variable_lo80 = forecasted_korea_rep_cost_variable_lo80,
  cost_variable_hi80 = forecasted_korea_rep_cost_variable_hi80
)

###### Mexico
# Filter the data for Mexico
mexico_data <- forecasting_data %>%
  filter(country_name == "Mexico")

# GDP Growth
mexico_gdp_growth_ts <- ts(
  mexico_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(mexico_data$year), "%Y")),
  frequency = 1
)

mexico_arima_model_gdp <- auto.arima(mexico_gdp_growth_ts)
forecasted_mexico_gdp_growth <- forecast(mexico_arima_model_gdp, h = 5)

# Household Expenditure
mexico_household_ts <- ts(
  mexico_data$household_expenditure_per_capita,
  start = as.numeric(format(min(mexico_data$year), "%Y")),
  frequency = 1
)

mexico_arima_model_household <- auto.arima(mexico_household_ts)
forecasted_mexico_household <- forecast(mexico_arima_model_household, h = 5)

# GDP Deflator
mexico_gdp_deflator_ts <- ts(
  mexico_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(mexico_data$year), "%Y")),
  frequency = 1
)

mexico_arima_model_deflator <- auto.arima(mexico_gdp_deflator_ts)
forecasted_mexico_deflator <- forecast(mexico_arima_model_deflator, h = 5)

# Unemployment
mexico_unemployment_ts <- ts(
  mexico_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(mexico_data$year), "%Y")),
  frequency = 1
)

mexico_arima_model_unemployment <- auto.arima(mexico_unemployment_ts)
forecasted_mexico_unemployment <- forecast(mexico_arima_model_unemployment, h = 5)

# Gini Index
mexico_gini_ts <- ts(
  mexico_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(mexico_data$year), "%Y")),
  frequency = 1
)

mexico_arima_model_gini <- auto.arima(mexico_gini_ts)
forecasted_mexico_gini <- forecast(mexico_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_mexico_gdp_growth <- as.numeric(forecasted_mexico_gdp_growth$mean)
future_mexico_household <- as.numeric(forecasted_mexico_household$mean)
future_mexico_gdp_deflator <- as.numeric(forecasted_mexico_deflator$mean)
future_mexico_unemployment <- as.numeric(forecasted_mexico_unemployment$mean)
future_mexico_gini <- as.numeric(forecasted_mexico_gini$mean)

# Combine historical data and forecasted predictors 
historical_mexico_predictors <- data.frame(
  gdp_growth = as.numeric(mexico_gdp_growth_ts),
  household = as.numeric(mexico_household_ts),
  gdp_deflator = as.numeric(mexico_gdp_deflator_ts),
  unemployment = as.numeric(mexico_unemployment_ts),
  gini = as.numeric(mexico_gini_ts)
)

# Create a time series for cost_variable
mexico_cost_variable_ts <- ts(
  mexico_data$cost_variable,
  start = as.numeric(format(min(mexico_data$year), "%Y")),
  frequency = 1
)

# Fit the tslm regression
mexico_tslm_model <- tslm(mexico_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_mexico_predictors)
summary(mexico_tslm_model)

# Create future predictors for the forecast
future_mexico_predictors <- data.frame(
  gdp_growth = future_mexico_gdp_growth,
  household = future_mexico_household,
  gdp_deflator = future_mexico_gdp_deflator,
  unemployment = future_mexico_unemployment,
  gini = future_mexico_gini
)

# Forecast cost_variable 
forecasted_mexico_cost_variable <- forecast(mexico_tslm_model, newdata = future_mexico_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_mexico_cost_variable,
  main = "Forecasted Cost Variable for Mexico",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_mexico_gdp_growth <- c(as.numeric(mexico_gdp_growth_ts), future_mexico_gdp_growth)
all_mexico_household <- c(as.numeric(mexico_household_ts), future_mexico_household)
all_mexico_gdp_deflator <- c(as.numeric(mexico_gdp_deflator_ts), future_mexico_gdp_deflator)
all_mexico_unemployment <- c(as.numeric(mexico_unemployment_ts), future_mexico_unemployment)
all_mexico_gini <- c(as.numeric(mexico_gini_ts), future_mexico_gini)

# Combine historical and forecasted cost_variable
forecasted_mexico_cost_variable_values <- as.numeric(forecasted_mexico_cost_variable$mean)
all_mexico_cost_variable <- c(as.numeric(mexico_cost_variable_ts), forecasted_mexico_cost_variable_values)

# Create year column
mexico_years <- seq(from = as.numeric(format(min(mexico_data$year), "%Y")),
                    to = as.numeric(format(min(mexico_data$year), "%Y")) + length(all_mexico_gdp_growth) - 1)

# Get the historical cost variable
historical_mexico_cost_variable <- as.numeric(mexico_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_mexico_cost_variable_lo80 <- c(rep(NA, length(historical_mexico_cost_variable)),
                                          forecasted_mexico_cost_variable$lower[, 1])
forecasted_mexico_cost_variable_hi80 <- c(rep(NA, length(historical_mexico_cost_variable)),
                                          forecasted_mexico_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
mexico_data_updated <- data.frame(
  year = mexico_years,
  gdp_growth = all_mexico_gdp_growth,
  household = all_mexico_household,
  gdp_deflator = all_mexico_gdp_deflator,
  unemployment = all_mexico_unemployment,
  gini = all_mexico_gini,
  cost_variable = all_mexico_cost_variable,
  cost_variable_lo80 = forecasted_mexico_cost_variable_lo80,
  cost_variable_hi80 = forecasted_mexico_cost_variable_hi80
)

###### Netherlands
# Filter the data for Netherlands
netherlands_data <- forecasting_data %>%
  filter(country_name == "Netherlands")

# GDP Growth
netherlands_gdp_growth_ts <- ts(
  netherlands_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(netherlands_data$year), "%Y")),
  frequency = 1
)

netherlands_arima_model_gdp <- auto.arima(netherlands_gdp_growth_ts)
forecasted_netherlands_gdp_growth <- forecast(netherlands_arima_model_gdp, h = 5)

# Household Expenditure
netherlands_household_ts <- ts(
  netherlands_data$household_expenditure_per_capita,
  start = as.numeric(format(min(netherlands_data$year), "%Y")),
  frequency = 1
)

netherlands_arima_model_household <- auto.arima(netherlands_household_ts)
forecasted_netherlands_household <- forecast(netherlands_arima_model_household, h = 5)

# GDP Deflator
netherlands_gdp_deflator_ts <- ts(
  netherlands_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(netherlands_data$year), "%Y")),
  frequency = 1
)

netherlands_arima_model_deflator <- auto.arima(netherlands_gdp_deflator_ts)
forecasted_netherlands_deflator <- forecast(netherlands_arima_model_deflator, h = 5)

# Unemployment
netherlands_unemployment_ts <- ts(
  netherlands_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(netherlands_data$year), "%Y")),
  frequency = 1
)

netherlands_arima_model_unemployment <- auto.arima(netherlands_unemployment_ts)
forecasted_netherlands_unemployment <- forecast(netherlands_arima_model_unemployment, h = 5)

# Gini Index
netherlands_gini_ts <- ts(
  netherlands_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(netherlands_data$year), "%Y")),
  frequency = 1
)

netherlands_arima_model_gini <- auto.arima(netherlands_gini_ts)
forecasted_netherlands_gini <- forecast(netherlands_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_netherlands_gdp_growth <- as.numeric(forecasted_netherlands_gdp_growth$mean)
future_netherlands_household <- as.numeric(forecasted_netherlands_household$mean)
future_netherlands_gdp_deflator <- as.numeric(forecasted_netherlands_deflator$mean)
future_netherlands_unemployment <- as.numeric(forecasted_netherlands_unemployment$mean)
future_netherlands_gini <- as.numeric(forecasted_netherlands_gini$mean)

# Combine historical data and forecasted predictors 
historical_netherlands_predictors <- data.frame(
  gdp_growth = as.numeric(netherlands_gdp_growth_ts),
  household = as.numeric(netherlands_household_ts),
  gdp_deflator = as.numeric(netherlands_gdp_deflator_ts),
  unemployment = as.numeric(netherlands_unemployment_ts),
  gini = as.numeric(netherlands_gini_ts)
)

# Create a time series for cost_variable
netherlands_cost_variable_ts <- ts(
  netherlands_data$cost_variable,
  start = as.numeric(format(min(netherlands_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
netherlands_tslm_model <- tslm(netherlands_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_netherlands_predictors)
summary(netherlands_tslm_model)

# Create future predictors for the forecast
future_netherlands_predictors <- data.frame(
  gdp_growth = future_netherlands_gdp_growth,
  household = future_netherlands_household,
  gdp_deflator = future_netherlands_gdp_deflator,
  unemployment = future_netherlands_unemployment,
  gini = future_netherlands_gini
)

# Forecast cost_variable 
forecasted_netherlands_cost_variable <- forecast(netherlands_tslm_model, newdata = future_netherlands_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_netherlands_cost_variable,
  main = "Forecasted Cost Variable for Netherlands",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_netherlands_gdp_growth <- c(as.numeric(netherlands_gdp_growth_ts), future_netherlands_gdp_growth)
all_netherlands_household <- c(as.numeric(netherlands_household_ts), future_netherlands_household)
all_netherlands_gdp_deflator <- c(as.numeric(netherlands_gdp_deflator_ts), future_netherlands_gdp_deflator)
all_netherlands_unemployment <- c(as.numeric(netherlands_unemployment_ts), future_netherlands_unemployment)
all_netherlands_gini <- c(as.numeric(netherlands_gini_ts), future_netherlands_gini)

# Combine historical and forecasted cost_variable
forecasted_netherlands_cost_variable_values <- as.numeric(forecasted_netherlands_cost_variable$mean)
all_netherlands_cost_variable <- c(as.numeric(netherlands_cost_variable_ts), forecasted_netherlands_cost_variable_values)

# Create year column
netherlands_years <- seq(from = as.numeric(format(min(netherlands_data$year), "%Y")),
                         to = as.numeric(format(min(netherlands_data$year), "%Y")) + length(all_netherlands_gdp_growth) - 1)

# Get the historical cost variable
historical_netherlands_cost_variable <- as.numeric(netherlands_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_netherlands_cost_variable_lo80 <- c(rep(NA, length(historical_netherlands_cost_variable)),
                                               forecasted_netherlands_cost_variable$lower[, 1])
forecasted_netherlands_cost_variable_hi80 <- c(rep(NA, length(historical_netherlands_cost_variable)),
                                               forecasted_netherlands_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
netherlands_data_updated <- data.frame(
  year = netherlands_years,
  gdp_growth = all_netherlands_gdp_growth,
  household = all_netherlands_household,
  gdp_deflator = all_netherlands_gdp_deflator,
  unemployment = all_netherlands_unemployment,
  gini = all_netherlands_gini,
  cost_variable = all_netherlands_cost_variable,
  cost_variable_lo80 = forecasted_netherlands_cost_variable_lo80,
  cost_variable_hi80 = forecasted_netherlands_cost_variable_hi80
)

###### Russian Federation
# Filter the data for Russian Federation
russia_data <- forecasting_data %>%
  filter(country_name == "Russian Federation")

# GDP Growth
russia_gdp_growth_ts <- ts(
  russia_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(russia_data$year), "%Y")),
  frequency = 1
)

russia_arima_model_gdp <- auto.arima(russia_gdp_growth_ts)
forecasted_russia_gdp_growth <- forecast(russia_arima_model_gdp, h = 5)

# Household Expenditure
russia_household_ts <- ts(
  russia_data$household_expenditure_per_capita,
  start = as.numeric(format(min(russia_data$year), "%Y")),
  frequency = 1
)

russia_arima_model_household <- auto.arima(russia_household_ts)
forecasted_russia_household <- forecast(russia_arima_model_household, h = 5)

# GDP Deflator
russia_gdp_deflator_ts <- ts(
  russia_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(russia_data$year), "%Y")),
  frequency = 1
)

russia_arima_model_deflator <- auto.arima(russia_gdp_deflator_ts)
forecasted_russia_deflator <- forecast(russia_arima_model_deflator, h = 5)

# Unemployment
russia_unemployment_ts <- ts(
  russia_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(russia_data$year), "%Y")),
  frequency = 1
)

russia_arima_model_unemployment <- auto.arima(russia_unemployment_ts)
forecasted_russia_unemployment <- forecast(russia_arima_model_unemployment, h = 5)

# Gini Index
russia_gini_ts <- ts(
  russia_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(russia_data$year), "%Y")),
  frequency = 1
)

russia_arima_model_gini <- auto.arima(russia_gini_ts)
forecasted_russia_gini <- forecast(russia_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_russia_gdp_growth <- as.numeric(forecasted_russia_gdp_growth$mean)
future_russia_household <- as.numeric(forecasted_russia_household$mean)
future_russia_gdp_deflator <- as.numeric(forecasted_russia_deflator$mean)
future_russia_unemployment <- as.numeric(forecasted_russia_unemployment$mean)
future_russia_gini <- as.numeric(forecasted_russia_gini$mean)

# Combine historical data and forecasted predictors 
historical_russia_predictors <- data.frame(
  gdp_growth = as.numeric(russia_gdp_growth_ts),
  household = as.numeric(russia_household_ts),
  gdp_deflator = as.numeric(russia_gdp_deflator_ts),
  unemployment = as.numeric(russia_unemployment_ts),
  gini = as.numeric(russia_gini_ts)
)

# Create a time series for cost_variable
russia_cost_variable_ts <- ts(
  russia_data$cost_variable,
  start = as.numeric(format(min(russia_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
russia_tslm_model <- tslm(russia_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_russia_predictors)
summary(russia_tslm_model)

# Create future predictors for the forecast
future_russia_predictors <- data.frame(
  gdp_growth = future_russia_gdp_growth,
  household = future_russia_household,
  gdp_deflator = future_russia_gdp_deflator,
  unemployment = future_russia_unemployment,
  gini = future_russia_gini
)

# Forecast cost_variable 
forecasted_russia_cost_variable <- forecast(russia_tslm_model, newdata = future_russia_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_russia_cost_variable,
  main = "Forecasted Cost Variable for Russian Federation",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_russia_gdp_growth <- c(as.numeric(russia_gdp_growth_ts), future_russia_gdp_growth)
all_russia_household <- c(as.numeric(russia_household_ts), future_russia_household)
all_russia_gdp_deflator <- c(as.numeric(russia_gdp_deflator_ts), future_russia_gdp_deflator)
all_russia_unemployment <- c(as.numeric(russia_unemployment_ts), future_russia_unemployment)
all_russia_gini <- c(as.numeric(russia_gini_ts), future_russia_gini)

# Combine historical and forecasted cost_variable
forecasted_russia_cost_variable_values <- as.numeric(forecasted_russia_cost_variable$mean)
all_russia_cost_variable <- c(as.numeric(russia_cost_variable_ts), forecasted_russia_cost_variable_values)

# Create year column
russia_years <- seq(from = as.numeric(format(min(russia_data$year), "%Y")),
                    to = as.numeric(format(min(russia_data$year), "%Y")) + length(all_russia_gdp_growth) - 1)

# Get the historical cost variable
historical_russia_cost_variable <- as.numeric(russia_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_russia_cost_variable_lo80 <- c(rep(NA, length(historical_russia_cost_variable)),
                                          forecasted_russia_cost_variable$lower[, 1])
forecasted_russia_cost_variable_hi80 <- c(rep(NA, length(historical_russia_cost_variable)),
                                          forecasted_russia_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
russia_data_updated <- data.frame(
  year = russia_years,
  gdp_growth = all_russia_gdp_growth,
  household = all_russia_household,
  gdp_deflator = all_russia_gdp_deflator,
  unemployment = all_russia_unemployment,
  gini = all_russia_gini,
  cost_variable = all_russia_cost_variable,
  cost_variable_lo80 = forecasted_russia_cost_variable_lo80,
  cost_variable_hi80 = forecasted_russia_cost_variable_hi80
)

###### Saudi Arabia
# Filter the data for Saudi Arabia
saudi_data <- forecasting_data %>%
  filter(country_name == "Saudi Arabia")

# GDP Growth
saudi_gdp_growth_ts <- ts(
  saudi_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(saudi_data$year), "%Y")),
  frequency = 1
)

saudi_arima_model_gdp <- auto.arima(saudi_gdp_growth_ts)
forecasted_saudi_gdp_growth <- forecast(saudi_arima_model_gdp, h = 5)

# Household Expenditure
saudi_household_ts <- ts(
  saudi_data$household_expenditure_per_capita,
  start = as.numeric(format(min(saudi_data$year), "%Y")),
  frequency = 1
)

saudi_arima_model_household <- auto.arima(saudi_household_ts)
forecasted_saudi_household <- forecast(saudi_arima_model_household, h = 5)

# GDP Deflator
saudi_gdp_deflator_ts <- ts(
  saudi_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(saudi_data$year), "%Y")),
  frequency = 1
)

saudi_arima_model_deflator <- auto.arima(saudi_gdp_deflator_ts)
forecasted_saudi_deflator <- forecast(saudi_arima_model_deflator, h = 5)

# Unemployment
saudi_unemployment_ts <- ts(
  saudi_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(saudi_data$year), "%Y")),
  frequency = 1
)

saudi_arima_model_unemployment <- auto.arima(saudi_unemployment_ts)
forecasted_saudi_unemployment <- forecast(saudi_arima_model_unemployment, h = 5)

# Gini Index
saudi_gini_ts <- ts(
  saudi_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(saudi_data$year), "%Y")),
  frequency = 1
)

saudi_arima_model_gini <- auto.arima(saudi_gini_ts)
forecasted_saudi_gini <- forecast(saudi_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_saudi_gdp_growth <- as.numeric(forecasted_saudi_gdp_growth$mean)
future_saudi_household <- as.numeric(forecasted_saudi_household$mean)
future_saudi_gdp_deflator <- as.numeric(forecasted_saudi_deflator$mean)
future_saudi_unemployment <- as.numeric(forecasted_saudi_unemployment$mean)
future_saudi_gini <- as.numeric(forecasted_saudi_gini$mean)

# Combine historical data and forecasted predictors 
historical_saudi_predictors <- data.frame(
  gdp_growth = as.numeric(saudi_gdp_growth_ts),
  household = as.numeric(saudi_household_ts),
  gdp_deflator = as.numeric(saudi_gdp_deflator_ts),
  unemployment = as.numeric(saudi_unemployment_ts),
  gini = as.numeric(saudi_gini_ts)
)

# Create a time series for cost_variable
saudi_cost_variable_ts <- ts(
  saudi_data$cost_variable,
  start = as.numeric(format(min(saudi_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
saudi_tslm_model <- tslm(saudi_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_saudi_predictors)
summary(saudi_tslm_model)

# Create future predictors for the forecast
future_saudi_predictors <- data.frame(
  gdp_growth = future_saudi_gdp_growth,
  household = future_saudi_household,
  gdp_deflator = future_saudi_gdp_deflator,
  unemployment = future_saudi_unemployment,
  gini = future_saudi_gini
)

# Forecast cost_variable 
forecasted_saudi_cost_variable <- forecast(saudi_tslm_model, newdata = future_saudi_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_saudi_cost_variable,
  main = "Forecasted Cost Variable for Saudi Arabia",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_saudi_gdp_growth <- c(as.numeric(saudi_gdp_growth_ts), future_saudi_gdp_growth)
all_saudi_household <- c(as.numeric(saudi_household_ts), future_saudi_household)
all_saudi_gdp_deflator <- c(as.numeric(saudi_gdp_deflator_ts), future_saudi_gdp_deflator)
all_saudi_unemployment <- c(as.numeric(saudi_unemployment_ts), future_saudi_unemployment)
all_saudi_gini <- c(as.numeric(saudi_gini_ts), future_saudi_gini)

# Combine historical and forecasted cost_variable
forecasted_saudi_cost_variable_values <- as.numeric(forecasted_saudi_cost_variable$mean)
all_saudi_cost_variable <- c(as.numeric(saudi_cost_variable_ts), forecasted_saudi_cost_variable_values)

# Create year column
saudi_years <- seq(from = as.numeric(format(min(saudi_data$year), "%Y")),
                   to = as.numeric(format(min(saudi_data$year), "%Y")) + length(all_saudi_gdp_growth) - 1)

# Get the historical cost variable
historical_saudi_cost_variable <- as.numeric(saudi_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_saudi_cost_variable_lo80 <- c(rep(NA, length(historical_saudi_cost_variable)),
                                         forecasted_saudi_cost_variable$lower[, 1])
forecasted_saudi_cost_variable_hi80 <- c(rep(NA, length(historical_saudi_cost_variable)),
                                         forecasted_saudi_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
saudi_data_updated <- data.frame(
  year = saudi_years,
  gdp_growth = all_saudi_gdp_growth,
  household = all_saudi_household,
  gdp_deflator = all_saudi_gdp_deflator,
  unemployment = all_saudi_unemployment,
  gini = all_saudi_gini,
  cost_variable = all_saudi_cost_variable,
  cost_variable_lo80 = forecasted_saudi_cost_variable_lo80,
  cost_variable_hi80 = forecasted_saudi_cost_variable_hi80
)

###### Spain
# Filter the data for Spain
spain_data <- forecasting_data %>%
  filter(country_name == "Spain")

# GDP Growth
spain_gdp_growth_ts <- ts(
  spain_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(spain_data$year), "%Y")),
  frequency = 1
)

spain_arima_model_gdp <- auto.arima(spain_gdp_growth_ts)
forecasted_spain_gdp_growth <- forecast(spain_arima_model_gdp, h = 5)

# Household Expenditure
spain_household_ts <- ts(
  spain_data$household_expenditure_per_capita,
  start = as.numeric(format(min(spain_data$year), "%Y")),
  frequency = 1
)

spain_arima_model_household <- auto.arima(spain_household_ts)
forecasted_spain_household <- forecast(spain_arima_model_household, h = 5)

# GDP Deflator
spain_gdp_deflator_ts <- ts(
  spain_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(spain_data$year), "%Y")),
  frequency = 1
)

spain_arima_model_deflator <- auto.arima(spain_gdp_deflator_ts)
forecasted_spain_deflator <- forecast(spain_arima_model_deflator, h = 5)

# Unemployment
spain_unemployment_ts <- ts(
  spain_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(spain_data$year), "%Y")),
  frequency = 1
)

spain_arima_model_unemployment <- auto.arima(spain_unemployment_ts)
forecasted_spain_unemployment <- forecast(spain_arima_model_unemployment, h = 5)

# Gini Index
spain_gini_ts <- ts(
  spain_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(spain_data$year), "%Y")),
  frequency = 1
)

spain_arima_model_gini <- auto.arima(spain_gini_ts)
forecasted_spain_gini <- forecast(spain_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_spain_gdp_growth <- as.numeric(forecasted_spain_gdp_growth$mean)
future_spain_household <- as.numeric(forecasted_spain_household$mean)
future_spain_gdp_deflator <- as.numeric(forecasted_spain_deflator$mean)
future_spain_unemployment <- as.numeric(forecasted_spain_unemployment$mean)
future_spain_gini <- as.numeric(forecasted_spain_gini$mean)

# Combine historical data and forecasted predictors 
historical_spain_predictors <- data.frame(
  gdp_growth = as.numeric(spain_gdp_growth_ts),
  household = as.numeric(spain_household_ts),
  gdp_deflator = as.numeric(spain_gdp_deflator_ts),
  unemployment = as.numeric(spain_unemployment_ts),
  gini = as.numeric(spain_gini_ts)
)

# Create a time series for cost_variable
spain_cost_variable_ts <- ts(
  spain_data$cost_variable,
  start = as.numeric(format(min(spain_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
spain_tslm_model <- tslm(spain_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_spain_predictors)
summary(spain_tslm_model)

# Create future predictors for the forecast
future_spain_predictors <- data.frame(
  gdp_growth = future_spain_gdp_growth,
  household = future_spain_household,
  gdp_deflator = future_spain_gdp_deflator,
  unemployment = future_spain_unemployment,
  gini = future_spain_gini
)

# Forecast cost_variable 
forecasted_spain_cost_variable <- forecast(spain_tslm_model, newdata = future_spain_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_spain_cost_variable,
  main = "Forecasted Cost Variable for Spain",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_spain_gdp_growth <- c(as.numeric(spain_gdp_growth_ts), future_spain_gdp_growth)
all_spain_household <- c(as.numeric(spain_household_ts), future_spain_household)
all_spain_gdp_deflator <- c(as.numeric(spain_gdp_deflator_ts), future_spain_gdp_deflator)
all_spain_unemployment <- c(as.numeric(spain_unemployment_ts), future_spain_unemployment)
all_spain_gini <- c(as.numeric(spain_gini_ts), future_spain_gini)

# Combine historical and forecasted cost_variable
forecasted_spain_cost_variable_values <- as.numeric(forecasted_spain_cost_variable$mean)
all_spain_cost_variable <- c(as.numeric(spain_cost_variable_ts), forecasted_spain_cost_variable_values)

# Create year column
spain_years <- seq(from = as.numeric(format(min(spain_data$year), "%Y")),
                   to = as.numeric(format(min(spain_data$year), "%Y")) + length(all_spain_gdp_growth) - 1)

# Get the historical cost variable
historical_spain_cost_variable <- as.numeric(spain_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_spain_cost_variable_lo80 <- c(rep(NA, length(historical_spain_cost_variable)),
                                         forecasted_spain_cost_variable$lower[, 1])
forecasted_spain_cost_variable_hi80 <- c(rep(NA, length(historical_spain_cost_variable)),
                                         forecasted_spain_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
spain_data_updated <- data.frame(
  year = spain_years,
  gdp_growth = all_spain_gdp_growth,
  household = all_spain_household,
  gdp_deflator = all_spain_gdp_deflator,
  unemployment = all_spain_unemployment,
  gini = all_spain_gini,
  cost_variable = all_spain_cost_variable,
  cost_variable_lo80 = forecasted_spain_cost_variable_lo80,
  cost_variable_hi80 = forecasted_spain_cost_variable_hi80
)

###### Switzerland
# Filter the data for Switzerland
switzerland_data <- forecasting_data %>%
  filter(country_name == "Switzerland")

# GDP Growth
switzerland_gdp_growth_ts <- ts(
  switzerland_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(switzerland_data$year), "%Y")),
  frequency = 1
)

switzerland_arima_model_gdp <- auto.arima(switzerland_gdp_growth_ts)
forecasted_switzerland_gdp_growth <- forecast(switzerland_arima_model_gdp, h = 5)

# Household Expenditure
switzerland_household_ts <- ts(
  switzerland_data$household_expenditure_per_capita,
  start = as.numeric(format(min(switzerland_data$year), "%Y")),
  frequency = 1
)

switzerland_arima_model_household <- auto.arima(switzerland_household_ts)
forecasted_switzerland_household <- forecast(switzerland_arima_model_household, h = 5)

# GDP Deflator
switzerland_gdp_deflator_ts <- ts(
  switzerland_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(switzerland_data$year), "%Y")),
  frequency = 1
)

switzerland_arima_model_deflator <- auto.arima(switzerland_gdp_deflator_ts)
forecasted_switzerland_deflator <- forecast(switzerland_arima_model_deflator, h = 5)

# Unemployment
switzerland_unemployment_ts <- ts(
  switzerland_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(switzerland_data$year), "%Y")),
  frequency = 1
)

switzerland_arima_model_unemployment <- auto.arima(switzerland_unemployment_ts)
forecasted_switzerland_unemployment <- forecast(switzerland_arima_model_unemployment, h = 5)

# Gini Index
switzerland_gini_ts <- ts(
  switzerland_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(switzerland_data$year), "%Y")),
  frequency = 1
)

switzerland_arima_model_gini <- auto.arima(switzerland_gini_ts)
forecasted_switzerland_gini <- forecast(switzerland_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_switzerland_gdp_growth <- as.numeric(forecasted_switzerland_gdp_growth$mean)
future_switzerland_household <- as.numeric(forecasted_switzerland_household$mean)
future_switzerland_gdp_deflator <- as.numeric(forecasted_switzerland_deflator$mean)
future_switzerland_unemployment <- as.numeric(forecasted_switzerland_unemployment$mean)
future_switzerland_gini <- as.numeric(forecasted_switzerland_gini$mean)

# Combine historical data and forecasted predictors 
historical_switzerland_predictors <- data.frame(
  gdp_growth = as.numeric(switzerland_gdp_growth_ts),
  household = as.numeric(switzerland_household_ts),
  gdp_deflator = as.numeric(switzerland_gdp_deflator_ts),
  unemployment = as.numeric(switzerland_unemployment_ts),
  gini = as.numeric(switzerland_gini_ts)
)

# Create a time series for cost_variable
switzerland_cost_variable_ts <- ts(
  switzerland_data$cost_variable,
  start = as.numeric(format(min(switzerland_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
switzerland_tslm_model <- tslm(switzerland_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_switzerland_predictors)
summary(switzerland_tslm_model)

# Create future predictors for the forecast
future_switzerland_predictors <- data.frame(
  gdp_growth = future_switzerland_gdp_growth,
  household = future_switzerland_household,
  gdp_deflator = future_switzerland_gdp_deflator,
  unemployment = future_switzerland_unemployment,
  gini = future_switzerland_gini
)

# Forecast cost_variable 
forecasted_switzerland_cost_variable <- forecast(switzerland_tslm_model, newdata = future_switzerland_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_switzerland_cost_variable,
  main = "Forecasted Cost Variable for Switzerland",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_switzerland_gdp_growth <- c(as.numeric(switzerland_gdp_growth_ts), future_switzerland_gdp_growth)
all_switzerland_household <- c(as.numeric(switzerland_household_ts), future_switzerland_household)
all_switzerland_gdp_deflator <- c(as.numeric(switzerland_gdp_deflator_ts), future_switzerland_gdp_deflator)
all_switzerland_unemployment <- c(as.numeric(switzerland_unemployment_ts), future_switzerland_unemployment)
all_switzerland_gini <- c(as.numeric(switzerland_gini_ts), future_switzerland_gini)

# Combine historical and forecasted cost_variable
forecasted_switzerland_cost_variable_values <- as.numeric(forecasted_switzerland_cost_variable$mean)
all_switzerland_cost_variable <- c(as.numeric(switzerland_cost_variable_ts), forecasted_switzerland_cost_variable_values)

# Create year column
switzerland_years <- seq(from = as.numeric(format(min(switzerland_data$year), "%Y")),
                         to = as.numeric(format(min(switzerland_data$year), "%Y")) + length(all_switzerland_gdp_growth) - 1)

# Get the historical cost variable
historical_switzerland_cost_variable <- as.numeric(switzerland_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_switzerland_cost_variable_lo80 <- c(rep(NA, length(historical_switzerland_cost_variable)),
                                               forecasted_switzerland_cost_variable$lower[, 1])
forecasted_switzerland_cost_variable_hi80 <- c(rep(NA, length(historical_switzerland_cost_variable)),
                                               forecasted_switzerland_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
switzerland_data_updated <- data.frame(
  year = switzerland_years,
  gdp_growth = all_switzerland_gdp_growth,
  household = all_switzerland_household,
  gdp_deflator = all_switzerland_gdp_deflator,
  unemployment = all_switzerland_unemployment,
  gini = all_switzerland_gini,
  cost_variable = all_switzerland_cost_variable,
  cost_variable_lo80 = forecasted_switzerland_cost_variable_lo80,
  cost_variable_hi80 = forecasted_switzerland_cost_variable_hi80
)

###### Turkiye
# Filter the data for Turkiye
turkiye_data <- forecasting_data %>%
  filter(country_name == "Turkiye")

# GDP Growth
turkiye_gdp_growth_ts <- ts(
  turkiye_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(turkiye_data$year), "%Y")),
  frequency = 1
)

turkiye_arima_model_gdp <- auto.arima(turkiye_gdp_growth_ts)
forecasted_turkiye_gdp_growth <- forecast(turkiye_arima_model_gdp, h = 5)

# Household Expenditure
turkiye_household_ts <- ts(
  turkiye_data$household_expenditure_per_capita,
  start = as.numeric(format(min(turkiye_data$year), "%Y")),
  frequency = 1
)

turkiye_arima_model_household <- auto.arima(turkiye_household_ts)
forecasted_turkiye_household <- forecast(turkiye_arima_model_household, h = 5)

# GDP Deflator
turkiye_gdp_deflator_ts <- ts(
  turkiye_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(turkiye_data$year), "%Y")),
  frequency = 1
)

turkiye_arima_model_deflator <- auto.arima(turkiye_gdp_deflator_ts)
forecasted_turkiye_deflator <- forecast(turkiye_arima_model_deflator, h = 5)

# Unemployment
turkiye_unemployment_ts <- ts(
  turkiye_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(turkiye_data$year), "%Y")),
  frequency = 1
)

turkiye_arima_model_unemployment <- auto.arima(turkiye_unemployment_ts)
forecasted_turkiye_unemployment <- forecast(turkiye_arima_model_unemployment, h = 5)

# Gini Index
turkiye_gini_ts <- ts(
  turkiye_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(turkiye_data$year), "%Y")),
  frequency = 1
)

turkiye_arima_model_gini <- auto.arima(turkiye_gini_ts)
forecasted_turkiye_gini <- forecast(turkiye_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_turkiye_gdp_growth <- as.numeric(forecasted_turkiye_gdp_growth$mean)
future_turkiye_household <- as.numeric(forecasted_turkiye_household$mean)
future_turkiye_gdp_deflator <- as.numeric(forecasted_turkiye_deflator$mean)
future_turkiye_unemployment <- as.numeric(forecasted_turkiye_unemployment$mean)
future_turkiye_gini <- as.numeric(forecasted_turkiye_gini$mean)

# Combine historical data and forecasted predictors 
historical_turkiye_predictors <- data.frame(
  gdp_growth = as.numeric(turkiye_gdp_growth_ts),
  household = as.numeric(turkiye_household_ts),
  gdp_deflator = as.numeric(turkiye_gdp_deflator_ts),
  unemployment = as.numeric(turkiye_unemployment_ts),
  gini = as.numeric(turkiye_gini_ts)
)

# Create a time series for cost_variable
turkiye_cost_variable_ts <- ts(
  turkiye_data$cost_variable,
  start = as.numeric(format(min(turkiye_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
turkiye_tslm_model <- tslm(turkiye_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_turkiye_predictors)
summary(turkiye_tslm_model)

# Create future predictors for the forecast
future_turkiye_predictors <- data.frame(
  gdp_growth = future_turkiye_gdp_growth,
  household = future_turkiye_household,
  gdp_deflator = future_turkiye_gdp_deflator,
  unemployment = future_turkiye_unemployment,
  gini = future_turkiye_gini
)

# Forecast cost_variable 
forecasted_turkiye_cost_variable <- forecast(turkiye_tslm_model, newdata = future_turkiye_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_turkiye_cost_variable,
  main = "Forecasted Cost Variable for Turkiye",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_turkiye_gdp_growth <- c(as.numeric(turkiye_gdp_growth_ts), future_turkiye_gdp_growth)
all_turkiye_household <- c(as.numeric(turkiye_household_ts), future_turkiye_household)
all_turkiye_gdp_deflator <- c(as.numeric(turkiye_gdp_deflator_ts), future_turkiye_gdp_deflator)
all_turkiye_unemployment <- c(as.numeric(turkiye_unemployment_ts), future_turkiye_unemployment)
all_turkiye_gini <- c(as.numeric(turkiye_gini_ts), future_turkiye_gini)

# Combine historical and forecasted cost_variable
forecasted_turkiye_cost_variable_values <- as.numeric(forecasted_turkiye_cost_variable$mean)
all_turkiye_cost_variable <- c(as.numeric(turkiye_cost_variable_ts), forecasted_turkiye_cost_variable_values)

# Create year column
turkiye_years <- seq(from = as.numeric(format(min(turkiye_data$year), "%Y")),
                     to = as.numeric(format(min(turkiye_data$year), "%Y")) + length(all_turkiye_gdp_growth) - 1)

# Get the historical cost variable
historical_turkiye_cost_variable <- as.numeric(turkiye_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_turkiye_cost_variable_lo80 <- c(rep(NA, length(historical_turkiye_cost_variable)),
                                           forecasted_turkiye_cost_variable$lower[, 1])
forecasted_turkiye_cost_variable_hi80 <- c(rep(NA, length(historical_turkiye_cost_variable)),
                                           forecasted_turkiye_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
turkiye_data_updated <- data.frame(
  year = turkiye_years,
  gdp_growth = all_turkiye_gdp_growth,
  household = all_turkiye_household,
  gdp_deflator = all_turkiye_gdp_deflator,
  unemployment = all_turkiye_unemployment,
  gini = all_turkiye_gini,
  cost_variable = all_turkiye_cost_variable,
  cost_variable_lo80 = forecasted_turkiye_cost_variable_lo80,
  cost_variable_hi80 = forecasted_turkiye_cost_variable_hi80
)

###### United Kingdom
# Filter the data for United Kingdom
uk_data <- forecasting_data %>%
  filter(country_name == "United Kingdom")

# GDP Growth
uk_gdp_growth_ts <- ts(
  uk_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(uk_data$year), "%Y")),
  frequency = 1
)

uk_arima_model_gdp <- auto.arima(uk_gdp_growth_ts)
forecasted_uk_gdp_growth <- forecast(uk_arima_model_gdp, h = 5)

# Household Expenditure
uk_household_ts <- ts(
  uk_data$household_expenditure_per_capita,
  start = as.numeric(format(min(uk_data$year), "%Y")),
  frequency = 1
)

uk_arima_model_household <- auto.arima(uk_household_ts)
forecasted_uk_household <- forecast(uk_arima_model_household, h = 5)

# GDP Deflator
uk_gdp_deflator_ts <- ts(
  uk_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(uk_data$year), "%Y")),
  frequency = 1
)

uk_arima_model_deflator <- auto.arima(uk_gdp_deflator_ts)
forecasted_uk_deflator <- forecast(uk_arima_model_deflator, h = 5)

# Unemployment
uk_unemployment_ts <- ts(
  uk_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(uk_data$year), "%Y")),
  frequency = 1
)

uk_arima_model_unemployment <- auto.arima(uk_unemployment_ts)
forecasted_uk_unemployment <- forecast(uk_arima_model_unemployment, h = 5)

# Gini Index
uk_gini_ts <- ts(
  uk_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(uk_data$year), "%Y")),
  frequency = 1
)

uk_arima_model_gini <- auto.arima(uk_gini_ts)
forecasted_uk_gini <- forecast(uk_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_uk_gdp_growth <- as.numeric(forecasted_uk_gdp_growth$mean)
future_uk_household <- as.numeric(forecasted_uk_household$mean)
future_uk_gdp_deflator <- as.numeric(forecasted_uk_deflator$mean)
future_uk_unemployment <- as.numeric(forecasted_uk_unemployment$mean)
future_uk_gini <- as.numeric(forecasted_uk_gini$mean)

# Combine historical data and forecasted predictors 
historical_uk_predictors <- data.frame(
  gdp_growth = as.numeric(uk_gdp_growth_ts),
  household = as.numeric(uk_household_ts),
  gdp_deflator = as.numeric(uk_gdp_deflator_ts),
  unemployment = as.numeric(uk_unemployment_ts),
  gini = as.numeric(uk_gini_ts)
)

# Create a time series for cost_variable
uk_cost_variable_ts <- ts(
  uk_data$cost_variable,
  start = as.numeric(format(min(uk_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
uk_tslm_model <- tslm(uk_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_uk_predictors)
summary(uk_tslm_model)

# Create future predictors for the forecast
future_uk_predictors <- data.frame(
  gdp_growth = future_uk_gdp_growth,
  household = future_uk_household,
  gdp_deflator = future_uk_gdp_deflator,
  unemployment = future_uk_unemployment,
  gini = future_uk_gini
)

# Forecast cost_variable 
forecasted_uk_cost_variable <- forecast(uk_tslm_model, newdata = future_uk_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_uk_cost_variable,
  main = "Forecasted Cost Variable for United Kingdom",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_uk_gdp_growth <- c(as.numeric(uk_gdp_growth_ts), future_uk_gdp_growth)
all_uk_household <- c(as.numeric(uk_household_ts), future_uk_household)
all_uk_gdp_deflator <- c(as.numeric(uk_gdp_deflator_ts), future_uk_gdp_deflator)
all_uk_unemployment <- c(as.numeric(uk_unemployment_ts), future_uk_unemployment)
all_uk_gini <- c(as.numeric(uk_gini_ts), future_uk_gini)

# Combine historical and forecasted cost_variable
forecasted_uk_cost_variable_values <- as.numeric(forecasted_uk_cost_variable$mean)
all_uk_cost_variable <- c(as.numeric(uk_cost_variable_ts), forecasted_uk_cost_variable_values)

# Create year column
uk_years <- seq(from = as.numeric(format(min(uk_data$year), "%Y")),
                to = as.numeric(format(min(uk_data$year), "%Y")) + length(all_uk_gdp_growth) - 1)

# Get the historical cost variable
historical_uk_cost_variable <- as.numeric(uk_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_uk_cost_variable_lo80 <- c(rep(NA, length(historical_uk_cost_variable)),
                                      forecasted_uk_cost_variable$lower[, 1])
forecasted_uk_cost_variable_hi80 <- c(rep(NA, length(historical_uk_cost_variable)),
                                      forecasted_uk_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
uk_data_updated <- data.frame(
  year = uk_years,
  gdp_growth = all_uk_gdp_growth,
  household = all_uk_household,
  gdp_deflator = all_uk_gdp_deflator,
  unemployment = all_uk_unemployment,
  gini = all_uk_gini,
  cost_variable = all_uk_cost_variable,
  cost_variable_lo80 = forecasted_uk_cost_variable_lo80,
  cost_variable_hi80 = forecasted_uk_cost_variable_hi80
)

###### United States
# Filter the data for United States
us_data <- forecasting_data %>%
  filter(country_name == "United States")

# GDP Growth
us_gdp_growth_ts <- ts(
  us_data$gdp_percapita_growth_NY.GDP.PCAP.KD.ZG,
  start = as.numeric(format(min(us_data$year), "%Y")),
  frequency = 1
)

us_arima_model_gdp <- auto.arima(us_gdp_growth_ts)
forecasted_us_gdp_growth <- forecast(us_arima_model_gdp, h = 5)

# Household Expenditure
us_household_ts <- ts(
  us_data$household_expenditure_per_capita,
  start = as.numeric(format(min(us_data$year), "%Y")),
  frequency = 1
)

us_arima_model_household <- auto.arima(us_household_ts)
forecasted_us_household <- forecast(us_arima_model_household, h = 5)

# GDP Deflator
us_gdp_deflator_ts <- ts(
  us_data$gdp_deflator_NY.GDP.DEFL.ZS,
  start = as.numeric(format(min(us_data$year), "%Y")),
  frequency = 1
)

us_arima_model_deflator <- auto.arima(us_gdp_deflator_ts)
forecasted_us_deflator <- forecast(us_arima_model_deflator, h = 5)

# Unemployment
us_unemployment_ts <- ts(
  us_data$unemployment_SL.UEM.TOTL.NE.ZS,
  start = as.numeric(format(min(us_data$year), "%Y")),
  frequency = 1
)

us_arima_model_unemployment <- auto.arima(us_unemployment_ts)
forecasted_us_unemployment <- forecast(us_arima_model_unemployment, h = 5)

# Gini Index
us_gini_ts <- ts(
  us_data$gini_SI.POV.GINI,
  start = as.numeric(format(min(us_data$year), "%Y")),
  frequency = 1
)

us_arima_model_gini <- auto.arima(us_gini_ts)
forecasted_us_gini <- forecast(us_arima_model_gini, h = 5)

# Extract forecasted predictor values
future_us_gdp_growth <- as.numeric(forecasted_us_gdp_growth$mean)
future_us_household <- as.numeric(forecasted_us_household$mean)
future_us_gdp_deflator <- as.numeric(forecasted_us_deflator$mean)
future_us_unemployment <- as.numeric(forecasted_us_unemployment$mean)
future_us_gini <- as.numeric(forecasted_us_gini$mean)

# Combine historical data and forecasted predictors 
historical_us_predictors <- data.frame(
  gdp_growth = as.numeric(us_gdp_growth_ts),
  household = as.numeric(us_household_ts),
  gdp_deflator = as.numeric(us_gdp_deflator_ts),
  unemployment = as.numeric(us_unemployment_ts),
  gini = as.numeric(us_gini_ts)
)

# Create a time series for cost_variable
us_cost_variable_ts <- ts(
  us_data$cost_variable,
  start = as.numeric(format(min(us_data$year), "%Y")),
  frequency = 1
)

# Fit the tlsm regression
us_tslm_model <- tslm(us_cost_variable_ts ~ gdp_growth + household + gdp_deflator + unemployment + gini, data = historical_us_predictors)
summary(us_tslm_model)

# Create future predictors for the forecast
future_us_predictors <- data.frame(
  gdp_growth = future_us_gdp_growth,
  household = future_us_household,
  gdp_deflator = future_us_gdp_deflator,
  unemployment = future_us_unemployment,
  gini = future_us_gini
)

# Forecast cost_variable 
forecasted_us_cost_variable <- forecast(us_tslm_model, newdata = future_us_predictors)

# Plot the forecasted cost variable
plot(
  forecasted_us_cost_variable,
  main = "Forecasted Cost Variable for United States",
  ylab = "Cost Variable",
  xlab = "Year"
)

# Combine historical and forecasted predictors
all_us_gdp_growth <- c(as.numeric(us_gdp_growth_ts), future_us_gdp_growth)
all_us_household <- c(as.numeric(us_household_ts), future_us_household)
all_us_gdp_deflator <- c(as.numeric(us_gdp_deflator_ts), future_us_gdp_deflator)
all_us_unemployment <- c(as.numeric(us_unemployment_ts), future_us_unemployment)
all_us_gini <- c(as.numeric(us_gini_ts), future_us_gini)

# Combine historical and forecasted cost_variable
forecasted_us_cost_variable_values <- as.numeric(forecasted_us_cost_variable$mean)
all_us_cost_variable <- c(as.numeric(us_cost_variable_ts), forecasted_us_cost_variable_values)

# Create year column
us_years <- seq(from = as.numeric(format(min(us_data$year), "%Y")),
                to = as.numeric(format(min(us_data$year), "%Y")) + length(all_us_gdp_growth) - 1)

# Get the historical cost variable
historical_us_cost_variable <- as.numeric(us_cost_variable_ts)

# Get 80% confidence intervals for the forecasted cost_variable
forecasted_us_cost_variable_lo80 <- c(rep(NA, length(historical_us_cost_variable)),
                                      forecasted_us_cost_variable$lower[, 1])
forecasted_us_cost_variable_hi80 <- c(rep(NA, length(historical_us_cost_variable)),
                                      forecasted_us_cost_variable$upper[, 1])


# Create a new dataframe combining historical and forecasted values
us_data_updated <- data.frame(
  year = us_years,
  gdp_growth = all_us_gdp_growth,
  household = all_us_household,
  gdp_deflator = all_us_gdp_deflator,
  unemployment = all_us_unemployment,
  gini = all_us_gini,
  cost_variable = all_us_cost_variable,
  cost_variable_lo80 = forecasted_us_cost_variable_lo80,
  cost_variable_hi80 = forecasted_us_cost_variable_hi80
)


# Combining all the historical and forecasted results for all the countries
countries_data_list <- list(
  australia = cbind(country_name = "Australia", country_code = "AUS", australia_data_updated),
  brazil = cbind(country_name = "Brazil", country_code = "BRA", brazil_data_updated),
  canada = cbind(country_name = "Canada", country_code = "CAN", canada_data_updated),
  china = cbind(country_name = "China", country_code = "CHN", china_data_updated),
  france = cbind(country_name = "France", country_code = "FRA", france_data_updated),
  germany = cbind(country_name = "Germany", country_code = "DEU", germany_data_updated),
  india = cbind(country_name = "India", country_code = "IND", india_data_updated),
  indonesia = cbind(country_name = "Indonesia", country_code = "IDN", indonesia_data_updated),
  italy = cbind(country_name = "Italy", country_code = "ITA", italy_data_updated),
  japan = cbind(country_name = "Japan", country_code = "JPN", japan_data_updated),
  korea = cbind(country_name = "Korea, Rep.", country_code = "KOR", korea_rep_data_updated),
  mexico = cbind(country_name = "Mexico", country_code = "MEX", mexico_data_updated),
  netherlands = cbind(country_name = "Netherlands", country_code = "NLD", netherlands_data_updated),
  russia = cbind(country_name = "Russian Federation", country_code = "RUS", russia_data_updated),
  saudi_arabia = cbind(country_name = "Saudi Arabia", country_code = "SAU", saudi_data_updated),
  spain = cbind(country_name = "Spain", country_code = "ESP", spain_data_updated),
  switzerland = cbind(country_name = "Switzerland", country_code = "CHE", switzerland_data_updated),
  turkiye = cbind(country_name = "Turkiye", country_code = "TUR", turkiye_data_updated),
  uk = cbind(country_name = "United Kingdom", country_code = "GBR", uk_data_updated),
  us = cbind(country_name = "United States", country_code = "USA", us_data_updated)
)

# Combine all country data frames into one
combined_data <- do.call(rbind, countries_data_list)

# Saving into a csv file
#write.csv(combined_data, file = "combined_forecasting_data.csv", row.names = FALSE)
 