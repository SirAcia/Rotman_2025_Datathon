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

#### CHECK BECAUSE CORRELATION HAS BEEN UPDATED ####

#household_expenditure_GDP WITH net_trade_goods_service_BN.GSR.GNFS.CD: (-0.7357246)
#household_expenditure_GDP WITH net_trade_goods_BN.GSR.MRCH.CD: (-0.7372171)
#household_expenditure_GDP WITH current_account_USD_BN.CAB.XOKA.CD: (-0.7065403)

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


model_act <- lm(cbind(net_trade_goods_service_BN.GSR.GNFS.CD, net_trade_goods_BN.GSR.MRCH.CD, current_account_USD_BN.CAB.XOKA.CD)
                ~ consumer_price+inflation+inflation_deflator+
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
                  unemployment_youth_SL.UEM.1524.NE.ZS, data = countries)

summary(model_act)

# store predictors that were significant across all the models

# Store the models in a list
models <- list(model1 = model_cost, model2 = model_volatility, model3 = model_netservice, model4 = model_netgoods, 
               model5 = model_act)

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
#' consumer_price"                       "household_expenditure_GDP"           
#" gdp_percapita_NY.GDP.PCAP.PP.CD"      "gdp_deflator_NY.GDP.DEFL.ZS"         
# "unemployment_SL.UEM.TOTL.NE.ZS"       "unemployment_youth_SL.UEM.1524.NE.ZS"

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


