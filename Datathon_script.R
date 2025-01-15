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
data < raw_data

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
  select(country_name, 
         country_code, 
         year, 
         time_code, 
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

#### Subsetting for different Countries/Regions ####

#G7
g7 <- nonmiss_data %>%
  filter(nonmiss_data$country_name == c("Canada","France", "Germany", "Italy", "Japan", "United Kingdom", "United States"))

#OECD
oecd <- nonmiss_data %>%
  filter(nonmiss_data$country_name == c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic",
                                "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                                "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands",
                                "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
                                "Switzerland", "Turkey", "United Kingdom", "United States"))


#Top 10 highest gdp
high.gdp <- nonmiss_data %>%
  filter(nonmiss_data$country_name == c("United States", "China", "Germany", "Japan", "India", "United Kingdom", "France", "Italy", 
                                "Brazil", "Canada"))

#Top 10 lowest gdp
low.gdp <- nonmiss_data %>%
  filter(nonmiss_data$country_name == c("American Samoa", "São Tomé and Principe", "Dominica", "St. Martin (French part)", "Tonga", "Micronesia, Fed. Sts.",
                                "Palau", "Kiribati", "Marshall Islands", "Nauru", "Tuvalu"))

# Global South Countries
global.south <- nonmiss_data %>%
  filter(nonmiss_data$country_name == c(
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
  ))

# Global North Countries
global.north <- nonmiss_data %>%
  filter(nonmiss_data$country_name == c(
    "Australia", "Austria", "Belgium", "Canada", "Denmark", "Estonia", "Finland", "France", "Germany",
    "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea, Republic of", "Latvia",
    "Lithuania", "Liechtenstein", "Luxembourg", "Monaco", "Netherlands", "New Zealand", "Norway", "Poland",
    "Portugal", "San Marino", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"
  ))

#### DATA IMPUTATION ####

imputed_data <- mice(
  data = nonmiss_data,
  method = 'mean',
  m = 1,           # Number of multiple imputations
  maxit = 10,      # Number of iterations
  seed = 123,      # Set seed for reproducibility
  by = nonmiss_data$country_code  # Group imputation by country
)

completed_data <- complete(imputed_data, 1)  # Replace '1' with the index of the dataset you want

#### PCA ANALYSIS ####

supply_chain_volatility <- completed_data %>%
  select(
  # LPI 
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
  current_account_USD_BN.CAB.XOKA.CD
  )

apply(supply_chain_volatility, 2, mean)
apply(supply_chain_volatility, 2, var)

pr_out <- prcomp(supply_chain_volatility, scale=TRUE)

loadings_volatility <- pr_out$rotation

pca_scores <- pr_out$x

volatility_variable <- pca_scores[, 1]

volatility_variable_weighted <- as.matrix(supply_chain_volatility) %*% loadings[, 1]

explained_variance_volatility <- (pr_out_volatility$sdev)^2
explained_variance_ratio_volatility <- explained_variance / sum(explained_variance)
explained_variance_ratio_volatility[1]

completed_data$volatility_variable <- (volatility_variable)^2

#### PCA volatility 

supply_chain_cost <- completed_data %>%
  select(
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

apply(supply_chain_cost, 2, mean)
apply(supply_chain_cost, 2, var)

pr_out_cost <- prcomp(supply_chain_cost, scale=TRUE)

loadings_cost <- pr_out_cost$rotation

pca_scores <- pr_out_cost$x

cost_variable <- pca_scores[, 1]

cost_variable_weighted <- as.matrix(supply_chain_volatility) %*% loadings[, 1]

explained_variance_cost <- (pr_out_cost$sdev)^2
explained_variance_ratio_cost <- explained_variance / sum(explained_variance)
explained_variance_ratio_cost[1]

completed_data$cost_variable <- (cost_variable)^2

test <- completed_data %>%
  select(country_name, volatility_variable, cost_variable)

  


