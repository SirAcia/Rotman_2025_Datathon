#Rotman Datathon 2025
# Libraries ----------------------------------------------------------------------------------------------------------

library(tidyverse)
library(funModeling)
library(Hmisc)
library(dplyr)
library(ggpubr)
library(tidyr)
library(knitr)
library(kableExtra)
library(readxl)

# Importing and cleaning data ----------------------------------------------------------------------------------------

#read raw data and metdata files
raw_data <- read_excel("Datathon_data-2025-Raw.xlsx", sheet = 1)
series_meta <- read_excel("Datathon_data-2025-Raw.xlsx", sheet = 2)
country_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 2)
country.series_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 3)
series.time_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 4)
footnote_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 5)

data <- raw_data %>%
  rename(
    country_name = "Country Name",
    country_code = "Country Code",
    year = "Time",
    cpi_FP.CPI.TOTL = "Consumer price index (2010 = 100) [FP.CPI.TOTL]",
    net_nat_income_NY.ADJ.NNTY.CD = "Adjusted net national income (current US$) [NY.ADJ.NNTY.CD]",
    net_nat_income_percapita_NY.ADJ.NNTY.PC.CD = "Adjusted net national income per capita (current US$) [NY.ADJ.NNTY.PC.CD]",
    net_nat_income_percent_NY.ADJ.NNTY.KD.ZG = "Adjusted net national income (annual % growth) [NY.ADJ.NNTY.KD.ZG]",
    gdp_percapita_NY.GDP.PCAP.PP.CD = "GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]",
    gdp_percapita_grouwth_NY.GDP.PCAP.KD.ZG = "GDP per capita growth (annual %) [NY.GDP.PCAP.KD.ZG]",
    gdp_deflator_NY.GDP.DEFL.ZS = "GDP deflator (base year varies by country) [NY.GDP.DEFL.ZS]",
    gdp_NY.GDP.MKTP.CD = "GDP (current US$) [NY.GDP.MKTP.CD]",
    gdp_constant_NY.GDP.MKTP.KD = "GDP (constant 2015 US$) [NY.GDP.MKTP.KD]",
    business_startup_cost_IC.REG.COST.PC.ZS = "Cost of business start-up procedures (% of GNI per capita) [IC.REG.COST.PC.ZS]",
    good_service_expense_GC.XPN.GSRV.ZS = "Goods and services expense (% of expense) [GC.XPN.GSRV.ZS]",
    household_npish_expenditure_NE.CON.PRVT.KD.ZG = "Households and NPISHs Final consumption expenditure (annual % growth) [NE.CON.PRVT.KD.ZG]",
    household_npish_expenditure_percapita_NE.CON.PRVT.PC.KD.ZG = "Households and NPISHs Final consumption expenditure per capita growth (annual %) [NE.CON.PRVT.PC.KD.ZG]",
    part_time_employment_SL.TLF.PART.ZS = "Part time employment, total (% of total employment) [SL.TLF.PART.ZS]",
    final_consumption_expenditure_NE.CON.TOTL.ZS = "Final consumption expenditure (% of GDP) [NE.CON.TOTL.ZS]",
    net_primary_income_NY.GSR.NFCY.CD = "Net primary income (Net income from abroad) (current US$) [NY.GSR.NFCY.CD]",
    human_capital_index_HD.HCI.OVRL = "Human capital index (HCI) (scale 0-1) [HD.HCI.OVRL]",
    education_expenditure_SE.XPD.CTOT.ZS = "Current education expenditure, total (% of total expenditure in public institutions) [SE.XPD.CTOT.ZS]",
    life_expectancy_SP.DYN.LE00.IN = "Life expectancy at birth, total (years) [SP.DYN.LE00.IN]",
    gini_SI.POV.GINI ="Gini index [SI.POV.GINI]",
    unemployment_SL.UEM.TOTL.NE.ZS = "Unemployment, total (% of total labor force) (national estimate) [SL.UEM.TOTL.NE.ZS]",
    unemployment_youth_SL.UEM.1524.NE.ZS = "Unemployment, youth total (% of total labor force ages 15-24) (national estimate) [SL.UEM.1524.NE.ZS]",
  )
