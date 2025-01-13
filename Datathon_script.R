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
raw_data <- read_excel("/Users/mausamvk/Desktop/Rotman 2025 Datathon Dataset/Datathon_data-2025-Raw.xlsx")
metadata <- read_excel("/Users/mausamvk/Desktop/Rotman 2025 Datathon Dataset/Datathon_data-2025-Raw Metadata.xlsx")
