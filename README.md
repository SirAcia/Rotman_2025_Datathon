# 2nd Place Winnner: Analyzing the Impact of Rising Cost of Living Relative to Economic Development and Supply Chain Dynamics

This repository contains the R code for the data cleaning and analysis for my team's submission for the Rotman 2025 Datathon. This analysis, and subsequent visualization, placed 2nd out of 180+ teams in the competition. 

Our analysis examined a wide range of global economic indicators (GDP, GNI, HDI, trade metrics, supply chain metrics, sector-specific metrics, etc.) to identify the relationship between rising cost of living and associated supply chain volatlity & cost. Our methodology involved:  
- Conducting a review to identify variables associated with supply chain volatlity or cost
- Spearman correlation matrices were used to quantify relationships between cost-of-living indicators
- Using principal component analysis (PCA) to create 2 weighted indexes, measuring supply chain volatility and supply chain costs
- Running multivariable regression and mixed-effect models to examine relationships between supply chain disruptions and cost of living indicators
- Utilising time-series forecasting to predict any potential supply chain disruptions

Note that our analysis focuses on the top 20 OECD nations. This choice was agreed upon as the dataset provided contained the most complete and reliable data for these countries, enabling robust and comparative insights. Moreover, these nations are among the most reliant on complex, global supply chains due to their high levels of industrialization, trade interdependence, and integration into the global economy.

Key Results:
- Supply Chain Volatility Index: Found a significant correlation between supply chain disruptions and rising costs in with the consumer price index, net national income, household expenditure, GDP, and uneployment metrics. 
- Cost of Living Impact: Similar results were found for the composite cost variable as for the composite volatility variable. 
- Forecasting Insights: Predicted potential disruptions in 2026 with concerns for Australia, Canada, France, Germany, Italy, Japan, Netherlands,
Switzerland, Spain, United Kingdom, United States.
  
Repository Structure:
- Datathon_Analysis.R: Full script for all analysis
- Datathon_data-2025-Raw.xlsx: Raw data file 
- Datathon_data-2025-Metadata.xlsx: Raw metadata file 

Acknowledgments:
We thank the Rotman Datathon organizers for hosting this competition and providing access to the dataset. 
