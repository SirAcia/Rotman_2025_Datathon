# 2nd Place Winnner: Submission for the Rotman 2025 Datathon
This repository contains the R code for the data cleaning and analysis for my team's submission for the Rotman 2025 Datathon. This analysis, and subsequent visualization, placed 2nd out of 180+ teams in the competition. 

Our analysis examined a wide range of global economic indicators (GDP, GNI, HDI, trade metrics, supply chain metrics, sector-specific metrics, etc.) to identify the relationship between rising cost of living and associated supply chain volatlity & cost. Our methodology involved:  
- Conducting a review to identify variables associated with supply chain volatlity or cost
- Using principal component analysis (PCA) to create 2 weighted indexes, measuring supply chain volatility and supply chain costs
- Running multivariable regression and mixed-effect models to examine relationships between supply chain disruptions and cost of living indicators
- Utilising time-series forecasting to predict any potential supply chain disruptions

Note that our analysis focuses on the top 20 OECD nations. This choice was agreed upon as the dataset provided contained the most complete and reliable data for these countries, enabling robust and comparative insights. Moreover, these nations are among the most reliant on complex, global supply chains due to their high levels of industrialization, trade interdependence, and integration into the global economy.

Key Results:
- Supply Chain Volatility Index: Identified key drivers of volatility, including trade imbalances, raw material shortages, and geopolitical risks.
- Cost of Living Impact: Found a significant correlation between supply chain disruptions and rising costs in essential goods like food and energy.
- Forecasting Insights: Predicted potential disruptions in 2026, with actionable recommendations for mitigating their effects.
  
Repository Structure:
data_cleaning.R: Scripts for cleaning and preprocessing the raw data.
pca_analysis.R: Code for conducting principal component analysis and creating indexes.
regression_models.R: Scripts for running multivariable regression and mixed-effects models.
forecasting.R: Time-series forecasting analysis and visualization.
visualizations.R: Scripts to generate publication-quality visualizations.

Acknowledgments:
We thank the Rotman Datathon organizers for hosting this competition and providing access to the dataset. 
