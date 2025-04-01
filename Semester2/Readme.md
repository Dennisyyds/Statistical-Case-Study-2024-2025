# Modelling Peak Electricity Demand in Great Britain

This repository contains the full code and documentation for the case study project: **Modelling Peak Electricity Demand in GB**, developed for NESO (National Electricity System Operator) as part of a statistical case study.

## Overview

The project aims to develop a linear regression model that estimates daily peak electricity demand in winter, using historical data from 1991 to 2014. The model is intended for use in NESO's security of supply analysis, particularly for:
- Understanding the relationship between temperature and electricity demand
- Estimating the effect of weather variability on annual peak demand
- Generating realistic future demand traces under different scenarios

## Repository Structure

📁 data/
├─ SCS_demand_modelling.csv         # Main dataset (daily demand and features)
├─ SCS_hourly_temp.csv              # Hourly temperature data (for temperature smoothing)

📁 plots/
├─ Rplot.png                        # Time series plot of peak demand
├─ Rplot01.png                      # Histogram of daily demand
├─ Rplot02.png                      # Demand vs temperature scatterplot
├─ Rplot03.png                      # Standard residual diagnostic plots
├─ Rplot04.png                      # Residuals vs. explanatory variables

📄 Sta Case S2P2 code.R               # Main R script for data analysis and modeling
📄 We define 19 models.docx           # Description and summary of all regression models
📄 Project_2_part_2.pdf               # Case study brief and task description
📄 README.md                          # This file

## Key Components

### 📊 Data
- **SCS_demand_modelling.csv**: Daily peak demand and predictors (temperature, wind, solar, calendar info).
- **SCS_hourly_temp.csv**: Hourly temperatures used to construct alternative temperature variables.

### 🧠 Models
19 linear regression models were developed and compared using:
- AIC/BIC scores
- Mean Absolute Error (MAE) on high-demand days
- Cross-validation by winter
- Residual diagnostics
- Bootstrapped confidence intervals

### 📈 Visualizations
Plots are provided for:
- Time series of demand
- Histogram of daily peaks
- Temperature vs demand relationship
- Residual diagnostics

### ✅ Results
The final recommended model uses:
- Effective Temperature (TE)
- Categorical day-of-week and start_year effects
- Seasonal trend via DSN (Days Since November 1)

It demonstrated strong fit on historical data and reliable performance on peak demand forecasting.

## How to Use

1. Clone the repo:
   ```bash
   git clone https://github.com/your-username/peak-demand-modeling.git

	2.	Open the R script:
	•	Sta Case S2P2 code.R contains all the steps for data cleaning, EDA, model fitting, evaluation, and plotting.
	3.	Required R packages:
	•	tidyverse, broom, caret, boot, lubridate
	4.	Run the script in RStudio or any R environment.

Author
	•	Your Name
MSc Statistics, University of Edinburgh
📧 your.email@example.com

License

This project is licensed for educational and research purposes. Please cite appropriately if used in publications.

