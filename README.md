# Exploratory-analysis-of-new-HIV-infections-on-WHO-dataset

## Overview
This repository contains code and data for analyzing HIV infection data. The analysis includes exploring the data, visualizing trends, and performing statistical tests on different variables.

## Data
The data used in this analysis can be found in the "HIV1.csv" file, which is located in the "Dataset" subfolder. You can access it [here]([URL_del_link](https://www.who.int/data/gho/data/indicators/indicator-details/GHO/new-hiv-infections-(per-1000-uninfected-population))https://www.who.int/data/gho/data/indicators/indicator-details/GHO/new-hiv-infections-(per-1000-uninfected-population))

## Code 
The analysis code is written in R and organized into several sections:
  -Data preprocessing: Selecting and renaming columns, handling missing values, and calculating summary statistics.
  -Exploratory analysis: Visualizing the distribution of values, exploring differences between sexes, analyzing variations over the years, and examining geographical areas.
  -Individual area analysis: Reusable functions for analyzing specific geographic areas and creating visualizations.
  -Statistical tests: Conducting non-parametric tests on sex and geographical area variables, including Mann-Whitney U tests, Kruskal-Wallis tests, and post-hoc analyses.

## Usage
You can use this repository to:
  -Explore and analyze the provided HIV infection data.
  -Adapt the code and functions for your own datasets or similar analyses.
  -Learn about data preprocessing, exploratory analysis, and statistical testing in R.

## Dependencies
This analysis relies on several R packages, including dplyr, ggplot2, kableExtra, vioplot, sf, and gridExtra. Make sure to install these packages before running the code.

