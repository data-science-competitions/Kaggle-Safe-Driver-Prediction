#' ---
#' title: "Porto Seguro’s Safe Driver Prediction"
#' author: "Harel Lustiger"
#' date: "October, 2017"
#' ---
rm(list = ls()); cat("\014")

# 1. Load libraries and helpers functions
source("src/load_libraries.R")

# 2. Load the Data 
source("src/load_the_data.R")

# 3. Data Preprocessing
source("src/data_preprocessing.R")

# 4. Split the Data
source("src/split_the_data.R")

# 5. Feature Extractor (NOT AVAILABLE)
#

# 6. Model Fitting
source("models/boosted_glm.R")


