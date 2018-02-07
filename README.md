# Indian-Education-Expenditure
# Synopsis
In this project I have collected and cleaned data on Indian education form data.gov.in. This project deals with the consolidation of data from five separate files and converting the data into a form suitable for analysis and visualization. This does not involve any missing value imputation or outlier treatment and focuses on getting the data into tidy-data format only.
# Libraries Used
readr
readxl
dplyr
tidyr
ggplot2
# Code Example

library(readr)

data <- read_delim("projects/inputfile.csv", delim = ",", col_names = T, col_types = cols(.default = col_character))

library(dplyr)

data %>%
  mutate(x = 1:dim(data)[1])

library(ggplot2)

data %>%
  ggplot() + aes(x) + geom_density()
  
 # Motivation
 This project mainly stems from a curiosity to look at the recent trends in Indian Education.
  
