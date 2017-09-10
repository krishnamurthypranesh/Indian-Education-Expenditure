# This project is about cleaning data and getting data spread over multiple files
# into tidy form
# The files contain data on the education systems and its variables from 2001 to
# 2011
# The data is available on the data.gov.in site of the Indian Government

#R version 3.3.2 (2016-10-31)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows >= 8 x64 (build 9200)

#locale:
#[1] LC_COLLATE=English_India.1252  LC_CTYPE=English_India.1252   
#[3] LC_MONETARY=English_India.1252 LC_NUMERIC=C                  
#[5] LC_TIME=English_India.1252    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] bindrcpp_0.2  readxl_1.0.0  ggplot2_2.2.1 readr_1.1.1   tidyr_0.7.0  
#[6] dplyr_0.7.2  

#loaded via a namespace (and not attached):
 #[1] Rcpp_0.12.6      bindr_0.1        magrittr_1.5     hms_0.3         
 #[5] munsell_0.4.3    colorspace_1.3-2 R6_2.2.0         rlang_0.1.2     
 #[9] plyr_1.8.4       tools_3.3.2      grid_3.3.2       gtable_0.2.0    
#[13] lazyeval_0.2.0   assertthat_0.1   tibble_1.3.4     purrr_0.2.3     
#[17] glue_1.1.1       labeling_0.3     cellranger_1.1.0 scales_0.4.1    
#[21] pkgconfig_2.0.1 


# Load Packages -----------------------------------------------------------

# load dplyr

library(dplyr)

# load tidyr

library(tidyr)

# load readr

library(readr)

# load ggplot2

library(ggplot2)

# load read_xl

library(readxl)

# Load Data ---------------------------------------------------------------

# set working directory to directory of script file

# create directory vector

files <- c("Data/Educational Institutions, Scholars And Expenditure.xls",
           "Data/Enrolment in School Education By Courses And Stages In Recognised Institutions.xls",
           "Data/Number Of Teachers In Educational Institutions (All India) upto 2010-11.xls",
           "Data/GDP of India and major Sectors of Economy, Share of each sector to GDP and Growth rate of GDP and other sectors of economy 1951-52 onward.csv",
           "Data/Statement_SES_2011-12-DropOut.csv")

# create list to store input data

inputs <- vector("list", length(files))

# read in data

for(i in seq_along(files)) {
  if(i <= 3) inputs[[i]] <- read_xls(files[[i]], col_names = T)
  
  else inputs[[i]] <- read_csv(files[[i]], col_names = T)
}


# convert input data to numeric 

# define conversion function

convert_numeric <- function(x) {
  for(i in seq_along(x)) {
    x[[i]] <- parse_number(x[[i]])
  }
}


# convert data in inputs to numeric

for(i in seq_along(inputs)){
  for(j in seq_along(inputs[[i]])) {
    inputs[[i]][[j]] <- parse_number(inputs[[i]][[j]])
  }
}

# separate out values of inputs

expenditure <- inputs[[1]]

enrolment <- inputs[[2]]

number_teachers <- inputs[[3]]

gdp <- inputs[[4]]

dropout_rates <- inputs[[5]]


# change variable names to make code readable

# expenditure

names(expenditure) <- c("year", "number_universities", "number_schools", "number_institutions_total", "number_scholars_universities_total", "number_scholars_universities_women", "number_scholars_schools_total", "number_scholars_schools_women", "number_scholars_institutions_total", "number_scholars_institutions_women", "gdp_current_price", "expenditure_public_total", "expenditure_education_dept", "expenditure_education_public_percentage", "expenditure_education_gdp_percentage")

# enrolment

names(enrolment) <- c("year", "nursery_total", "nurseru_women", "primary_total", "primary_women", "middle_total", "middle_women", "high_total", "high_women", "pre_primary_total", "pre_primary_women", "technical_total", "technical_women", "teacher_traning_total", "teacher_training_women", "vocational_general_total", "vocational_general_women")

#number_teachers

names(number_teachers) <- c("year", "junior_teahcers_total", "junior_teachers_women", "junior_teachers_trained_percentage", "middle_teahcers_total", "middle_teachers_women", "middle_teachers_trained_percentage", "high_teachers_total", "high_teachers_women", "high_teachers_trained_percentage", "higher_secondary_teachers_total", "higher_secondary_teachers_women", "higher_secondary_teachers_trained_percentage", "teachers_total", "teachers_women", "total_teachers_universities", "total_teachers_colleges", "total_teachers_number")

# gdp 

names(gdp) <- c("year", "gdp", "agriculture_allied", "agriculture", "industry", "mining", "manufacturing", "services", "agriculture_allied_total_share", "agriculture_total_share", "industry_total_share", "mining_total_share", "manufacturing_total_share", "services_total_share", "gdp_growth_rate", "agriculture_allied_growth_rate", "agriculture_growth_rate", "industry_growth_rate", "mining_growth_rate", "manufacturing_growth_rate", "services_growth_rate")



# Tidy Data ---------------------------------------------------------------

# This stage is here because dropout_rates and gdp aren't in the proper format i,e., they aren't in the same format as the rest of the data, which
# incidentally is in tidy data format. 

# tidy dropout_rates

dropout_rates <- dropout_rates %>% 
  mutate(Year = parse_number(Year)) %>% 
  filter(Year > 2000) %>% 
  select(Year, `All Categories - Classes I-X - Total`) %>% 
  rename(year = Year, 
         dropout_rate = `All Categories - Classes I-X - Total`)


# tidy gdp

gdp <- gdp %>% 
  filter(year %in% c(2000 : 2011))

# combine data into a single dataframe full

full <- left_join(expenditure, enrolment)

full <- left_join(full, number_teachers)

full <- left_join(full, gdp)

full <- left_join(full, dropout_rates)

# cleaning full

full <- full[2:dim(full)[1], ]

# The resultant dataset full, is in tidy data format where every variable 
# is in a separate column and each observation has its own row and a single 
# value is in a particular cell. There are a lot of missing values which 
# require a separate script for their treatment

# Visualizations ----------------------------------------------------------

# some visualizations

# expediture on education and current price gdp

ggplot(full) + aes(gdp_current_price, expenditure_education_dept) + geom_point()
