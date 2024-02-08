#### The use of „We“ and „Us“: Investigating the Impact of Identity Leadership Language on the Intention to Engage in Pro-Environmental Behavior ####

# Oliver Hoffmann 
# Faculty 05: Department of Psychology and Sports Sciences, Goethe Universität Frankfurt am Main
# Bachelor thesis 
# Kira Bibic, Dr. Stefanie Marx-Fleck
# January , 2024
# s6168767@stud.uni-frankfurt.de


### https://pandar.netlify.app/
### https://app.datacamp.com/ 
### https://stackoverflow.com/ 


# RStudio version used for this script: ‘2022.12.0.353’
    # Certain calculations may only work with this version
    # Same goes for the versions of the used packages

#------------------------------------------------------------------------------#
#Loading relevant packages

library(readxl)     # Inserting dataset
library(dplyr)      # cleaning data
library(tidyverse)  # For data manipulation and visualization
library(mediation)  # For mediation analysis
library(lmtest)     # For linear hypothesis testing
library(ggplot2)    # For creating plots
library(corrplot)   # For correlation plots
library(car)        # For diagnostic tests
library(skimr)      # For "skim" argument 
library(moments)    # For skewness (and Kurtosis)
library(flextable)  # to create table and export as word
library(psych)      # describe function, descriptive statistics
library(gtsummary)  # to create a summary table of the dataset (descriptive statistics)
library(rstatix)    # for Levene Test
library(effsize)    # calculate effect size
library(purrr)
library(tidyr)

######################### Environment Setup and Data Loading ############################################
# Clear workspace and set working directory

## Clear workspace
rm(list = ls()) 

## Set up environment
setwd("//Users/oliverhoffmann/Documents/1. UNI/Psychologie/Bachelor /Material/5.) WiSe 22:23/Bachelorarbeit - PsyBSc 21/Datenanalyse/Bachelor Datenauswertung")
getwd()

# Load raw dataset
data_raw <- read_excel("data_pro-environmentalbehavior_2024-02-03_n=123.xlsx")


######################### Initial Data Inspection ####################################

str(data_raw)   # view data structure 
names(data_raw) # Variable Namens 
table(data_raw$CASE) #Rows per ID?
glimpse(data_raw) 


# 1. DATA PREPARATION -----------------------------------------------------

## 1.1 Clean Data ----
# - Convert variable names, select, and rename variables
# - Factorize categorical variables

data_initial_cleaning <- data_raw %>%
  # lower case variables
  rename_with(~tolower(.x)) %>%
  # selcting variables
  dplyr::select(
    id_participant = case, 
    time_sum, 
    rz01, 
    missing,
    starts_with("bi"),
    starts_with("bp"), 
    starts_with("cd"),
    starts_with("ec"),
    starts_with("ih"),
    starts_with("il"),
    starts_with("sd")
  ) %>%
  # renaming 
  rename(
    sex = sd01,
    age = sd02_01,
    education = sd10, 
    occupation = sd14, 
    study_subject = sd19_01, 
    income = sd16, 
    politics = sd20_01, 
    id_scenario = rz01,
    attention_test = ec01_18,
  ) %>%
  # Factorisation of the variables
  mutate(
    sex = factor(sex, levels = 1:3, labels = c("w", "m", "d")),
    education = factor(education, levels = 1:8, labels = c(
      "no degree", "secondary school certificate", "intermediate school certificate",
      "polytechnical high school", "technical college certificate",
      "university entrance qualification", "university degree", "other")
    ),
    id_scenario = factor(
      id_scenario, 
      levels = 1:2, 
      labels = c("ILL", "non_ILL")
    ),
    occupation = factor(
      occupation, 
      levels = 1:8, 
      labels = c(
        "pupil/student",
        "trainee/apprentice",
        "university/college student",
        "employee",
        "civil servant",
        "self-employed",
        "unemployed/job seeking",
        "other")
    ),
    income = factor(
      income, 
      levels = 1:12, 
      labels = c("No own income",
                 "< 250 €",
                 "250 € - < 500 €",
                 "500 € - < 1000 €",
                 "1000 € - < 1500 €",
                 "1500 € - < 2000 €",
                 "2000 € - < 2500 €",
                 "2500 € - < 3000 €",
                 "3000 € - < 3500 €",
                 "3500 € - < 4000 €",
                 "4000 € or more",
                 "NA")
    )
  ) 
str(data_initial_cleaning)
# 1.2 Removing the first row if it's a header row ----
data_initial_cleaning <- data_initial_cleaning[-1,]


# 1.3 set approprate varibales as numeric ---- 
data_initial_cleaning <- data_initial_cleaning %>%
  mutate_at(vars("bi01_01", "bi01_02","bi01_03", "bi01_04", "bi01_05", "bi01_06", "bi01_07", "bi01_08", "bi01_09", "bi01_10", "bp01_01", "bp01_02",
                 "bp01_03", "bp01_04","bp01_05", "bp01_06", "bp01_07", "bp01_08", "bp01_09", "bp01_10", "cd02_01", "ec01_01", "ec01_02", "ec01_03",
                 "ec01_04", "ec01_05", "ec01_06", "ec01_07", "ec01_08", "ec01_09", "attention_test", "ec01_10", "ec01_11", "ec01_12", "ec01_13", "ec01_14",
                 "ec01_15", "ec01_16", "ec01_17", "ih01_01", "ih01_02", "ih01_03", "ih01_04", "il01_01", "il01_02", "il01_03", "il01_04", "time_sum", 
                 "id_participant", "age"), 
            as.numeric)






# 2. SCALE FORMATION AND INITIAL PROCESSING ---------------------------------

# 2.1 recoding of items ---- 
## Pro-environmental Behavior intentions 
data_initial_cleaning$bi01_01 <- 8 - data_initial_cleaning$bi01_01
data_initial_cleaning$bi01_02 <- 8 - data_initial_cleaning$bi01_02
data_initial_cleaning$bi01_06 <- 8 - data_initial_cleaning$bi01_06
data_initial_cleaning$bi01_10 <- 8 - data_initial_cleaning$bi01_10

## Post manipulation Pro-environmental Behavior intentions 
data_initial_cleaning$bp01_01 <- 8 - data_initial_cleaning$bp01_01
data_initial_cleaning$bp01_02 <- 8 - data_initial_cleaning$bp01_02
data_initial_cleaning$bp01_06 <- 8 - data_initial_cleaning$bp01_06
data_initial_cleaning$bp01_10 <- 8 - data_initial_cleaning$bp01_10


# 2.2 Scale formation ---- 
data_scales <- data_initial_cleaning %>%
  rowwise() %>%
  mutate(
    scale_bipre = mean(c_across(starts_with("bi"))), #calculate means
    scale_bipost = mean(c_across(starts_with("bp"))), #calculate means
    scale_ec = sum(c_across(c("ec01_06", "ec01_07", "ec01_08", "ec01_09", # create sum score of scale
                               "ec01_10", "ec01_11", "ec01_12", "ec01_13",
                               "ec01_14", "ec01_15", "ec01_16", "ec01_17"))),    # excluding EC01_18, because it was a manipulation check 
    scale_cd = cd02_01,
    scale_iwah =  mean(c_across(starts_with("ih"))), #calculate means
    scale_ili = mean(c_across(starts_with("il"))), #calculate means
  ) %>%
  ungroup() 
# 2.3 Restructuring the dataset for better logic ---- 
data_scales <- data_scales %>%
  relocate(c(id_scenario, sex, age, education, occupation, sd14_08, study_subject, income, politics, attention_test, time_sum, missing), .before = bi01_01) # %>% 



# 3. DATA FILTERING AND CLEANING FURTHER ------------------------------------
# Exclude participants based on criteria and check for missing data

# 3.1 exclude everyone with a missing value on scale_bipre, scale_bipost and with age < 18 years ----
data_narm <- data_scales %>% 
  filter(!is.na(scale_bipre), !is.na(scale_bipost), age >= 18)

 
# Double checking/ Determining total NA's 
sum(is.na(data_narm$scale_bipost))
sum(is.na(data_narm$scale_bipre))
sum(data_narm$age < 18)
# no more missing data
# calculations as per pre-regestratio can begin with data_narm dataset


######################### Searching for (time) outliers ######################### 

# 4.1 TIME OUTLIERS --------------------------------------------
# 4. 1.1  Calculate mean and median completion time----
stats <- data_narm %>%
  summarise(
    mean_time = mean(time_sum, na.rm = TRUE),
    median_time = median(time_sum, na.rm = TRUE),
    sd_time = sd(time_sum, na.rm = TRUE)
  )

# Extracting the values
mean_time <- stats$mean_time
median_time <- stats$median_time
sd_time <- stats$sd_time
  
# 4.1.2 Calculating +- 3 SD from median as cutoffs ---- 

# Calculate the value 3 standard deviations below the median
value_below_median <- median_time - (3 * sd_time)
# Print results
value_below_median
# 97.37783 sec

# Calculate the value 3 standard deviations above the median
value_above_median <- median_time + (3 * sd_time)
# Print results
value_above_median
# 678.6222 sec


# How many participants completed the survey > 3 standard deviations from the average survey completion time for the whole sample?
data_narm %>%
  filter(time_sum < 97) %>%
  summarise(count = n())
# 1

data_narm %>%
  filter(time_sum > 678) %>%
  summarise(count = n())
# 0


# 4.1.3 Density Plot for time +/- apa - style  ---- 
ggplot(data_narm, aes(x = time_sum)) +
  geom_density(fill = "#5470D6") +  # Simple color scheme # ,color = "black"
  scale_x_continuous(name = "Time for Completion (sec)", limits = c(0, 650),
                     breaks = seq(0, 600, 100)) +
  scale_y_continuous(name = "Number of Participants", limits = c(0, 0.0047)) +
  geom_vline(xintercept = 96, linetype = "dotted", linewidth = 0.9, color = "red") +
  geom_rug(alpha = 0.2) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),  # Consistent font and size
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 0.5))


#--------------------- 4.2 OUTLIERS --------------------------------------------


## Boxplots to see Outlies
boxplot(data_narm$scale_ec, main = "Boxplot Environmental Concern")
boxplot(data_narm$scale_bipost, main = "Boxplot Pro-Environmental Behavior Intentions")
boxplot(data_narm$scale_iwah, main = "Boxplot Identification with All Humanity")
boxplot(data_narm$scale_ili, main = "Boxplot Identity Leadership Language")

# Outlier exclusion
# iwah
Q1iwah <- quantile(data_narm$scale_iwah, 0.25)
Q3iwah <- quantile(data_narm$scale_iwah, 0.75)
IQRiwah <- Q3iwah - Q1iwah

# Calculating bounds
lower_bound_iwah <- Q1iwah - 1.5 * IQRiwah
upper_bound_iwah <- Q3iwah + 1.5 * IQRiwah

# Excluding outliers --> not performed here as per pre-regestration
# data_no_outliers <- data[data_narm$scale_iwah >= lower_bound_iwah & data$scale_iwah <= upper_bound_iwah, ]

# Outlier exclusion
# bipost
# Q1_bipost <- quantile(data_narm$scale_bipost, 0.25)
# Q3_bipost <- quantile(data_narm$scale_bipost, 0.75)
# IQR_bipost <- Q3 - Q1
# 
# # Calculating bounds
# lower_bound_bipost <- Q1_bipost - 1.5 * IQR_bipost
# upper_bound_bipost <- Q3_bipost + 1.5 * IQR_bipost

# Excluding outliers
# data_no_outliers <- data[data_narm$scale_bipost >= lower_bound_bipost & data$scale_bipost <= upper_bound_bipost, ]



# 4.3. -------Outliers failing the ATTENTION TEST ----------------------------

# 5.1 Assess successful answer of attention check ---- 
# show how participants answered test items & NAs 
table(data_narm$attention_test, useNA = "ifany")

sum(data_narm$attention_test == 6, na.rm = TRUE)
# 102 participants ticked the correct box for the test item

sum(data_narm$attention_test != 6, na.rm = TRUE)
# 7 participants did not tick the correct box for the test item

# 5.2 Bar plot of attention_test values ----

ggplot(data_narm, aes(x = factor(attention_test))) +
  geom_bar() +
  labs(title = "Distribution of Attention Test Responses", x = "Response", y = "Count")

# 5.3 Create new dataset, exclude all but attention_test value of 6 ---- 
# not perforemed as per pre-regestration 

# data_only_attention <- filter(data_narm, attention_test == 6)
#str(data_only_attention)


######################### Creating functions for further calculations ##########
# Define functions for reshaping, reliability analysis, and APA styling

# Function definition
pivot_wider_data <- function(.data) {
  pivot_wider(   # Use the pivot_wider function from the tidyr package to reshape the data
    data = .data,  # Use the input data as the first argument
    # id_cols = -id_item,     # Specify which columns to use as the identifier variables
    names_from = id_item,     # Use the id_item column as the new variable names for the wide format
    values_from = value       # Use the value column as the new variable values for the wide format
  )
}


# Function to calculate Cronbach's alpha
cronbach_alpha <- function(x) {
  # Use the alpha function from the psych package to calculate Cronbach's alpha
  # and store the result in a variable
  psych::alpha(x) %>% 
    # remove total and raw_alpha from the result
    chuck("total", "raw_alpha") %>% 
    # suppress warning messages
    suppressWarnings() %>%
    # suppress messages
    suppressMessages()
}


# Function to calculate McDonald's omega coefficient
mc_omega <- function(x) {
  # Use the omega function from the psych package to calculate McDonald's omega coefficient
  # and store the result in a variable
  psych::omega(x, plot = FALSE) %>%
    # remove omega.tot from the result
    chuck("omega.tot") %>% 
    suppressWarnings() %>%
    suppressMessages()
}


# Function to apply APA style to flextable
theme_apa <- function(x, ...) {
  # Check if the input is a flextable object
  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.", "theme_apa()"))
  }
  # Define the border style for APA style
  apa.border <- list("width" = 1, color = "black", style = "solid")
  # Apply Times New Roman font to all text in the table
  x <- flextable::font(x, part = "all", fontname = "Times New Roman")
  # Set line spacing to 1 for all text in the table
  x <- flextable::line_spacing(x, space = 1, part = "all")
  # Add top border to the head of the table
  x <- flextable::hline_top(x, part = "head", border = apa.border)
  # Add bottom border to the head of the table
  x <- flextable::hline_bottom(x, part = "head", border = apa.border)
  # Add top border to the body of the table
  x <- flextable::hline_top(x, part = "body", border = apa.border)
  # Add bottom border to the body of the table
  x <- flextable::hline_bottom(x, part = "body", border = apa.border)
  # Center align all text in the table
  x <- flextable::align(x, align = "center", part = "all")
  # Center align all cells in the table
  x <- flextable::valign(x, valign = "center", part = "all")
  # Format all double values with 2 decimal digits
  x <- flextable::colformat_double(x, digits = 2)
  flextable:::fix_border_issues(x)
}

    
######################### Descriptive Statistics ###############################


#summary of the data, number of observations, variables,variable type, missing values, summary statistics 
    # (e.g., mean, standard deviation, min, max, quartiles, etc.) & histograms 
skim(data_narm) # from package skimr

# 6. RELIABILITY ANALYSIS -------------------------------------------------  


# 6.1 Prepare the data for reliability analysis ----
data_reliability <- data_narm %>%
  # Remove unnecessary columns
  dplyr:: select(-c(id_scenario, age, sex, education, occupation, sd14_08, study_subject, income, politics, attention_test, time_sum, missing, cd02_01)) %>%
  # Convert id_participant to a factor
  mutate(id_participant = as_factor(id_participant)) %>%
  # Reshape data from wide to long format
  pivot_longer(
    cols = -id_participant,
    names_to = "id_item",
    values_to = "value"
  ) %>%
  # Assign a scale to each item based on the item name
  mutate(id_scale = case_when(
    str_starts(id_item, "bi") ~ "bipre",
    str_starts(id_item, "bp") ~ "bipost",
    str_starts(id_item, "ec") ~ "ec",
    str_starts(id_item, "ih") ~ "iwah",
    str_starts(id_item, "il") ~ "ili"
    ),
    # Convert id_scale to a factor
    id_scale = as_factor(id_scale)
  ) %>%
  nest_by(id_scale) %>%
  ungroup() %>%
  # Use pivot_wider_data function to reshape data from long to wide format
  mutate(data = map(.x = data, .f = pivot_wider_data)) %>%
  # Calculate Cronbach's alpha for each scale
  mutate(
    alpha = map_dbl(.x = data, .f = ~cronbach_alpha(.x %>% 
                                                      dplyr:: select(-id_participant))),
  # Calculate McDonald's omega for each scale
  omega = map_dbl(.x = data, .f = ~mc_omega(.x %>% 
                                              dplyr:: select(-id_participant))),
  ) %>%
  # Count the number of items in each scale
  mutate(item_count = map_dbl(data, ~.x %>% 
                                dplyr:: select(-id_participant) %>% length()), .before = data)


# 6.2 Generate descriptive statistics for scales ----
descriptive_scales <- data_scales %>%
  # Select only columns that start with "scale"
  dplyr:: select(starts_with("scale")) %>% 
  # Reshape data from wide to long format
  pivot_longer(cols = everything(), names_to = "scale") %>%
  # Group data by scale
  group_by(scale) %>%
  # Remove missing values
  drop_na(value) %>%
  # Calculate mean, standard deviation, skewness, and kurtosis
  summarise(
    mean = mean(value),
    sd = sd(value),
    skewness = skewness(value),
    kurtosis = kurtosis(value),
  ) %>%
  # Remove "scale_" prefix from id_scale
  mutate(id_scale = str_remove(scale, "scale_"), .before = everything()) %>%
  # Remove the "scale" column
  dplyr:: select(-scale) %>%
  # Join with data_reliability on id_scale
  left_join(., data_reliability, by = "id_scale") %>%
  # Select the desired columns
  dplyr:: select(id_scale, item_count, mean:kurtosis, alpha, omega) 

count(data_narm)

# 6.3 Generate a table .doc of descriptive statistics and reliability measures ----
descriptive_scales %>%
  # Round numeric values to 2 decimal places
  mutate(across(where(is.numeric), ~round(.x, 2))) %>%
  # Create a flextable
  flextable() %>%
  # Apply the APA theme
  theme_apa() %>%
  # Save the table as a Word document
  save_as_docx(path = "output/tables/descriptives_reliability.docx")


# 6.4 SAMPLE DESCRIPTIVE STATISTICS ----

## 6.4.1 age ---- 
# Describing age range with mean and sd before excluding na's
data_narm %>%
  pull(age) %>%
  describe()
# 24.43

## 6.4.2 gender ---- 
# Summarizing gender
data_narm %>% 
  drop_na(sex) %>%
  group_by(sex) %>% 
  summarize(n())
# w = 73 | m = 34 | d = 2

#percentage gender
round(prop.table(table(data_narm$sex)), 2)

## 6.4.3 education level ----
# Summarizing education level
data_narm %>%
  drop_na(education) %>%
  group_by(education) %>% 
  summarize(n = n())


#how many people were allocated to each scenario? 
table(data_narm$id_scenario)


## 6.4.5 political affiliation ----
data_sample_info <- data_narm %>%
  # Add political affiliation category to meaningfully categorise the continious scores into the 20, 40, 60,80 percentile 
  mutate(
    politics = case_when(
      politics >= 1 & politics <= 20 ~ "Very progressiv",
      politics >= 21 & politics <= 40 ~ "Rather progressiv",
      politics >= 41 & politics <= 60 ~ "Neutral",
      politics >= 61 & politics <= 80 ~ "Rather conservative",
      politics >= 81 ~ "Very conservative",
      TRUE ~ NA_character_  # Add NA for values outside the specified ranges or missing
    )
  )

table(data_sample_info$politics)

# Histogramms
ggplot(data_sample_info, aes(x = factor(politics, levels = c("Very progressiv", "Rather progressiv", "Neutral", "Rather conservative", "Very conservative")))) +
  geom_bar(fill = "#5470D6") +
  labs( #title = "Political Affiliation of Sample",
       x = "Category",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  theme(text = element_text(family = "Times New Roman"), 
        title = element_text(size = 18),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 0.5))





## 6.4.4 Study subjects ----

# View unique study subjects
unique_study_subjects <- unique(data_narm$study_subject)
print(unique_study_subjects)

# Standardize study_subject labels
data_sample_info <- data_narm %>%
  mutate(
    study_subject = case_when(
      study_subject %in% c("Psychologiestudium", "Psychologie", "Psycholgie", "Paychologie", "Master klinische Psychologie und Psychotherapie", "psychologie", "Pdychologie", "Psychologie Master", "Psycholgie") ~ "Psychology",
      study_subject %in% c("Jura", "Rechtswissenschaften", "Rechtswissenschaft") ~ "Law",
      study_subject %in% c("Neuroscience", "Neurowissenschaften") ~ "Neuroscience",
      TRUE ~ study_subject  # Keeps all other subjects as they are
    )
  )

new_unique_study_subjects <- unique(data_sample_info$study_subject)
print(new_unique_study_subjects)



## 6.4.5 income ----

# visual representation of the income categories 
ggplot(data_narm, aes(x = income)) +
  geom_bar(fill = "#5470D6") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +  # Rotate x labels for readability
  labs(x = "Category", y = "Count", ) + # title = "Distribution of Income") +
  theme(text = element_text(family = "Times New Roman"), 
        title = element_text(size = 18),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 0.5))

### 6.4.6 Belief in antropogenic climate crisis ---- 

data_narm$scale_cd <- as.numeric(data_narm$scale_cd)

skim(data_narm$scale_cd)

# how many in each category?
data_narm %>%
  group_by(scale_cd) %>%
  summarize(count = n())

ggplot(data_narm, aes(x = factor(scale_cd))) +
  geom_bar(fill = "#5470D6") +
  labs( #title = "Belief in Man-Made Climate Crisis",
    x = "Category",
    y = "Count") +
  # scale_x_discrete(drop = FALSE) +  # drop = FALSE to include unused factor levels
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  theme(text = element_text(family = "Times New Roman"), 
        title = element_text(size = 18),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 0.5))



# 6. 5 Create a summary table of descriptive statistics by id_scenario ----
data_sample_info %>%
  # Remove rows with missing id_scenario values
  drop_na(id_scenario) %>%
  # Select the desired columns (all from sex to attention check)
  dplyr::select(sex:attention_test, scale_bipost, id_scenario) %>%
  # Use the gtsummary package to generate a summary table
  gtsummary::tbl_summary(
    data = ., 
    by = id_scenario,
    # Specify statistics to include in the table
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  # Apply the APA theme
  as_flex_table() %>%
  theme_apa() %>%
  # Save the table as a Word document
  save_as_docx(path = "output/tables/descriptives_sample_grouped.docx")




#### Assessing ceiling effects (though redundant with skimr function) ---- 

# Environmental Concern 
ggplot(data_narm, aes(x = scale_ec)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of scale_ec",
       x = "Scale EC Score",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# PEBI Post
ggplot(data_narm, aes(x = scale_bipost)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black") +
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +  # Set x-axis limits and breaks
  theme_minimal() +
  labs(title = "Histogram of scale_bipost",
       x = "Scale BI Post Score",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# PEBI Pre
ggplot(data_narm, aes(x = scale_bipre)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black") +
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +  # Set x-axis limits and breaks 
  theme_minimal() +
  labs(title = "Histogram of scale_bipre",
       x = "Scale BI Pre Score",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

## to assess percentage value at ceiling 
# Calculate descriptive statistics 
descriptive_st <- data_narm %>% 
  summarize(Max_Score = max(scale_ec, na.rm = TRUE),
            Percent_At_Max = mean(scale_ec == max(scale_ec, na.rm = TRUE), na.rm = TRUE),
            Skewness = skewness(scale_ec, na.rm = TRUE))

# Output descriptive statistics
print(descriptive_st)

# Calculate and print the percentage of participants scoring at the maximum
percent_at_ceiling <- sum(data_narm$scale_ec == max(data_narm$scale_ec, na.rm = TRUE), na.rm = TRUE) / nrow(data_narm) * 100
print(paste("Percentage at ceiling:", percent_at_ceiling))
# 0.9 % of sample at ceiling 

# for ceiling effects in percentage of PEBI Post/ pre change "scale_ec" with scale_bipre and scale_bipost respectively 


############################ INFERENTIAL STATISTICS #############################
#UV: leaderhip language (id_scenario)
#AV: pro-environmental behavior intentions ("scale_bipre &" scale_bipost)
#Mediator: Identification with humanity (scale_iwah)
#Moderator: Environmental Concern (scale_ec)
#Control: manipulation check (scale_ili), climate denial (scale_cd)


######################### T-Test: Manipulation Check Leadership Language -n. s. ###################
# Purpose: Compare behavior intentions before and after exposure to the stimulus.

# MC.0 #Preparing the data ----

data_mc_ili <- data_narm %>%
  dplyr:: select(starts_with("id"), scale_ili)  %>% # Selects only columns that start with "id", "scale_intrinreg", and "scale_all_extrinsic"
  mutate(id_scenario = factor(id_scenario)) %>%  # Converts the "id_scenario" column to a factor variable
  drop_na() # Removes any rows with missing values


# MC.1 Independent t-tests ----

describeBy(x = data_mc_ili$scale_ili, group = data_mc_ili$id_scenario)        # beide Gruppen im Vergleich 


# T-tests for the difference in means of "scale_bipre" and "scale_bipost" between levels of the "id_scenario" variable, with a 
# Bonferroni-Holm correction for multiple comparisons
t_test_ili <- t_test(
  data = data_mc_ili,
  formula = scale_ili ~ id_scenario,
  alternative = "greater",  # "greater" testet, ob IL > NIL
  detailed = TRUE
)
t_test_ili

# Differenz der Mittelwerte zwischen den beiden Gruppen. 
# Mittelwert von scale_ili in der Gruppe 'ILL' (Identity Leadership) um 0.183 Punkte höher als in der Gruppe 'non_ILL' (Non-Identity Leadership).
# estimate1 und estimate2: Mittelwerte der scale_ili für jede Gruppe separat.

# p-Wert 0.193, --> keine statistisch signifikante Differenz zwischen den Gruppen  

# --> kann nicht eindeutig geschlussfolgert werden , dass Identity Leadership zu einem höheren 'scale_ili' Wert führt als Non-Identity Leadership.


# testing all dimensions of ili individually (il01_01 - il01_04)
t_test_ili_dim <- t_test(
  data = data_narm,
  formula = il01_04 ~ id_scenario,
  alternative = "greater",  # "greater" testet, ob IL > NIL
  detailed = TRUE # giving more details in the result 
)
t_test_ili_dim
# all yield non-significant results 


#Export results in apa style
bind_rows(t_test_ili_dim) %>%
  dplyr:: select(.y., estimate1, estimate2, statistic, df, p) %>% 
  mutate(p = p_format(p, digits = 3, leading.zero = FALSE, accuracy = 0.001) %>% 
           p_mark_significant(p)
  ) %>%
  rename_with(~c("Variable", "Identity leadership Language", "Non-Identity leadership Language", "t", "df", "p")) %>%
  mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
  flextable() %>%
  theme_apa() %>%
  italic(part = "header", j = -c(1:3)) 




### MC.2 Plot ----
library(RColorBrewer) # color palatte (not used here)
library(wesanderson) # color palatte 

# Enhanced plot
# Extract t-test results

t_value_t_test_ili <- t_test_ili$statistic
p_value_t_test_ili <- t_test_ili$p

# Add the results to the plot
ggplot(data_mc_ili, aes(x = id_scenario, y = scale_ili, fill = id_scenario)) +
  geom_boxplot(position = position_dodge(1.9), width = 0.4) +
  ggdist:: stat_slab( # violin plot to the plot, showing the distribution of "scale_bipost" by "id_scenario"
    side = "left",
    color = NA,
    position = position_nudge(x = -0.2),
    normalize = "all",
    alpha = .8,
    trim = FALSE,
    scale = 0.4,
    show.legend = FALSE,
    expand = TRUE,
    n = 109
  ) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6) +
#  annotate("text", x = 0.3, y = 1.5, 
#          label = sprintf("t = %.2f, p = %.3f", t_value_t_test_ili, p_value_t_test_ili),
#           size = 5, color = "black", fontface = "bold", hjust = 0.5) +
  scale_fill_manual(values = wes_palette("Zissou1")[1:2]) + # beautiful alternativ is GrandBudapest2
  scale_y_continuous(limits = c(NA, 8)) + # where max_value is the maximum value you want to include on the y-axis
  labs( # title = "Manipulation Check of Leadership Language on ILI",
        # subtitle = "Box-Scatter-Slab Plot with t-test results",
       x = "ID Scenario",
       y = "Identity Leaderhip Inventory") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none",  # Hide legend if not necessary
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))



# MC. 3 Assumptions----
# do for il01_01 - il01_04, by exchanging for scale_ili

# MC. 3.1 Normality ----
# Shapiro-Wilk-Test für die Normalverteilung
shapiro.test(data_mc_ili$scale_ili[data_mc_ili$id_scenario == "ILL"])
# p-value = 0.2852 ns.
# assumption met: normal distribution
shapiro.test(data_mc_ili$scale_ili[data_mc_ili$id_scenario == "non_ILL"])
# p-value = 0.6134 ns. 
# assumption met: normal distribution

# Histogramm für visuelle Überprüfung
hist(data_mc_ili$scale_ili[data_mc_ili$id_scenario == "ILL"], main="Histogram - ILL", xlab="scale_ili")
hist(data_mc_ili$scale_ili[data_mc_ili$id_scenario == "non_ILL"], main="Histogram - non_ILL", xlab="scale_ili")

# Q-Q-Plot
qqnorm(data_mc_ili$scale_ili[data_mc_ili$id_scenario == "ILL"])
qqline(data_mc_ili$scale_ili[data_mc_ili$id_scenario == "ILL"])
qqnorm(data_mc_ili$scale_ili[data_mc_ili$id_scenario == "non_ILL"])
qqline(data_mc_ili$scale_ili[data_mc_ili$id_scenario == "non_ILL"])


# MC. 3.2 homogeneity of variances ----
# Levene-Test
leveneTest(scale_ili ~ id_scenario, data = data_mc_ili)
# p = 0.667 ns. 
# assumption met: Varianzhomogenität
leveneTest(scale_ili ~ id_scenario, data = data_mc_ili) %>%
  tidy()


# Check sample size
table(data_mc_ili$id_scenario)
# 53 : 56 

# MC. 3.3. Boxplot to check for outliers ---- 
boxplot(scale_ili ~ id_scenario, data = data_mc_ili)
# no outliers 







##### Behavior Intentions Pre vs. Post stimulus ----

t_test_result_mc_bi <- t.test(data_narm$scale_bipost, data_narm$scale_bipre, paired = TRUE)

print(t_test_result_mc_bi)

# Effect size ----
d2 <- cohen.d(data_narm$scale_bipost, data_narm$scale_bipre, 
              paired = TRUE,  #paired steht fuer 'abhaengig'
              within = FALSE, #wir brauchen nicht die Varianz innerhalb
              na.rm = TRUE)   
d2



# Calculate the mean for pre-intervention
mean_pre <- mean(data_narm$scale_bipre, na.rm = TRUE)
print(paste("Mean Pre-intervention:", mean_pre))
sd(data_narm$scale_bipre)
describeBy(data_narm$scale_bipre)

# Calculate the mean for post-intervention
mean_post <- mean(data_narm$scale_bipost, na.rm = TRUE)
print(paste("Mean Post-intervention:", mean_post))
sd(data_narm$scale_bipost)
describeBy(data_narm$scale_bipost)



## 1. Pre-post Assumptions ----

# Checking normality of the differences
differences <- data_narm$scale_bipost - data_narm$scale_bipre
shapiro_test_diff <- shapiro.test(differences)
print(shapiro_test_diff)

# Histogramm
difference <- data_narm$scale_bipost-data_narm$scale_bipre
hist(difference, 
     xlim=c(-3,3), 
     ylim = c(0,1),
     main="Verteilung der Differenzen", 
     xlab="Differenzen", 
     ylab="", 
     las=1, 
     freq = F)
curve(dnorm(x, mean=mean(difference, na.rm = T), sd=sd(difference, na.rm = T)), 
      col="blue", 
      lwd=2, 
      add=T)

#qqplot
qqnorm(difference)
qqline(difference)


# Visual inspection for outliers in the differences
boxplot(differences, main = "Boxplot of Differences (Post - Pre)")

wilcox_test_result_mc_bi <- wilcox.test(data_narm$scale_bipre, data_narm$scale_bipost, paired = TRUE)
print(wilcox_test_result_mc_bi)

# effect size 

p_value_mc_bi <- 2.105e-09  #  p-value from  test
N <- 109  # Total number of observations 

# Calculate Z from the p-value
Z_mc_bi <- qnorm(p_value_mc_bi / 2, lower.tail = FALSE)

# Calculate effect size r
r_mc_bi <- Z_mc_bi / sqrt(N)

# Print the effect size
print(r_mc_bi)





##################### H1: ALeadership Language Effect ###################
# Main Hypotheses (H1): Participants exposed to identity leadership language will demonstrate 
# significantly higher pro-environmental behavior intentions compared to those exposed to 
# non-identity leadership language.

# Purpose: Compare mean scores of pro-environmental behavior intentions across leadership language groups.

## H1. 0 Preparing the data ---- 

# Create a new dataframe for hypothesis 1
data_hypothesis1 <- data_narm %>% 
  # Convert the id_scenario column to a factor
  mutate(id_scenario = factor(id_scenario))


# descriptive (s.h. tabelle)
mean_differences <- data_hypothesis1  %>%
  # drop all NA rows
  # drop_na() %>%
  # group by id_scenario
  group_by(id_scenario) %>%
  # calculate the mean and standard deviation of scale_bipost
  summarise(mean = mean(scale_bipost, na.rm = TRUE),
            sd = sd(scale_bipost, na.rm = TRUE))
mean_differences # shows that mean differences between the groups are basically non existed


## H1. t test ----

# id_scenario has only two levels, a two-sample t-test would provide the same information as a one-way ANOVA. 
#ANOVA is essentially a generalization of the t-test for more than two groups, 
# when comparing just two groups, both tests will give you the same p-value for the group difference.


# Perform a two-sample t-test
t_test_result_h1 <- t.test(scale_bipost ~ id_scenario, data = data_hypothesis1, var.equal = T )

# Output the results
print(t_test_result_h1)


# Summarize descriptive statistics by group
data_hypothesis1 %>%
  group_by(id_scenario) %>%
  summarise(
    count = n(),
    mean = mean(scale_bipost, na.rm = TRUE),
    sd = sd(scale_bipost, na.rm = TRUE),
    se = sd(scale_bipost, na.rm = TRUE) / sqrt(n())
  ) %>%
  print()

## Assumption tests----
# Normality Test
shapiro.test(data_hypothesis1$scale_bipost[data_hypothesis1$id_scenario == "ILL"])
# p = 0.4936 ns
shapiro.test(data_hypothesis1$scale_bipost[data_hypothesis1$id_scenario == "non_ILL"])

# For group "ILL"
qqnorm(data_hypothesis1$scale_bipost[data_hypothesis1$id_scenario == "ILL"])
qqline(data_hypothesis1$scale_bipost[data_hypothesis1$id_scenario == "ILL"])

# For group "non_ILL"
qqnorm(data_hypothesis1$scale_bipost[data_hypothesis1$id_scenario == "non_ILL"])
qqline(data_hypothesis1$scale_bipost[data_hypothesis1$id_scenario == "non_ILL"])


# Homogeneity of Variances Test
library(car)
leveneTest(scale_bipost ~ id_scenario, data = data_hypothesis1)
# 0.661
# assumption of equal variances is met


# # Perform a two-sample Welch's t-test
# # unequal variances
# # comparing the means of scale_bipost between two groups of id_scenario.
# t_test_result_welch <- t.test(scale_bipost ~ id_scenario, data = data_hypothesis1, var.equal = FALSE)
# 
# # Output the results
# print(t_test_result_welch)
# # p-value = 0.9021 is the probability of observing the data, or something more extreme, if the null hypothesis (that there is no difference in means) is true. 



### H1. Exploratory Regression ---- 

# measure the impact of your independent variable (id_scenario) on the dependent variable (scale_bipost) while controlling for other variables  age, gender, income, and political affiliation. 
# ANCOVA  extends ANOVA by including covariates—variables 

## 'gender' and 'political_affiliation' are factors 
## 'age' and 'income' are numeric

### Data Preperation ---- 

# Create a new dataframe for hypothesis 1
data_hypothesis1 <- data_narm %>% 
  # Convert the id_scenario column to a factor
  mutate(id_scenario = factor(id_scenario))


#setting the contrasts for the "id_scenario" variable  
#to be of type "treatment" with a base level of 1 and 2 levels in total.
#allows R to correctly interpret levels of the independent variable & perform the appropriate statistical test.

contrasts(data_hypothesis1$id_scenario) <- contr.treatment(n = 2, base = 1) # sets the first level of the factor as the reference or baseline category. In the context of statistical analysis, this means that the effects of the second level of id_scenario will be compared against the first level.
#Scenario 1 = Identity_language.png
#         2 = Non-identity_language.png (see sosci survey) 


# To simplify the income variable by grouping a wide range of income categories into broader, more manageable categories. This reduces complexity and facilitates easier interpretation of the model’s results.
data_hypothesis_ancova <- data_hypothesis1 %>%
  mutate(
    income_category = case_when(
      income %in% c("No own income", "< 250 €") ~ "Low income",
      income %in% c("250 € - < 500 €", "500 € - < 1000 €") ~ "Lower Middle Income",
      income %in% c("1000 € - < 1500 €", "1500 € - < 2000 €", "2000 € - < 2500 €") ~ "Middle Income",
      income %in% c("2500 € - < 3000 €", "3000 € - < 3500 €", "3500 € - < 4000 €", "4000 € or more") ~ "High Income",
      TRUE ~ "NA" # Handles missing data
    )
  )

# same for politics 
data_hypothesis_ancova <- data_hypothesis_ancova %>%
  mutate(
    politics_category = case_when(
      politics >= 1 & politics <= 20 ~ "Very progressive",
      politics >= 21 & politics <= 40 ~ "Rather progressive",
      politics >= 41 & politics <= 60 ~ "Neutral",
      politics >= 61 & politics <= 80 ~ "Rather conservative",
      politics >= 81 ~ "Very conservative",
      TRUE ~ NA_character_  # Handles missing data or values outside specified ranges
    )
  )

# daten zentrieren 
data_hypothesis_ancova$age_centered <- scale(data_hypothesis_ancova$age, scale = FALSE)

# convert to factor 
data_hypothesis_ancova$id_scenario <- as.factor(data_hypothesis_ancova$id_scenario)


##### Calculations ----- 

# Full Model (All Predictors Included):
lm.full <- lm(scale_bipost ~ id_scenario + age + sex + income_category + politics_category, data = data_hypothesis_ancova)
summary(lm.full)

# Reduced Model (Only Significant Predictors):
lm.reduced <- lm(scale_bipost ~ politics_category, data = data_hypothesis_ancova)
summary(lm.reduced)

# Models Excluding Each Predictor One at a Time:
lm.no_id_scenario <- lm(scale_bipost ~ age + sex + income_category + politics_category, data = data_hypothesis_ancova)
summary(lm.no_id_scenario)

lm.no_age <- lm(scale_bipost ~ id_scenario + sex + income_category + politics_category, data = data_hypothesis_ancova)
summary(lm.no_age)

lm.no_sex <- lm(scale_bipost ~ id_scenario + age + income_category + politics_category, data = data_hypothesis_ancova)
summary(lm.no_sex)

lm.no_income <- lm(scale_bipost ~ id_scenario + age + sex + politics_category, data = data_hypothesis_ancova)
summary(lm.no_income)

lm.no_politics <- lm(scale_bipost ~ id_scenario + age + sex + income_category, data = data_hypothesis_ancova)
summary(lm.no_politics)

# Models with Interaction Terms:
lm.interaction <- lm(scale_bipost ~ id_scenario * politics_category + age + sex + income_category, data = data_hypothesis_ancova)
summary(lm.interaction)

# Models with Polynomial Terms:
lm.polynomial <- lm(scale_bipost ~ id_scenario + poly(age, 2) + sex + income_category + politics_category, data = data_hypothesis_ancova)
summary(lm.polynomial)

# Hierarchical Models:
lm.hierarchical1 <- lm(scale_bipost ~ id_scenario, data = data_hypothesis_ancova)
summary(lm.hierarchical1)

lm.hierarchical2 <- update(lm.hierarchical1, . ~ . + age)
summary(lm.hierarchical2)

lm.hierarchical3 <- update(lm.hierarchical2, . ~ . + sex)
summary(lm.hierarchical3)

lm.hierarchical4 <- update(lm.hierarchical3, . ~ . + income_category)
summary(lm.hierarchical4)

lm.hierarchical5 <- update(lm.hierarchical4, . ~ . + politics_category)
summary(lm.hierarchical5)

# Model with Only Control Variables
lm.controls_only <- lm(scale_bipost ~ age + sex + income_category + politics_category, data = data_hypothesis_ancova)
summary(lm.controls_only)

# Stepwise Regression Models:
# iteratively adds (forward selection) or removes (backward elimination) variables based solely on statistical criteria such as the Akaike Information Criterion (AIC)
lm.stepwise <- step(lm.full, direction = "both")
summary(lm.stepwise)


## assumption checks --
# do for each model 

# 1. Check for Linearity ----
# Scatter plot of fitted values vs residuals
plot(fitted(lm.full), residuals(lm.full))
abline(h = 0, col = "red")

# 2. Check for Homoscedasticity ----
# Scatter plot of fitted values vs standardized residuals
plot(fitted(lm.full), rstandard(lm.full))
abline(h = 0, col = "red")

# 3. Check for Normality of Residuals ----
# Histogram of residuals
hist(residuals(lm.full))

# Q-Q plot of residuals
qqnorm(residuals(lm.full))
qqline(residuals(lm.full), col = "red")

# Shapiro-Wilk test ----
shapiro.test(residuals(lm.full))

# 4. Check for Independence of Residuals ----
# Durbin-Watson test 
# library(lmtest)
dwtest(lm.full)

# 5. Check for Multicollinearity ----
# Variance Inflation Factors (VIF)
#library(car)
vif(lm.full)  # Values > 5 or 10 might indicate problematic multicollinearity

# 6. Check for Influential Outliers ----
# Cook's distance plot
plot(lm.full, which = 4)
abline(h = 4/(nrow(data_hypothesis_ancova)-length(lm.full$coefficients)-2), col = "red")

# Leverage plot
plot(hatvalues(lm.full))

# all the diagnostic plots at once, 
par(mfrow=c(2,2))
plot(lm.full)



# Exploration using using avo 
ancova_result_h1 <- aov(scale_bipost ~ id_scenario + age + sex + income + politics, 
                        data = data_hypothesis1)

# Output the results
summary(ancova_result_h1)
# None of the factors or covariates in the model have a statistically significant effect on the dependent variable




######################### H2: Mediation Analysis #####################################
# Purpose: Test if identification with humanity mediates the effect of leadership language on behavior intentions. 

# H2.0 Preparing ----

data_hypothesis2 <- data_narm %>%
  dplyr::select(id_scenario, scale_bipost, scale_iwah) %>%
  drop_na() %>% # exclude NAs 
  mutate(id_scenario = factor(id_scenario)) # Convert the id_scenario column to a factor

# set  contrasts for the id_scenario variable.
contrasts(data_hypothesis2$id_scenario) <- contr.treatment(n = 2, base = 1) # This sets the first level of the factor as the reference or baseline category. In the context of statistical analysis, this means that the effects of the second level of id_scenario will be compared against the first level.


# Change the labels of the factor levels
levels(data_hypothesis2$id_scenario) <- c("ILI", "non_ILI")


# H2.1 Test: Conduct mediation analysis using Baron & Kenny approach----
# Useful for theoretical models where the mediator is expected to be the primary channel of influence

# Load necessary library
library(lmtest)

# Step 1: Test IV -> M
# Model: scale_iwah ~ id_scenario
model1 <- lm(scale_iwah ~ id_scenario, data = data_hypothesis2)
summary(model1)
# p = 0.16 n.s


# Step 2: Test IV -> DV
# Model: scale_bipost ~ id_scenario
model2 <- lm(scale_bipost ~ id_scenario, data = data_hypothesis2)
summary(model2)
# p = 0.902 n.s 


# Step 3: Test IV + M -> DV
# Model: scale_bipost ~ id_scenario + scale_iwah
model3 <- lm(scale_bipost ~ id_scenario + scale_iwah, data = data_hypothesis2)
summary(model3)
# id_scenario2 is -0.04625, p = 0.80054 --> n.s.
# scale_iwah is 0.27113, p = 0.00734 --> significant (does predict bi)
# but. # Mediation is present if the coefficient of leadership_language in model3 is less significant than in model2
# --> n.s. 


## Option 2 - ----

# https://uedufy.com/how-to-run-mediation-analysis-in-r/

# Install and load the lavaan package
# install.packages("lavaan")
library(lavaan)

mediation_model <- '
  # Direct effects
  scale_iwah ~ a * id_scenario
  scale_bipost ~ c * id_scenario + b * scale_iwah

  # Indirect effect (a * b)
  indirect := a * b

  # Total effect (c + indirect)
  total := c + indirect
'

# Estimate the mediation model
mediation_results <- sem(mediation_model, data = data_narm)

# Summarize the results
summary(mediation_results, standardized = TRUE, fit.measures = TRUE)

# perfect fit: CFI and TLI values of 1.000 and an RMSEA and SRMR of 0.000
# just-identified model has exactly enough parameters to perfectly predict the data, resulting in zero degrees of freedom.
# limited ability to generalize beyond current dataset. 

#Direct Effects:
  # The effect of id_scenario on scale_iwah (path a) is not significant (p = 0.153).
  # The effect of id_scenario on scale_bipost (path c) is also not significant (p = 0.797).
  # The effect of scale_iwah on scale_bipost (path b) is significant (p = 0.005).
  
# Indirect Effect:
# The indirect effect (a*b) of id_scenario on scale_bipost through scale_iwah is not significant (p = 0.204).

# Total Effect:
# The total effect (c + indirect) of id_scenario on scale_bipost is not significant (p = 0.901).

# mediator (scale_iwah) significantly influences the dv (scale_bipost), 
# the independent variable (id_scenario) no significant direct or indirect effect on the dv. 
# suggests that the mechanism by which id_scenario influences scale_bipost is not captured by scale_iwah in this model.


# Option 3 ----

install.packages("processR")
library(processR)


# Run the mediation analysis
results <- process(data = data_narm, 
                   model = 4, 
                   x = "id_scenario", 
                   y = "scale_bipost", 
                   m = "scale_iwah", 
                   boot = 5000)

# View the results
print(results)

update.packages(ask = FALSE, checkBuilt = TRUE)
# https://cran.r-project.org/web/packages/processR/index.html

# Package ‘processR’ was removed from the CRAN repository.
# Formerly available versions can be obtained from the archive.
# Archived on 2023-02-02 as requires archived package 'predict3d'.
# A summary of the most recent check results can be obtained from the check results archive.
# Please use the canonical form https://CRAN.R-project.org/package=processR to link to this page.

# R.version.string
# 
# install.packages("/Users/oliverhoffmann/Documents/1. UNI/Psychologie/Bachelor /Material/5.) WiSe 22:23/Bachelorarbeit - PsyBSc 21/Datenanalyse/Bachelor Datenauswertung_/processR_0.2.7.tar.gz", repos = NULL, type = "source")
# 
# 
# if(!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
# devtools::install_version("processR", version = "0.2.7", repos = "https://cran.r-project.org/src/contrib/Archive/processR/")
# 
# install.packages("https://cran.r-project.org/src/contrib/Archive/predict3d/predict3d_x.x.x.tar.gz", repos = NULL, type = "source")
# devtools::install()
# install.packages("processR", repos = NULL, type = "source")



######################### H3: Moderated Regression Analysis ##########################

# Purpose: Examine if environmental concern moderates the relationship between identification with humanity and behavior intentions.
  # exploring if the effect of identification with humanity on pro-environmental behavior intentions changes at different levels of environmental concern
# H3.0 Preparing data ----

# Create a new dataframe for hypothesis 3
data_hypothesis3 <- data_narm %>% 
  # Convert the id_scenario column to a factor
  mutate(id_scenario = factor(id_scenario)) %>%
  dplyr::select(id_scenario, scale_bipost, scale_iwah, scale_ec) %>%
  drop_na()  # exclude NAs 

#set  contrasts for the id_scenario variable.
contrasts(data_hypothesis3$id_scenario) <- contr.treatment(n = 2, base = 1) # This sets the first level of the factor as the reference or baseline category. In the context of statistical analysis, this means that the effects of the second level of id_scenario will be compared against the first level.


# Change the labels of the factor levels
levels(data_hypothesis3$id_scenario) <- c("ILI", "non_ILI") # This sets the first level of the factor as the reference or baseline category. In the context of statistical analysis, this means that the effects of the second level of id_scenario will be compared against the first level.


# H3.1 Test: Perform moderated regression analysis to check the significance of the interaction term. ---- 

# Option 1
data_hypothesis3$interaction <- data_hypothesis3$scale_ec * data_hypothesis3$scale_iwah

moderated_model_h3 <- lm(scale_bipost ~ scale_ec + scale_iwah + interaction, data = data_hypothesis3)
summary(moderated_model_h3)

# Option 2
standardized_model_h3 <- lm(scale(scale_bipost) ~ scale(scale_ec) * scale(scale_iwah), data = data_hypothesis3)
summary(standardized_model_h3)



# Coefficients: Estimates the effect of each predictor on the dependent variable.
  # scale_ec: Positive, marginally significant effect (p = 0.0589).
  # scale_iwah: Positive, marginally significant effect (p = 0.0788).
  # interaction: Negative effect, not statistically significant (p = 0.1910).
# Model Fit:
  # Multiple R-squared of 0.1992, - 19.92% of the variance in scale_bipost is explained by the model.
# F-statistic  8.292  p-value = 0.00005583 significant

# While the overall model is significant, none of the individual predictors, including the interaction term, are significant at the 5% level. 
  # while the model can explain some variability in scale_bipost, the specific contributions of scale_iwah, scale_ec, and their interaction are not statistically significant.
# The lack of significance for the interaction term suggests that scale_ec may not moderate the relationship between scale_iwah and scale_bipost 




# H3.2 Assumptions: Linearity, independence, homoscedasticity, no multicollinearity. ----

# independence: by desgin
dwtest(standardized_model_h3)

cooks_distances_h3 <- cooks.distance(standardized_model_h3)

# Plot Cook's distance
plot(cooks_distances_h3, pch=19, main="Cook's Distance", type="h")
abline(h = 4/(nrow(data_hypothesis3)-length(standardized_model_h3$coefficients)-2), col = "blue")


## 9.2.1 no multicollinearity ----
vif(standardized_model_h3)  # Multicollinearity check
vif_results_h3 <- vif(standardized_model_h3)
print(vif_results_h3)

# A common rule of thumb is that a VIF greater than 5 or 10 indicates a problematic amount of collinearity.
# multicollinearity present


# Tolerance values can be calculated as the inverse of VIF
tolerance_values <- 1 / vif_results_h3
print(tolerance_values)


## H3.2.2 Linearity ----
plot(standardized_model_h3)

avPlots(moderated_model_h3)
avPlots(standardized_model_h3)


plot(data$scale_iwah, data$scale_bipost)

plot(standardized_model_h3$fitted.values, residuals(standardized_model_h3), 
     xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")




## H3.2.3 homoscedasticity ----

car::residualPlot(standardized_model_h3, pch = 16, linear = TRUE)
# Non-Constant Error Variance Test für Unabhängigkeit der Residuen
car::ncvTest(standardized_model_h3)

# 
plot(fitted(standardized_model_h3), residuals(standardized_model_h3))
abline(h = 0, col = "blue")

# not appropriate 
# leveneTest(scale_bipost ~ id_scenario, data = data_hypothesis3)
# # not significant

## independance of residuals 
library(lmtest)
dwtest(moderated_model_h3)


## H3.2.4 Normality of Residuals ----

qqnorm(residuals(standardized_model_h3))
qqline(residuals(standardized_model_h3))
shapiro.test(residuals(standardized_model_h3))
# p-value = 0.3195



# H3.3 Plot: Interaction plot showing the relationship at different levels of the moderator. ----


pacman::p_load(
  here, tidyverse, showtext, sysfonts,
  broom, interactions, scico, jtools,
  gtsummary, gt, flextable, wesanderson,
  colorspace, qqplotr, rstatix, stats
)


# data frame
model_data_test <- data_narm %>%
  dplyr::select(id_scenario, scale_bipost, scale_iwah, scale_ec) %>%
  mutate(id_scenario = factor(id_scenario),
         ec_category = cut(scale_ec, breaks = quantile(scale_ec, probs = c(0, 1/3, 2/3, 1)), 
                           labels = c("Low", "Middle", "High"), include.lowest = TRUE))

# Set contrasts for the id_scenario variable
contrasts(model_data_test$id_scenario) <- contr.treatment(n = 2, base = 1)

# Create a regression model with the new ec_category
formula_lm_test <- formula(scale_bipost ~ scale_iwah * ec_category + id_scenario)
reg_mod_h3_test <- lm(formula = formula_lm_test, data = model_data_test)
tidy(reg_mod_h3_test)
summary(reg_mod_h3_test)

# Plot interaction
interact_plot(
  model = reg_mod_h3_test,
  pred  = scale_iwah,
  modx = ec_category, 
  centered = TRUE,
  interval = TRUE,
  int.type = "confidence", 
  int.width = .95,
  plot.points = TRUE,
  colors = wes_palette("Zissou1"),
  vary.lty = FALSE,
  point.alpha = 0.4,
  point.size = 1.5
) + 
  scale_x_continuous(name = "Identification with Humanity") +
  scale_y_continuous(name = "Pro-environmental Behavior Intentions") +
  labs(#title = "Exploring the Moderating Role of Environmental Concern",
       #subtitle = "Interaction Between IWAH and EC on Pro-environmental Behavior",
       color = "Environmental Concern ") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"),
        # legend.position = "none",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))



######################### Correlation Calculations##############################
# Assumptions: 
## Pairwise deletion of missing data 
## variables should be measured on an interval or ratio scale 
## linear relationship between the variables


# preparing data for cor. testing
cor_data <- data_narm %>%
  dplyr:: select(starts_with("scale")) %>%
  dplyr:: select(-"scale_ili", "scale_bipost")%>%
  drop_na()

#correlation table
cor_table <- cor_data %>%
  cor_mat(conf.level = 0.95) %>% #confidence level of 0.95.
  #marking the significant correlations by using cutpoints and symbols
  cor_mark_significant(
    cutpoints = c(0, 0.001, 0.01, 0.05, 1),
    symbols = c("***", "**", "*", "")
  ) # %>%
  # as_tibble() %>% # converting table as tibble
  # rename(Scale = rowname) %>%
  # mutate(Scale = str_remove(Scale, "scale_")) %>%
  # rename_with(~str_remove(.x, "scale_")) %>%
  # flextable() %>% # formating table
  # theme_apa()

print(cor_table)


# Assuming cor_data is your correlation data frame
fancy_cor_plot <- cor_data %>% 
  correlate() %>% 
  stretch() %>%
  mutate(across(
    .cols = c(x, y),
    .fns = ~case_when(
      .x == "scale_bipre" ~ "Pro-environmental Behavior Intentions",
      .x == "scale_bipost" ~ "Post Pro-environmental Behavior Intentions",
      .x == "scale_ec" ~ "Environmental Concern",
      .x == "scale_cd" ~ "Climate Denial",
      .x == "scale_iwah" ~ "Identification with Humanity",
      TRUE ~ .x
    )
  )) %>% 
  graph_from_data_frame(directed = FALSE) %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(
    edge_alpha = abs(r), 
    edge_width = abs(r),
    color = r
  )) +
  geom_node_point(fill = "grey90", size = 3, shape = 21, color = "grey70") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width_continuous(
    name = "Pearson *r* (abs)",
    breaks = c(0.1, 0.3, 0.5),
    range = c(0.6, 2)
  ) +
  scale_edge_alpha_continuous(
    name = "Pearson *r* (abs)", 
    breaks = c(0.1, 0.3, 0.5),
    range = c(0.3, 1)
  ) +
  scale_edge_colour_gradientn(
    name = "Pearson *r*",
    limits = c(-1, 1), 
    colors = scico::scico(n = 20, palette = "vikO", direction = -1)
  ) +
  theme_graph() +
  theme(
    text = element_text(size = 11)
    # Removed element_markdown()
  )

fancy_cor_plot


#### Post hoc power analysis -----

library(pwr)

mean_pre <- mean(data_narm$scale_bipre) # the mean of pre-test scores
mean_post <- mean(data_narm$scale_bipost)# the mean of post-test scores
sd_pre <- sd(data_narm$scale_bipre) # the standard deviation of pre-test scores
sd_post <- sd(data_narm$scale_bipost) # the standard deviation of post-test scores
n <- 104 # number of participants

# Calculate the standard deviation of differences
sd_diff <- sqrt((sd_pre^2 + sd_post^2)/2)

# Calculate the effect size (Cohen's d for paired samples)
d  <- (mean_post - mean_pre) / sd_diff


# Calculate power
pwr.t.test(d = d, n = n, sig.level = 0.05, type = "paired", alternative = "two.sided")$power
# 0.8429676




#--------------------------------------Ende------------------------------------#
