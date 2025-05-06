# Load required library
library(readxl)
library(vcd)
library(irr)
library(knitr)
library(ciTools)
library(kableExtra)


#excel_file <- "C:\\Users\\Default\\Desktop\\Efe\\VCTEvsMRE_103cases_Oct24.xlsx"
excel_file <- "C:\\Users\\Default\\Desktop\\Efe\\VCTEvsMRE_103cases_Jan21_25.xlsx"
df <- read_excel(excel_file)

##################################
# Perform a t-test to compare BMI between the two groups
t_test_result <- t.test(BMI ~ agreement, data = df)
sd_bmi <- tapply(df$BMI, df$agreement, sd)
print(t_test_result)
print(sd_bmi)


# Perform a t-test to compare age between the two groups
t_test_result <- t.test(Age ~ agreement, data = df)
sd_age <- tapply(df$Age, df$agreement, sd)
print(t_test_result)
print(sd_age)


# Perform a t-test to compare weight between the two groups
t_test_result <- t.test(weight ~ agreement, data = df)
sd_weight <- tapply(df$weight, df$agreement, sd)
print(t_test_result)
print(sd_weight)


model_age <- glm(agreement ~ Age, data = df, family = binomial)
summary(model_age)

# Odds ratio (OR) for Age
or_age <- exp(coef(model_age))

# 95% Confidence Interval (CI) for Age OR
ci_age <- exp(confint(model_age))

# Display OR and CI for Age
or_age
ci_age

# Repeat for other variables (Sex, BMI, Weight)

# Univariable logistic regression for Sex
model_sex <- glm(agreement ~ Sex, data = df, family = binomial)
summary(model_sex)

# Odds ratio (OR) for Sex
or_sex <- exp(coef(model_sex))

# 95% Confidence Interval (CI) for Sex OR
ci_sex <- exp(confint(model_sex))

# Display OR and CI for Sex
or_sex
ci_sex

# Univariable logistic regression for BMI
model_bmi <- glm(agreement ~ BMI, data = df, family = binomial)
summary(model_bmi)

# Odds ratio (OR) for BMI
or_bmi <- exp(coef(model_bmi))

# 95% Confidence Interval (CI) for BMI OR
ci_bmi <- exp(confint(model_bmi))

# Display OR and CI for BMI
or_bmi
ci_bmi

# Univariable logistic regression for Weight
model_weight <- glm(agreement ~ weight, data = df, family = binomial)
summary(model_weight)

# Odds ratio (OR) for Weight
or_weight <- exp(coef(model_weight))

# 95% Confidence Interval (CI) for Weight OR
ci_weight <- exp(confint(model_weight))

# Display OR and CI for Weight
or_weight
ci_weight


### for each category #####
TE<-df$`TE: (F0-1< 8.2, F2<=9.7, F3 <=13.6, F4>13.6)`
MRE <-df$`MRE: (F0-1<3.14, F2<3.53, F3<4.45, F4>=4.45)`
# Create a data frame
data <- data.frame(TE = factor(TE, levels = c("F0-1", "F2", "F3", "F4")),
                   MRE = factor(MRE, levels = c("F0-1", "F2", "F3", "F4")))

# Create a 4x4 contingency table
contingency_table <- table(data$TE, data$MRE)

# Print the contingency table
print(contingency_table)

# Compute Cohen's Kappa
kappa_result <- kappa2(data)

# Print the Kappa result
print(kappa_result)

###########  for F0-F1 vs F2-F4 ##################
contingency_table <- table(df$`MRE: F0-F1 vs F2-F4`,df$`TE: F0-F1 vs F2-F4`)
print(addmargins(contingency_table))
# Perform Fisher exact test
fisher_test_result <- fisher.test(contingency_table)
# Print the result
print(fisher_test_result)

# Perform Cohen's kappa
data_df <- data.frame(MRE = df$`MRE: F0-F1 vs F2-F4`,TE = df$`TE: F0-F1 vs F2-F4`)
data_matrix <- as.matrix(data_df)
kappa_result <- kappa2(data_matrix)
# Print the result
print("Cohen's Kappa Result:")
print(kappa_result)


# Perform McNemar's test
mcnemar_test_result <- mcnemar.test(df$`MRE: F0-F1 vs F2-F4`,df$`TE: F0-F1 vs F2-F4`)
# Print the result
print("McNemar's Test Result:")
print(mcnemar_test_result)



################ F0-F2 vs F3-F4 ########################
contingency_table <- table(df$`MRE: F0-F2 vs F3-F4`, df$`TE: F0-F2 vs F3-F4`)
print(addmargins(contingency_table))

# Perform Fisher exact test
fisher_test_result <- fisher.test(contingency_table)
# Print the result
print(fisher_test_result)

# Perform Cohen's kappa
data_df <- data.frame(MRE = df$`MRE: F0-F2 vs F3-F4`, TE = df$`TE: F0-F2 vs F3-F4`)
data_matrix <- as.matrix(data_df)
kappa_result <- kappa2(data_matrix)
# Print the result
print("Cohen's Kappa Result:")
print(kappa_result)

# Perform McNemar's test
mcnemar_test_result <- mcnemar.test(df$`MRE: F0-F2 vs F3-F4`, df$`TE: F0-F2 vs F3-F4`)
# Print the result
print("McNemar's Test Result:")
print(mcnemar_test_result)




################# F0-F3 vs F4######################
contingency_table <- table(df$`MRE: F0-F3 vs F4`, df$`TE: F0-F3 vs F4`)
print(addmargins(contingency_table))

# Perform Fisher exact test
fisher_test_result <- fisher.test(contingency_table)
# Print the result
print(fisher_test_result)

# Perform Cohen's kappa
data_df <- data.frame(MRE = df$`MRE: F0-F3 vs F4`, TE = df$`TE: F0-F3 vs F4`)
data_matrix <- as.matrix(data_df)
kappa_result <- kappa2(data_matrix)
# Print the result
print("Cohen's Kappa Result:")
print(kappa_result)

# Perform McNemar's test
mcnemar_test_result <- mcnemar.test(df$`MRE: F0-F3 vs F4`, df$`TE: F0-F3 vs F4`)
# Print the result
print("McNemar's Test Result:")
print(mcnemar_test_result)



###################### F2-F3#######################
contingency_table <- table(df$`MRE: F2 or F3`, df$`TE: F2 or F3`)
print(addmargins(contingency_table))

# Perform Fisher exact test
fisher_test_result <- fisher.test(contingency_table)
# Print the result
print(fisher_test_result)

# Perform Cohen's kappa
data_df <- data.frame(MRE = df$`MRE: F2 or F3`, TE = df$`TE: F2 or F3`)
data_matrix <- as.matrix(data_df)
kappa_result <- kappa2(data_matrix)
# Print the result
print("Cohen's Kappa Result:")
print(kappa_result)

# Perform McNemar's test
mcnemar_test_result <- mcnemar.test(df$`MRE: F2 or F3`, df$`TE: F2 or F3`)
# Print the result
print("McNemar's Test Result:")
print(mcnemar_test_result)

library(dplyr)

df %>%
  group_by(`TE: F4`) %>% # Group by the binary column
  summarise(
    mean_value = mean(`LSM on TE`, na.rm = TRUE), # Calculate mean
    std_dev = sd(`LSM on TE`, na.rm = TRUE),     # Calculate standard deviation
    min_value = min(`LSM on TE`, na.rm = TRUE),  # Calculate minimum
    max_value = max(`LSM on TE`, na.rm = TRUE)   # Calculate maximum
  )

