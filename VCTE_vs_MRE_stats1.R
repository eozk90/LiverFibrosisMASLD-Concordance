library(readxl)
library(ggplot2)
library(ggpubr)
library(vcd)
library(irr)
library(knitr)
library(ciTools)
library(kableExtra)
library(pROC)

excel_file <- "C:\\Users\\Default\\Desktop\\Efe\\VCTEvsMRE_103cases_Jan21_25.xlsx"
# Read the Excel file
data <- read_excel(excel_file)
# View the first few rows of the data
head(data)
column_names <- colnames(data)
print(column_names)
# Check the structure of your data
str(data)


########################### correlation of PDFF vs CAP ###########################
# Assuming your columns are named "X" and "Y"
data_filtered <- na.omit(data[, c("CAP", "Fat fraction (PDFF %)")])

# Extracting the columns without NA values
X <- data_filtered$CAP
Y <- data_filtered$`Fat fraction (PDFF %)`
#X <- data$CAP
#Y <- data$`Fat fraction (PDFF %)`

# Calculate the Spearman correlation coefficient
correlation <- cor(X, Y, method = "spearman")

# Calculate Spearman correlation coefficient and its p-value
correlation_result <- cor.test(X, Y, method = "spearman")
# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_result$estimate
p_value <- correlation_result$p.value
# Print correlation coefficient and p-value
print(paste("Spearman correlation coefficient:", correlation_coefficient))
print(paste("P-value:", p_value))


# Create a data frame
df <- data.frame(CAP = X, PDFF = Y)

# Fit a linear model
fit <- lm(PDFF ~ CAP, data = df)
fit_eq <- as.character(as.expression(paste("PDFF =", round(fit$coefficients[2], 4), "x CAP", round(fit$coefficients[1], 4))))

# Scatter plot with line of best fit
plot <- ggplot(df, aes(x = CAP, y = PDFF)) +
  geom_point() +                  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    # Add line of best fit
  labs(
    title = "Scatter Plot with Line of Best Fit",
    x = "CAP (dB/m)",
    y = "PDFF (%)"
  ) +
  annotate(
    "text",
    x = min(df$CAP, na.rm = TRUE) + 5,
    y = min(df$PDFF, na.rm = TRUE) + 31,
    label = paste("Spearman Correlation(r):", round(correlation, 2)),
    hjust = 0,
    vjust = 0,
    color = "black",
    size = 4, # Set the size of the annotation
    family = "Arial", # Set the font family
    
  ) +
  annotate(
    "text",
    x = min(df$CAP, na.rm = TRUE) + 5,
    y = min(df$PDFF, na.rm = TRUE) + 33,
    label = paste("Fit Equation:", fit_eq),
    hjust = 0,
    vjust = 0,
    color = "black",
    size = 4, # Set the size of the annotation
    family = "Arial", # Set the font family
    
  )+
  theme(
    axis.title.x = element_text(family = "Arial", size = 16), # X axis title
    axis.title.y = element_text(family = "Arial", size = 16),  # Y axis title
    axis.text.x = element_text(family = "Arial", size = 16),   # X axis tick labels
    axis.text.y = element_text(family = "Arial", size = 16)    # Y axis tick labels
  )

# Print the plot
print(plot)

 

########################### correlation of LSM from MRE vs TE for IQR/MED<30% ###########################
#exclude poor quality Fibroscan data
data <- data[data$`IQR/med` <= 30, ]
print(data)
X <- data$`LSM on TE`
Y <- data$`LSM on MRE`

# Calculate Spearman correlation coefficient and its p-value
correlation_result <- cor.test(X, Y, method = "spearman")
# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_result$estimate
p_value <- correlation_result$p.value
# Print correlation coefficient and p-value
print(paste("Spearman correlation coefficient:", correlation_coefficient))
print(paste("P-value:", p_value))


# Create a data frame
df <- data.frame(TE = X, MRE = Y)

# Fit a linear model
fit <- lm(MRE ~ TE, data = df)
fit_eq <- as.character(as.expression(paste("MRE =", round(fit$coefficients[2], 4), "x VCTE +", round(fit$coefficients[1], 4))))

# Scatter plot with line of best fit
plot <- ggplot(df, aes(x = TE, y = MRE)) +
  geom_point() +                  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    # Add line of best fit
  labs(
    title = "Scatter Plot with Line of Best Fit",
    x = "VCTE (kPa)",
    y = "MRE (kPa)"
  ) +
  annotate(
    "text",
    x = 5,
    y = 9.2,
    label = paste("Spearman Correlation(r):", round(correlation, 2)),
    hjust = 0,
    vjust = 0,
    color = "black",
    size = 5, # Set the size of the annotation
    family = "Arial", # Set the font family
    
  ) +
  annotate(
    "text",
    x = 5,
    y = 9.6,
    label = paste("Fit Equation:", fit_eq),
    hjust = 0,
    vjust = 0,
    color = "black",
    size = 5, # Set the size of the annotation
    family = "Arial", # Set the font family
    
  )+
  
  theme(
    axis.title.x = element_text(family = "Arial", size = 16), # X axis title
    axis.title.y = element_text(family = "Arial", size = 16),  # Y axis title
    axis.text.x = element_text(family = "Arial", size = 16),   # X axis tick labels
    axis.text.y = element_text(family = "Arial", size = 16)    # Y axis tick labels
  )
  

# Print the plot
print(plot)


##################################freq plots#####################################
excel_file <- "C:\\Users\\Default\\Desktop\\Efe\\VCTEvsMRE_103cases_Jan21_25.xlsx"
data <- read_excel(excel_file)
# Create frequency table for "PDFF"
pdff_freq <- table(data$`PDFF Steatosis (S0: <6, S1: 6-<=17, S2: 17-<=22, S3: >22)`)
pdff_freq_df <- as.data.frame(pdff_freq)
pdff_freq_df$Category <- pdff_freq_df$Var1

# Create frequency table for "CAP"
cap_freq <- table(data$`CAP Steatosis (S1: 249-268, S2: 269-280, S3: >280)`)
cap_freq_df <- as.data.frame(cap_freq)
cap_freq_df$Category <- cap_freq_df$Var1

# Combine data frames
pdff_freq_df$Source <- "PDFF"
cap_freq_df$Source <- "CAP"
combined_df <- rbind(pdff_freq_df, cap_freq_df)

# Plot
ggplot(combined_df, aes(x = Category, y = Freq, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Freq), vjust = -0.3, size = 4, family = "Arial") +
  labs(
    title = "Frequency of Categories based on PDFF and CAP",
    x = "Category",
    y = "Count"
  ) +
  scale_x_discrete(limits = c("S0", "S1", "S2", "S3")) +
  scale_fill_manual(values = c("PDFF" = "skyblue", "CAP" = "lightgreen")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(family = "Arial", size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(family = "Arial", size = 14),
    axis.title = element_text(family = "Arial", size = 14),
    legend.text = element_text(family = "Arial", size = 14),
    legend.title = element_text(family = "Arial", size = 14)
  ) +
  facet_wrap(~ Source, nrow = 1)  # Facet by "Source" (PDFF and CAP) in one row



############################################Contingency tables#####################################
# Create the contingency table and perform McNemar's test
cont_table <- table(data$`CAP Steatosis (S1: 249-268, S2: 269-280, S3: >280)`, data$`PDFF Steatosis (S0: <6, S1: 6-<=17, S2: 17-<=22, S3: >22)`)
colnames(cont_table) <- c("PDFF S0", "PDFF S1","PDFF S2","PDFF S3")
rownames(cont_table) <- c("CAP S0", "CAP S1","CAP S2","CAP S3")
print(cont_table)
mcnemar_result <- mcnemar.test(cont_table)
print(mcnemar_result)
# Calculate Cohen's Kappa
data_df <- data.frame(PDFF = data$`PDFF Steatosis (S0: <6, S1: 6-<=17, S2: 17-<=22, S3: >22)`, CAP = data$`CAP Steatosis (S1: 249-268, S2: 269-280, S3: >280)`)
data_matrix <- as.matrix(data_df)
kappa_result <- kappa2(data_matrix)
print(kappa_result)



#exclude poor quality Fibroscan data
data <- data[data$`IQR/med` <= 30, ]

# Create the first 2x2 contingency table and perform McNemar's Test
cont_table <- table(data$`TE: F0-F1 vs F2-F4`, data$`MRE: F0-F1 vs F2-F4`)
colnames(cont_table) <- c("MRE F0-F1", "MRE F2-F4")
rownames(cont_table) <- c("TE F0-F1", "TE F2-F4")
print(cont_table)
result1 <- mcnemar.test(cont_table)
print(result1)
# Calculate Cohen's Kappa
data_df <- data.frame(MRE = data$`MRE: F0-F1 vs F2-F4`, TE = data$`TE: F0-F1 vs F2-F4`)
data_matrix <- as.matrix(data_df)
kappa_result <- kappa2(data_matrix)
print(kappa_result)


# Create the first 2x2 contingency table and perform McNemar's Test
cont_table <- table(data$`TE: F0-F2 vs F3-F4`, data$`MRE: F0-F2 vs F3-F4`)
colnames(cont_table) <- c("MRE F0-F2", "MRE F3-F4")
rownames(cont_table) <- c("TE F0-F2", "TE F3-F4")
print(cont_table)
result1 <- mcnemar.test(cont_table)
print(result1)
# Calculate Cohen's Kappa
data_df <- data.frame(MRE = data$`MRE: F0-F2 vs F3-F4`, TE = data$`TE: F0-F2 vs F3-F4`)
data_matrix <- as.matrix(data_df)
kappa_result <- kappa2(data_matrix)
print(kappa_result)

# Create the first 2x2 contingency table and perform McNemar's Test
cont_table2 <- table(data$`TE: F0-F3 vs F4`, data$`MRE: F0-F3 vs F4`)
colnames(cont_table2) <- c("MRE F0-F3", "MRE F4")
rownames(cont_table2) <- c("TE F0-F3", "TE F4")
print(cont_table2)
result2 <- mcnemar.test(cont_table2)
print(result2)
# Calculate Cohen's Kappa
data_df_2 <- data.frame(MRE = data$`MRE: F0-F3 vs F4`, TE = data$`TE: F0-F3 vs F4`)
data_matrix_2 <- as.matrix(data_df_2)
kappa_result_2 <- kappa2(data_matrix_2)
print(kappa_result_2)


###########MWU test combined with ROC#############
perform_mann_whitney_roc <- function(data, group_column, parameter_indices, predictor_indices, response) {
  
  
  # Create an empty data frame to store results
  results_df <- data.frame(
    Parameter = character(),
    Test = character(),
    Mean_Group_0 = numeric(),
    Mean_Group_1 = numeric(),
    SD_Group_0 = numeric(),
    SD_Group_1 = numeric(),
    Count_Group_0 = numeric(),
    Count_Group_1 = numeric(),
    P_Value = numeric(),
    AUC = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    Cutoff = numeric(),
    Sens = numeric(),
    Spec = numeric(),
    stringsAsFactors = FALSE
  )
  
  data$binary_response <- as.factor(ifelse(data[[response]] == 1, 1, 0))
  
  # Loop through each parameter index
  for (param_index in parameter_indices) {
    param_name <- colnames(data)[param_index]
    
    predictor <- colnames(data)[param_index]
    
    data_filtered <- na.omit(data[complete.cases(data[[predictor]], data[[response]]), 
                                  c(predictor, response, "binary_response")])
    
    # Perform ROC analysis
    ci_result <- ci.auc(data_filtered$binary_response,as.numeric(data_filtered[[predictor]]))
    roc_results <- roc(data_filtered$binary_response,as.numeric(data_filtered[[predictor]]))
    YoudenJindex = which.max(abs(as.array(unlist(roc_results$sensitivities))+as.array(unlist(roc_results$specificities))-1))
    Sensitivity = roc_results$sensitivities[YoudenJindex]
    Specificity = roc_results$specificities[YoudenJindex]
    Threshold = roc_results$thresholds[YoudenJindex]
    
    
    
    # Separate data into groups based on CPSH (0 and 1)
    group_0 <- data[data[[group_column]] == 0, ]
    group_1 <- data[data[[group_column]] == 1, ]
    
    # Calculate means and standard deviations for each group
    mean_group_0 <- mean(group_0[[param_name]], na.rm = TRUE)
    mean_group_1 <- mean(group_1[[param_name]], na.rm = TRUE)
    sd_group_0 <- sd(group_0[[param_name]], na.rm = TRUE)
    sd_group_1 <- sd(group_1[[param_name]], na.rm = TRUE)
    
    # Get data count for each group
    count_group_0 <- sum(!is.na(group_0[[param_name]]))
    count_group_1 <- sum(!is.na(group_1[[param_name]]))
    
    # Perform Mann-Whitney U test
    test_result <- wilcox.test(data[[param_name]] ~ data[[group_column]])
    
    # Store results in the data frame
    results_df <- rbind(results_df, data.frame(
      Parameter = param_name,
      Test = "MWU",
      Count_Group_0 = count_group_0,
      Mean_Group_0 = mean_group_0,
      SD_Group_0 = sd_group_0,
      Count_Group_1 = count_group_1,
      Mean_Group_1 = mean_group_1,
      SD_Group_1 = sd_group_1,
      P_Value = test_result$p.value,
      AUC = round(ci_result[2],digits=3),
      CI_Lower = round(ci_result[1],digits=3),
      CI_Upper = round(ci_result[3],digits=3),
      Cutoff = Threshold,
      Sens = round(Sensitivity,digits=3),
      Spec = round(Specificity,digits=3)
    ))
  }
  
  # Print the results table
  kable(results_df, format = "markdown")
}

### stats for classifying steatosis severity based on CAP
group_column <- "Mild steatosis (PDFF: >=6-17)"
parameter_indices <- c(7)
response <- "Mild steatosis (PDFF: >=6-17)"
predictor_indices <- c(7)
perform_mann_whitney_roc(data, group_column, parameter_indices, predictor_indices, response)
# Create ROC curve
roc_obj_mild <- roc(data$`Mild steatosis (PDFF: >=6-17)`, data$CAP)


group_column <- "Moderate steatosis (PDFF >=17-<=22)"
parameter_indices <- c(7)
response <- "Moderate steatosis (PDFF >=17-<=22)"
predictor_indices <- c(7)
perform_mann_whitney_roc(data, group_column, parameter_indices, predictor_indices, response)
roc_obj_moderate <- roc(data$`Moderate steatosis (PDFF >=17-<=22)`, data$CAP)

group_column <- "Severe steatosis (PDFF > 22)"
parameter_indices <- c(7)
response <- "Severe steatosis (PDFF > 22)"
predictor_indices <- c(7)
perform_mann_whitney_roc(data, group_column, parameter_indices, predictor_indices, response)
roc_obj_severe<- roc(data$`Severe steatosis (PDFF > 22)`, data$CAP)

# Plot ROC curve
plot(roc_obj_mild, main = "ROC Curve for Mild Steatosis", col = "blue")
# Freeze the current plot
par(new = TRUE)
roc_obj_moderate <- roc(data$`Moderate steatosis (PDFF >=17-<=22)`, data$CAP)
# Plot ROC curve
plot(roc_obj_moderate, main = "ROC Curve for Moderate Steatosis", col = "green")
par(new = TRUE)
roc_obj_severe <- roc(data$`Severe steatosis (PDFF > 22)`, data$CAP)
# Plot ROC curve
plot(roc_obj_severe, main = "ROC Curve for Severe Steatosis", col = "red")

##########################comibned ROC plots#######################################



# Plot ROC curve for Mild Steatosis
plot(roc_obj_mild, main = "ROC Curves for Steatosis", col = "blue", lty = 1, lwd = 2)
legend("bottomright", legend = "Mild Steatosis", col = "blue", lty = 1, lwd = 2, bty = "n", xpd = TRUE, inset = c(0.05, 0))

# Freeze the current plot
par(new = TRUE)

# Plot ROC curve for Moderate Steatosis
plot(roc_obj_moderate, main = "", col = "green", lty = 1, lwd = 2)
legend("bottomright", legend = "Moderate Steatosis", col = "green", lty = 1, lwd = 2, bty = "n", xpd = TRUE, inset = c(0, 0.05))

# Freeze the current plot
par(new = TRUE)

# Plot ROC curve for Severe Steatosis
plot(roc_obj_severe, main = "", col = "red", lty = 3, lwd = 2)
legend("bottomright", legend = "Severe Steatosis", col = "red", lty = 3, lwd = 2, bty = "n", xpd = TRUE, inset = c(0, 0.1))



#######################error box plots######################################
excel_file <- "C:\\Users\\Default\\Desktop\\Efe\\VCTEvsMRE_103cases_Jan21_25.xlsx"
# Read the Excel file
df <- read_excel(excel_file)

ggplot(df, aes(x = `TE: (F0-1< 8.2, F2<=9.7, F3 <=13.6, F4>13.6)`, y = `LSM on TE`)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 8, size = 4, color = "red") +  # Add mean as asterisks
  labs(title = "Box Plot of Liver Stiffness by Fibrosis Stage",
       x = "Fibrosis Stage",
       y = "Liver Stiffness from VCTE (kPA)") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 16),  # Font size for x-axis tick labels
    axis.text.y = element_text(size = 16),  # Font size for y-axis tick labels
    axis.title.x = element_text(size = 14), # Font size for x-axis title
    axis.title.y = element_text(size = 14), # Font size for y-axis title
  )

ggplot(df, aes(x = `MRE: (F0-1<3.14, F2<3.53, F3<4.45, F4>=4.45)`, y = `LSM on MRE`)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 8, size = 4, color = "red") +  # Add mean as asterisks
  labs(title = "Box Plot of Liver Stiffness by Fibrosis Stage",
       x = "Fibrosis Stage",
       y = "Liver Stiffness from MRE (kPA)") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 16),  # Font size for x-axis tick labels
    axis.text.y = element_text(size = 16),  # Font size for y-axis tick labels
    axis.title.x = element_text(size = 14), # Font size for x-axis title
    axis.title.y = element_text(size = 14), # Font size for y-axis title
  )

