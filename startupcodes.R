#start up codes
####START UP #####
#upload data and tidy data
library(dplyr)
library(dplyr)
library(caret)
library(lmtest)
library(pROC)
#upload the data:
data<-read.csv("startup.csv")
str(data)
View(data)
#data at first glance:
dim(data) #234-50
sum(is.na(data)) #0, no imputation needed
colnames(data)
#manipulate data shortly:
#rename response variable: Successs(bianry variable)
data <- data%>% rename(Success = Dependent)
# change variable names:shorten Company-Founders-Founder
colnames(data) <- gsub("Company_", "C_", colnames(data))
colnames(data) <- gsub("Founders_", "F_", colnames(data))
colnames(data) <- gsub("Founder_", "F_", colnames(data))
# shorten: skills_score
colnames(data) <- gsub("skills_score$", "SS", colnames(data))
#split numeric-categoric data
numeric_data<- data[,c(6,7,8,9,10,12,24,27,28,29,30,31,32,33,34,35,36,37,39,40,41,45)]
categoric_data<- data[,-c(6,7,8,9,10,12,24,27,28,29,30,31,32,33,34,35,36,37,39,40,41,45)]
#turn all categoric data to factor
categoric_data <- categoric_data %>% mutate(across(everything(), as.factor))
data <- cbind(numeric_data, categoric_data)

#EDA&CDA: Brief
#see the distribution of Success (dependent variable)
ggplot(data, aes(x = Success)) + 
  geom_bar(fill = "darkgray") +
  ggtitle("Success Frequency Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Frequency")  
#no class imbalance detected.

#decide for distributions for numeric data
# calculate skewness for all numeric_data
install.packages("moments")
library(moments)
#skewnwss:
skewness_values <- apply(numeric_data, 2, skewness, na.rm = TRUE)
print(skewness_values)
#kurtosis:
kurtosis_value <- apply(numeric_data, 2, kurtosis, na.rm = TRUE)
print(kurtosis_value)
#decision:non-normal

#an alaternative for normality testing:
#anderson-darling test loop: for normality since=50<n<500
install.packages("nortest")  
library(nortest)
# Anderson-Darling test vectors creation:
ad_results <- data.frame(Variable = character(),
                         AD_Statistic = numeric(),
                         p_value = numeric(),
                         Normality = character(),
                         stringsAsFactors = FALSE)

#a for loop to shorten the process
for (col in colnames(numeric_data)) {
  test_result <- ad.test(numeric_data[[col]])
  p_val <- test_result$p.value
  normality <- ifelse(p_val > 0.05, "Normal", "Non-Normal")
  
  ad_results <- rbind(ad_results, data.frame(
    Variable = col,
    AD_Statistic = test_result$statistic,
    p_value = p_val,
    Normality = normality))}

# show results:
print(ad_results) #all of them are non-normal

#check the correlations for numeric variables
cornum_data<-cor(numeric_data,method="kendall") #since they are non-normal
#looking at relations visually:correlation heatmap
install.packages("pheatmap")
library(pheatmap)
pheatmap(cornum_data,
  color = colorRampPalette(c("blue", "white", "red"))(100),  
  breaks = seq(-1, 1, length.out = 101),# -1 and 1 equal breaks
  display_numbers = TRUE, fontsize = 10,fontsize_number = 8, cluster_rows = FALSE,                                 
  cluster_cols = FALSE,border_color = NA,angle_col = 45)

#0.3 and higher correlations:
filtered_cornum_data <- ifelse(abs(cornum_data) >= 0.3, cornum_data, NA)
pheatmap(filtered_cornum_data,
         color = colorRampPalette(c("blue", "white", "red"))(100), 
         breaks = seq(-1, 1, length.out = 101), display_numbers = TRUE, fontsize = 10,                                           
         fontsize_number = 8, cluster_rows = FALSE, cluster_cols = FALSE, na_col = "grey")

#Significance between response-covariates:
#numeric variables vs success (dependent variable):Mann-Whitney-U test for significance for mean difference
#Mann-Whitney U Test (since they are all non-normally distributed)
results <- numeric_data %>%
  select_if(is.numeric) %>%
  summarise(across(
    everything(),
    ~ {
      test_result <- wilcox.test(. ~ data$Success)
      p_value <- test_result$p.value
      significance <- ifelse(p_value < 0.05, "Significant", "Non-Significant")
      list(p_value = p_value, significance = significance)}))
# put results in to a table
results_df <- tibble(Variable = names(results),P_Value = sapply(results, function(x) x$p_value),
  Significance = sapply(results, function(x) x$significance))
# filter only significant ones
significant_results <- results_df %>% filter(Significance == "Significant")
# see significant results
print(significant_results)


#categoric-categoric independency test for 2X2 : chi-square
# 1. choose 2 level categoric data
two_level_vars <- categoric_data %>%select_if(~ is.factor(.) && nlevels(.) == 2) %>%colnames()

# 2. Chi-Square test for each variable for Success 
results2 <- lapply(two_level_vars, function(var) {
  contingency_table <- table(categoric_data[[var]], categoric_data$Success)
  test <- chisq.test(contingency_table)
  data.frame(Variable = var, P_Value = test$p.value)})

# 3. aggregate results and filter the sginificant ones
results_categoric2level <- bind_rows(results2) %>%
  mutate(Significance = ifelse(P_Value < 0.05, "Significant", "Non-Significant")) %>%
  filter(Significance == "Significant")

# 4. see significant results
print(results_categoric2level)

#split data for only significant variables with success:
signifdata<-data[,c(1,5,6,8,14,19,20,21,22,23,28,35,44,45)]

#traditional logistic models:glm()
model1.1<- glm(Success~.,family=binomial, data=data)
summary(model1.1)
#logistic model with significant variables:
model1<- glm(Success~.,family=binomial, data=signifdata)
summary(model1)

install.packages("olsrr") #search for multicolinearity-no multicol detected.
library(olsrr)
vif_values <- olsrr::ols_vif_tol(model1)
print(vif_values)

#we omit C_big_data
model2 <- glm(Success ~ . - C_big_data, family = binomial, data = signifdata)
summary(model2)

dim(signifdata) #234-14
#LOGISTIC REGRESSION-part2: compare models:
#Train-test-validation data split data:

#LOGISTIC REGRESSION - Train-Validation-Test Split for Small Dataset
 
#Split 70% of the data for training
set.seed(123)
train_index <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_index, ]

#Remaining 30% of the data will be used for validation and test
remaining_data <- data[-train_index, ]

#Split 50% of the remaining data for validation
validation_index <- sample(1:nrow(remaining_data), size = 0.5 * nrow(remaining_data))
validation_data <- remaining_data[validation_index, ]

#Step 4: The rest will be the test set
test_data <- remaining_data[-validation_index, ]

#check the dimensions:ok
dim(train_data)  
dim(test_data)  
dim(validation_data)


# LOGISTIC REGRESSION: Full Pipeline with Train-Validation-Test Split

# Step 1: Build a full logistic regression model using training data
model3 <- glm(Success ~ ., family = binomial, data = train_data)
summary(model3)

# Step 2: Apply stepwise regression for variable selection (both directions)
final_model <- step(model3, direction = "both")
summary(final_model)

# Step 3: Compare full model with null model using deviance (likelihood ratio test)
reduced.model <- glm(Success ~ 1, data = train_data, family = binomial)
p_value <- pchisq(2 * (logLik(final_model) - logLik(reduced.model)), df = 2, lower.tail = FALSE)
print(p_value)

# Alternative LR test using lmtest package
lr_test_result <- lmtest::lrtest(reduced.model, final_model)
print(lr_test_result)

# Step 4: Predict probabilities on the test set
probs <- predict(final_model, test_data, type = "response")

# Step 5: Make predictions using default threshold 0.5
predicted <- ifelse(probs > 0.5, 1, 0)

# Step 6: Confusion Matrix (threshold = 0.5)
conf_matrix <- confusionMatrix(factor(predicted, levels = c(0, 1)), factor(test_data$Success, levels = c(0, 1)), positive = "1")
print(conf_matrix)

# Step 7: ROC Curve and AUC Score
roc_curve <- roc(test_data$Success, probs, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE)

# Step 8: Find best threshold from ROC curve
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold_value <- as.numeric(best_threshold[1])
print(best_threshold_value)

# Step 9: Make predictions using best threshold
predicted2 <- ifelse(probs > best_threshold_value, 1, 0)
conf_matrix2 <- confusionMatrix(factor(predicted2, levels = c(0, 1)), factor(test_data$Success, levels = c(0, 1)), positive = "1")
print(conf_matrix2)

# Step 10: Calculate Precision, Recall, and F1 Score (threshold = 0.5)
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1 Score (threshold = 0.5):", round(f1_score, 4)))

# Step 11: Precision, Recall, and F1 Score (best threshold)
precision2 <- conf_matrix2$byClass["Precision"]
recall2 <- conf_matrix2$byClass["Recall"]
f1_score2 <- 2 * (precision2 * recall2) / (precision2 + recall2)
print(paste("F1 Score (best threshold):", round(f1_score2, 4)))

