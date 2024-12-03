library(dplyr)
library(ggplot2)


# load data
data <- read.csv("data/diabetes_binary_health_indicators_BRFSS2015.csv")

# data summaries from looking at the kaggle descriptions
# Diabetes_binary - thing to predict - 0 = no diabetes, 1 = prediabetes or diabetes
# HighBP - high blood pressure - 0 = no, 1 = yes
# HighChol - high cholesterol - 0 = no, 1 = yes
# CholCheck - cholesterol checked - 0 = no, 1 = yes (within 5 years)
# BMI - body mass index
# Smoker - 5 packs (100 cigarettes lifetime) - 0 = no, 1 = yes
# Stroke - ever told had stroke - 0 = no, 1 = yes
# HeartDiseaseorAttack - CHD or MI - 0 = no, 1 = yes
# PhysActivity - last 30 days - 0 = no, 1 = yes
# Fruits - 1 or more fruits/day - 0 = no, 1 = yes
# Veggies - 1 or more veggies/day - 0 = no, 1 = yes
# HvyAlcoholConsump - 14/wk men or 7/wk women - 0 = no, 1 = yes
# AnyHealthcare - 0 = no, 1 = yes
# NodocbcCost - needed to see a doctor but couldn't past 12 month - 0 = no, 1 = yes
# GenHlth - general health - 1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor
# MentHlth - days thinking about in last 30 days - 0-30
# PhysHlth - days thinking about in last 30 days - 0-30
# DiffWalk - walking or stairs difficulty - 0 = no, 1 = yes
# Sex - 0 = female, 1 = male
# Age - grouped by 5 years - 1-13 - first is 18-24 and last is 80+
# Education = 1-6, from never attended to college graduate
# Income - 1-8, from less than $10,000 to $75,000+

# no NAs
sum(is.na(data))
table(data$Age)

n <- nrow(data) # 253680 items
table(data$Diabetes_binary) / n # 14% have diabetes, would get 86% simply by guessing no on all


helper <- function(category) {
  # finds the proportions of the category for yes/no categories
  # finds the proportion of each combination
  # finds the percentages of diabetes based on the category
  prop <- table(data[[category]]) / n
  ct <- table(ifelse(data$Diabetes_binary == 1, "Diabetes", "NoDiabetes" ),
              ifelse(data[[category]] == 1, category, paste0("No", category))) / n
  explained <- t(t(ct) / ( ct[1,] + ct[2,]))
  max(explained[,1]) * prop[2] + max(explained[,2]) * prop[1]
  print(prop)
  print(ct)
  print(explained)
}

helper("HighBP")
helper("HighChol")
helper("CholCheck")
helper("Smoker")
helper("Stroke")
helper("HeartDiseaseorAttack")
helper("PhysActivity")
helper("Fruits")
helper("Veggies")
helper("HvyAlcoholConsump")
helper("AnyHealthcare")
helper("NoDocbcCost")
helper("DiffWalk")
helper("Sex")

table(f=data$Fruits, dw=data$DiffWalk, d=data$Diabetes_binary)

# leftover BMI, GenHlth, MentHlth, PhysHlth, Age, Education, Income
cor(data$MentHlth, data$Diabetes_binary)
cor(data$PhysHlth, data$Diabetes_binary)
cor(data$GenHlth, data$Diabetes_binary)
cor(data$BMI, data$Diabetes_binary)
cor(data$Age, data$Diabetes_binary)
cor(data$Education, data$Diabetes_binary)
cor(data$Income, data$Diabetes_binary)

ggplot(data, aes(x = Age, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = Income, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = Education, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = GenHlth, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = MentHlth, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = PhysHlth, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = BMI, fill = factor(Diabetes_binary))) +
    geom_histogram(position = "fill")
hist(data$BMI, breaks = 20)

# summing across the common items that have an effect
# summing across the rare items that have an effect
data_fixed <- data |>
  mutate(
    count_common = rowSums(across(c(HighBP, HighChol, PhysActivity, DiffWalk, Smoker))),
    count_rare = rowSums(across(c(CholCheck, Stroke, HeartDiseaseorAttack, HvyAlcoholConsump)))
  ) |>
  select(-c(Fruits, Veggies, AnyHealthcare, NoDocbcCost, Income, Education, PhysHlth, MentHlth, Sex))

table(data_fixed$Diabetes_binary, data_fixed$count_common)
table(data_fixed$Diabetes_binary, data_fixed$count_rare)

str(data_fixed)
data_fixed_m <- data_fixed |>
  mutate(
    HighBP = factor(HighBP, levels = c(0, 1), labels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c(0, 1), labels = c("No", "Yes")),
    CholCheck = factor(CholCheck, levels = c(0, 1), labels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c(0, 1), labels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c(0, 1), labels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0, 1), labels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("No", "Yes")),
    DiffWalk = factor(DiffWalk, levels = c(0, 1), labels = c("No", "Yes")),
    GenHlth = factor(GenHlth, levels = c(1, 2, 3, 4, 5), labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    Age = factor(Age, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
    Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("No", "Yes"))
  )
str(data_fixed_m)

