library(dplyr)
library(ggplot2)
# load data
data <- read.csv("data/diabetes_binary_health_indicators_BRFSS2015.csv")

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
# Age - grouped by 5 years - 0-13 - first is 18-24 and last is 80+
# Education = 1-6, from never attended to college graduate
# Income - 1-8, from less than $10,000 to $75,000+

# check count of yes no flag variables that are yes
# excluding sex and non-binary variables
data <- data |>
  # count of highbp, highchol, etc that are 1
  mutate(
    count_common = rowSums(across(c(HighBP, HighChol, PhysActivity, DiffWalk))),
    count_rare = rowSums(across(c(CholCheck, Smoker, Stroke, HeartDiseaseorAttack, HvyAlcoholConsump)))
  )

table(data$Diabetes_binary, data$count_common > 2 & data$count_rare > 3)
table(data$Diabetes_binary, data$count_common)
table(data$Diabetes_binary, data$count_rare)
table(data$HighBP, data$count, data$Diabetes_binary)

# no NAs
sum(is.na(data))

n <- nrow(data) # 253680 items
table(data$Diabetes_binary) / n # 14% have diabetes, would get 86% simply by guessing no on all

helper <- function(category) {
  prop <- table(data[[category]]) / n
  ct <- table(ifelse(data$Diabetes_binary == 1, "Diabetes", "NoDiabetes" ),
              ifelse(data[[category]] == 1, category, paste0("No", category))) / n
  explained <- t(t(ct) / ( ct[1,] + ct[2,]))
  max(explained[,1]) * prop[2] + max(explained[,2]) * prop[1]
  print(prop)
  # print(ct)
  print(explained)
}
# phys, diff walk, heart disease, stroke, highchol, highbp
#
helper("HighBP") # .57
helper("HighChol") # .22
helper("CholCheck") # .14 - too many have been checked
helper("Smoker") # .16
helper("Stroke") # .31 - too many haven't had stroke
helper("HeartDiseaseorAttack")
helper("PhysActivity")
helper("Fruits")
helper("Veggies")
helper("HvyAlcoholConsump")
helper("AnyHealthcare")
helper("NoDocbcCost")
helper("DiffWalk")
helper("Sex")
table(hc = data$HighChol, hbp = data$HighBP, db = data$Diabetes_binary)

# leftover BMI, GenHlth, MentHlth, PhysHlth, Age, Education, Income
cor(data$MentHlth, data$Diabetes_binary)
cor(data$PhysHlth, data$Diabetes_binary)
cor(data$GenHlth, data$Diabetes_binary)
cor(data$BMI, data$Diabetes_binary)
cor(data$Age, data$Diabetes_binary)
cor(data$Education, data$Diabetes_binary)
cor(data$Income, data$Diabetes_binary)

cor(data$Age, data$Income)
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

hist(data$BMI)
# use age, genHlth, BMI, income?
