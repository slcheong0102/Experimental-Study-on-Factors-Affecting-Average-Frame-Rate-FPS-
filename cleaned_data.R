# ----- Setup -----
# Cleaning the environment

rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)


data <- read.table("C:/Users/soulu/OneDrive - University of Toronto/University of Toronto/UTM 2021-2023/8_2024 Winter/STA305H5S/Project/STA305 Data.txt",
sep=",", header=TRUE, fileEncoding = "UTF-16LE")

data1 <- data

# GTA5 = 1, Forza_Horizon5 = 2, Cyberpunk2077 = 3
data1$Games <- replace(data1$Games, data1$Games=="GTA5", 1)
data1$Games <- replace(data1$Games, data1$Games=="Forza_Horizon5", 2)
data1$Games <- replace(data1$Games, data1$Games=="Cyberpunk2077", 3)

# MSI_RTX3060ti = 1, GTX1060 = 2
data1$GPU_Types <- replace(data1$GPU_Types, data1$GPU_Types=="MSI_RTX3060ti", 1)
data1$GPU_Types <- replace(data1$GPU_Types, data1$GPU_Types=="GTX1060", 2)

# 5600x3.2GHz = 1, 5600x3.7GHz = 2, 5600x4.2GHz = 3
data1$CPU_Frequency <- replace(data1$CPU_Frequency, data1$CPU_Frequency=="5600x3.2GHz", 1)
data1$CPU_Frequency <- replace(data1$CPU_Frequency, data1$CPU_Frequency=="5600x3.7GHz", 2)
data1$CPU_Frequency <- replace(data1$CPU_Frequency, data1$CPU_Frequency=="5600x4.2GHz", 3)

write.csv(data1, "C:/Users/soulu/OneDrive - University of Toronto/University of Toronto/UTM 2021-2023/8_2024 Winter/STA305H5S/Project/STA305 Clean.csv", 
          row.names=FALSE)

cpu <- factor(data$CPU_Frequency)
gpu <- factor(data$GPU_Types)
fps <- data$Average_FPS

# ----- Check Normality and Equal Variance Assumptions -----
res.aov <- aov(Average_FPS~CPU_Frequency+GPU_Types, data=data);
summary(res.aov);
shapiro.test(residuals(res.aov));
# p-value is 5.563e-08 < 0.05, i.e. we reject H0: Normality of residuals
plot(res.aov, 1);
plot(res.aov, 2);
df = within(data,{group<-paste(GPU_Types,CPU_Frequency)});
bartlett.test(Average_FPS~group,data=df);
# no equal variance
model1=lm(fps~cpu*gpu,data=data);
summary(model1);
model2=lm(fps~cpu+gpu,data=data);
summary(model2);
anova(model1);
anova(model2);
boxplot(fps~group,data=df,xlab='treatments');
ggplot(data=data,aes(x=gpu,y=fps,fill=cpu))+geom_boxplot();
ggplot(data=data,aes(x=cpu,y=fps,fill=gpu))+geom_boxplot();
