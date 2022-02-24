###############################################################################################
# Prepared for the BUEC 488:
# Data Analysis for Business Economics and Policy - Winter 2022
# by Ikechukwu NWAKA 
# 
# License:Data sourced from Demographic Health Survey 2013 (DHS) for Nigeria
#######################################################################################x

# Additional practice for CHAPTER 11
# Research questions:
#1. Are Female headed households more likely to use poor energy sources for cooking?
#2. Do family size and poverty rate translate to poor energy choices? 

# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())
# Import libraries
install.packages("flextable")
library(flextable)
library(tidyverse)
library(xtable)
library(haven)
library(cowplot)
library(lspline)
library(data.table)
library(mfx)
library(margins)
library(stargazer)
library(psych)
library(estimatr)
library(huxtable)
#set the working directory for the case studies:
setwd("~/Desktop/TAship/Resources - Copy/da_case_studies")

# set data dir, load theme and functions:
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used

data_dir<-"~/Desktop/TAship/Resources - Copy/da_data_repo" #Alternatively: source("C:/Users/Ikechukwu Nwaka/Desktop/WINTER 2022/BUEC 488/da_data_repo/set-data-directory.R") 

data_in <- paste(data_dir,"Cookingfuel-Nigeria","clean/", sep = "/")

use_case_dir <- "Cookingfuel-practice-Chapter11/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------



################################################################################
# 1. PART - CREATE WORKFILE
################################################################################

# load in clean and tidy data and create workfile

cook_data <- read.csv(paste(data_in,"cooking_fuel_data.csv", sep = "/"),stringsAsFactors = F)

# Observe the data
head(cook_data, n=5)

# Observe variables classification - strings or numeric 
sapply(cook_data, class)

#Key variables of interest: cook_fl; gender_hd, hhsize, wealth and others are transformed to numeric variables

table(cook_data$gender_hd) 
# Gender dummy of family heads such that Female-heads =1 and Male-heads=0
cook_data$Female.Dummy<-ifelse(cook_data$gender_hd=="Female",1,0)
# Create cooking energy dummy
#Poor cooking fuel sources = 1 which include wood, Coal, straw, charcoal,  agricultural crops, animal dung
#Cleaner cooking fuel sources= 0 which include electricity, LPG, natural gas, biogas, kerosene
table(cook_data$cook_fl) 
cook_data$cook_fl.Dummy<-ifelse(cook_data$cook_fl=="Wood" | cook_data$cook_fl=="Charcoal" | cook_data$cook_fl=="Coal, lignite" | cook_data$cook_fl=="Straw/shrubs/grass" | cook_data$cook_fl=="Agricultural crop" | cook_data$cook_fl=="Animal dung",1,0)
table(cook_data$cook_fl.Dummy) 

#Poverty status - check to see
table(cook_data$wealth) 

# Generate dummies for the poor ( Poorer and Poorest) and non-poor (Middle, Richer and Richest)
cook_data$Poverty.Dummy<-ifelse(cook_data$wealth=="Poorer" | cook_data$wealth=="Poorest",1,0)


table(cook_data$ed_hd) 

summary(cook_data[ cook_data$ed_hd != "9", , drop=FALSE]) #Drop education categorized as 9

#Education dummy = 1 (>=secondary) and 0 otherwise
cook_data$ed_hd_dum<-ifelse(cook_data$ed_hd=="Secondary" | cook_data$ed_hd=="Higher",1,0)



# Summary stats
summary(cook_data$Gender.Dummy)
summary(cook_data$cook_fl.Dummy)
summary(cook_data$Poverty.Dummy)
summary(cook_data$ed_hd_dum)
summary(cook_data$HHsize)

class(cook_data$Age_hd)
class(cook_data$cook_fl.Dummy)

#Convert Age to numeric
cook_data$Age_hd <- as.numeric(cook_data$Age_hd)
# save temp file
write.csv(cook_data, paste0(data_out, "ch11_cook_energy.csv"), row.names = F)

################################################################################
# 2. PART - SIMPLE LPM MODELS
################################################################################

# Linear probability models of good health at endline and smoking

# (1) Let Y = DV = Fuel Choice and IV = Gender of Head of the Family
lpm1 <- lm(cook_fl.Dummy ~ Female.Dummy, data=cook_data)
summary(lpm1, vcov=sandwich)

# visualize this regression
cook_data$pred1 <- predict(lpm1)

table(cook_data$pred1, cook_data$Female.Dummy)

#create weights
cook_data<-cook_data %>%
  group_by(cook_fl.Dummy, Female.Dummy) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))


g1<-ggplot(data = cook_data, label=cook_fl.Dummy) +
  geom_point(aes(x = Female.Dummy, y = pred1), size = 1, color=color[1], shape = 16) +
  geom_line(aes(x = Female.Dummy, y = pred1), colour=color[1],  size=0.7) +
  geom_point(aes(x = Female.Dummy, y = cook_fl.Dummy, size=weight_2), fill = color[2], color=color[2], shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Female Head",y = "Poor Energy Choice / Predicted probability of ")+
  coord_cartesian(xlim = c(0, 1), ylim=c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,1))+
  theme_bg() 
g1

save_fig("Cooking_fuel_Gender1", output, "small")

# (2) Add head of the family's highest educational attainment as covariate
lpm2 <- lm(cook_fl.Dummy ~ Female.Dummy + as.factor(ed_hd), data=cook_data)
summary(lpm2, vcov=sandwich)


# adding other right-hand-side variables
# first check some functional forms

cook_data<-cook_data %>%
  group_by(HHsize, cook_fl.Dummy) %>%
  mutate(weight = n()/100)

g2a<-ggplot(data = cook_data, aes(x=HHsize, y=cook_fl.Dummy)) +
  #  geom_point(aes(x = eduyears, y = stayshealthy, size=weight), color=color[1], shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  geom_smooth_da(method="loess", color=color[1]) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(1,35), breaks = seq(1,35,5))+
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  labs(x = "Family size",y = "Probability of using poor cooking energy") +

  theme_bg() 
g2a
#Save the plot as an image file in the output folder 
save_fig("Cook_fuel and HHsize", output, "small")

g2b<-ggplot(data = cook_data, aes(x=Age_hd, y=cook_fl.Dummy)) +
  geom_smooth_da(method="loess", color=color[1]) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(15, 75), breaks = seq(10,75,10))+
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  labs(x = "Age of family head)",y = "Probability of using poor cooking energy ") +
  theme_bg()
g2b
#Save the plot as an image file in the output folder
save_fig("cook_fuel_age", output, "small")


################################################################################
# 3. PART - PROBABILITY MODELS (LPM, LOGIT, PROBIT) & PREDICTION
################################################################################

# creating quadratic HHsize variable 
cook_data$HHsize_sq <- cook_data$HHsize^2 


lpm3 <-lm(cook_fl.Dummy ~ Female.Dummy + as.factor(ed_hd) + Poverty.Dummy + HHsize + HHsize_sq, data=cook_data)
summary(lpm3, vcov=sandwich)

# predicted probabilities 
cook_data$pred_lpm <- predict(lpm3)
summary(cook_data$pred_lpm)

share_pred_lpm <- data.table(cook_data)
share_pred_lpm[,.(mean=mean(cook_data$pred_lpm), sd = sd(cook_data$pred_lpm), min = min(cook_data$pred_lpm) , max = max(cook_data$pred_lpm),
                  q25 = quantile(cook_data$pred_lpm, probs = c(0.25)), q50 = quantile(cook_data$pred_lpm, probs = c(0.5)), 
                  q75 = quantile(cook_data$pred_lpm, probs = c(0.75)),.N)]



g3<-ggplot(data=cook_data, aes(x=pred_lpm)) +
  geom_histogram_da(type='percent', binwidth=0.02) +
  coord_cartesian(xlim = c(0, 1.2)) +
  labs(x = "Predicted probability of using poor cooking energy(LPM)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0,0.07), breaks = seq(0, 0.07, 0.01), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0.001,0.01), limits = c(0,1.1), breaks = seq(0,1.1, 0.2)) +
  theme_bg() 
g3
#Save the plot as an image file in the output folder
save_fig("cooking-pred-histogram-lpm", output, "small")

# list top 1% and bottom 1%
cook_data <- cook_data[!is.na(cook_data$pred_lpm), ]

cook_data <- cook_data %>% 
  mutate(q100_pred_lpm = ntile(pred_lpm, 100))

summary(cook_data[cook_data$q100_pred_lpm==1, ]$Female.Dummy)
summary(cook_data[cook_data$q100_pred_lpm==1, ]$Poverty.Dummy) 
summary(cook_data[cook_data$q100_pred_lpm==1, ]$ed_hd) 
summary(cook_data[cook_data$q100_pred_lpm==1, ]$HHsize) 
summary(cook_data[cook_data$q100_pred_lpm==1, ]$Age_hd) 

summary(cook_data[cook_data$q100_pred_lpm==100, ]$Female.Dummy)
summary(cook_data[cook_data$q100_pred_lpm==100, ]$Poverty.Dummy) 
summary(cook_data[cook_data$q100_pred_lpm==100, ]$ed_hd) 
summary(cook_data[cook_data$q100_pred_lpm==100, ]$HHsize) 
summary(cook_data[cook_data$q100_pred_lpm==100, ]$Age_hd) 



# Interaction between poverty and family size
lpm4 <-lm(cook_fl.Dummy ~ Female.Dummy + as.factor(ed_hd) + Poverty.Dummy + HHsize + HHsize_sq + Poverty.Dummy*HHsize, data=cook_data)
summary(lpm4, vcov=sandwich)
stargazer(lpm1, lpm2, lpm3, lpm4, digits=3, out=paste(output,"T11_reg3_R.html",sep=""))

################################################################################
# 4. PART - LOGIT VS. PROBIT MODELS
################################################################################

# lpm versus logit and probit
# with all right-hand-side variables

#prep
library(pscl)
library(modelsummary)
library(margins)



# lpm (repeating the previous regression)
lpm <-lm(cook_fl.Dummy ~ Male.Dummy + as.factor(ed_hd) + Affluence.Dummy + HHsize + HHsize_sq + Affluence.Dummy*HHsize, data=cook_data)
summary(lpm4, vcov=sandwich)
stargazer(lpm1, lpm2, lpm3, lpm4, digits=3, out=paste(output,"T11_reg3_R.html",sep=""))

# logit coefficients
logit <- glm(cook_fl.Dummy ~ Female.Dummy + as.factor(ed_hd) + Poverty.Dummy + HHsize + HHsize_sq + Poverty.Dummy*HHsize, data=cook_data, family='binomial')
summary(logit)
glance(logit)





# predicted probabilities 
cook_data$pred_logit <- predict.glm(logit, type="response")
summary(cook_data$pred_logit)


# logit marginal differences
logit_marg <- margins(logit, type = "response")
summary(logit_marg)
logit_marg

# probit coefficients
probit <- glm(cook_fl.Dummy ~ Female.Dummy + as.factor(ed_hd) + Poverty.Dummy + HHsize + HHsize_sq+ Poverty.Dummy*HHsize, data=cook_data, family=binomial(link="probit"))
summary(probit)
glance(probit)
# predicted probabilities 
cook_data$pred_probit<- predict.glm(probit, type="response") 
summary(cook_data$pred_probit)

# probit marginal differences
probit_marg <- margins(probit, type = "response")
summary(probit_marg)
probit_marg

cm <- c('(Intercept)' = 'Constant')
msummary(list("Linear Probability Model"=lpm, "Logit"=logit, "Logit Marginal Effect"=logit_marg, "Probit"=probit, "Probit Marginal Effect"=probit_marg),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|R2|PseudoR2',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         coef_omit = 'as.factor(country)*',
         output = paste(output,"ch09_reg2.docx",sep="")
)

# adding pseudo R2 (not work for mfx)
glance_custom.glm <- function(x) data.frame(`PseudoR2` = pR2(x)["McFadden"])
cm <- c('(Intercept)' = 'Constant')
msummary(list("Linear Probability Model"=lpm, "Logit"=logit, "Probit"=probit),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         coef_omit = 'as.factor(country)*',
         output = paste(output,"ch09_reg2-R.docx",sep="")
)



g5<-ggplot(data = cook_data) +
  geom_point(aes(x=pred_lpm, y=pred_probit, color="Probit"), size=0.4,  shape=16) +
  geom_point(aes(x=pred_lpm, y=pred_logit,  color="Logit"), size=0.4,  shape=16) +
  #geom_line(aes(x=pred_lpm, y=pred_probit, color="Probit"), size=0.3) +
  #geom_line(aes(x=pred_lpm, y=pred_logit,  color="Logit"), size=0.3) +
  geom_line(aes(x=pred_lpm, y=pred_lpm,    color="45 degree line"), size=0.4) +
  labs(x = "Predicted probability of poor cooking energy (LPM)", y="Predicted probability")+
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_x_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_color_manual(name = "", values=c(color[3], color[1],color[2])) +
  theme_bg()+
  theme(legend.position=c(0.55,0.08),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4))
g5
#Save the plot as an image file in the output folder
save_fig("pred-scatter-3models", output, "small")

stargazer(lpm, logit, probit, digits=3, out=paste(output,"T11_reg4_R.html",sep=""))

stargazer(list(lpm, logit, logit_marg, probit, probit_marg), digits=3, out=paste(output,"T11_reg5_R.html",sep=""))

# FIXME:  save marginals 
# if mfx - could be useful https://github.com/tidymodels/broom/pull/756

################################################################################
# 5. PART - GOF
################################################################################
# GOODNESS OF FIT

# re-estimate the simplest lpm
lpmbase <- lm(cook_fl.Dummy ~ Female.Dummy, data=cook_data)
cook_data$pred_lpmbase <- predict(lpmbase) 
print(cook_data$pred_lpmbase)
summary(cook_data$pred_lpmbase)

# DISTRIBUTION OF PREDICTED PROBABILITIES BY OUTCOME
# LPM simple model
g7a<-ggplot(data = cook_data,aes(x=pred_lpmbase)) + 
  geom_histogram(data=subset(cook_data[cook_data$cook_fl.Dummy == 1, ]), 
                 aes(fill=as.factor(cook_fl.Dummy), color=as.factor(cook_fl.Dummy), y = (..count..)/sum(..count..)*100),
                 binwidth = 0.05, boundary=0, alpha=0.8) +
  geom_histogram(data=subset(cook_data[cook_data$cook_fl.Dummy == 0, ]), 
                 aes(fill=as.factor(cook_fl.Dummy), color=as.factor(cook_fl.Dummy), y = (..count..)/sum(..count..)*100), 
                 binwidth = 0.05, boundary=0, alpha=0) +
  scale_fill_manual(name="", values=c("0" = "white", "1" = color[1]),labels=c("Households' cleaner energy choice","Households' Poor energy choice")) +
  scale_color_manual(name="", values=c("0" = color[2], "1" = color[1]),labels=c("Households' cleaner energy choice","Households' Poor energy choice")) +
  ylab("Percent") +
  xlab("Fitted values") +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,80), breaks = seq(0,80,20)) +
  theme_bg() +
  theme(legend.position = c(0.3,0.9),
        legend.key.size = unit(x = 0.5, units = "cm"))
g7a
save_fig("ch11-figure-7a-pred-hist-byoutcome-lpmbase", output, "small") #Save the plot as an image file in the output folder

# LPM rich model
g7b<-ggplot(data = cook_data,aes(x=pred_lpm)) + 
  geom_histogram(data=subset(cook_data[cook_data$cook_fl.Dummy == 1, ]), 
                 aes(fill=as.factor(cook_fl.Dummy), color=as.factor(cook_fl.Dummy), y = (..count..)/sum(..count..)*100),
                 binwidth = 0.05, boundary=0, alpha=0.8) +
  geom_histogram(data=subset(cook_data[cook_data$cook_fl.Dummy == 0, ]), 
                 aes(fill=as.factor(cook_fl.Dummy), color=as.factor(cook_fl.Dummy), y = (..count..)/sum(..count..)*100), 
                 binwidth = 0.05, boundary=0, alpha=0) +
  scale_fill_manual(name="", values=c("0" = "white", "1" = color[1]),labels=c("Households' cleaner energy choice","Households' Poor energy choice")) +
  scale_color_manual(name="", values=c("0" = color[2], "1" = color[1]),labels=c("Households' cleaner energy choice","Households' Poor energy choice")) +
  ylab("Percent") +
  xlab("Fitted values") +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,20), breaks = seq(0,20,4)) +
  theme_bg() +
  theme(legend.position = c(0.3,0.9),
        legend.key.size = unit(x = 0.5, units = "cm"))
g7b

save_fig("ch11-figure-7b-pred-hist-byoutcome-lpm", output, "small") #Save the plot as an image file in the output folder

### SUMMARY STATS OF PREDICTED PROBABILITIES BY OUTCOME
dt_pred = data.table(cook_data)
dt_pred[,list(mean_lpmbase=mean(pred_lpmbase), mean_lpm=mean(pred_lpm), mean_logit=mean(pred_logit), mean_probit=mean(pred_probit)),by=list(cook_fl.Dummy)]
dt_pred[,list(median_lpmbase=median(pred_lpmbase), median_lpm=median(pred_lpm), median_logit=median(pred_logit), median_probit=median(pred_probit)),by=list(cook_fl.Dummy)]









