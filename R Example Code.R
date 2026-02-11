# Coding Sample – Dhruv Singh Yadav

#Loading Necessary libraries 
library(tidyverse)
library(haven)
library(broom)
library(dplyr)
library(skimr)
library(labelled)
library(ggplot2)
library(estimatr)
library(sandwich)  
library(lmtest)
library(stargazer)
library(knitr)
library(stats)

# Problem 1 :  Criminalization of Sex Work and Health Outcomes #

# Reading the data
data <- read_dta("SexWork.dta")

#subsetting data into baseline and endline depending on the el survey variable
baseline_data <- subset(data, data$el == 0) 
endline_data <- subset(data, data$el == 1)

## PART 1 ##

# QUESTION 3 AND 4 #

# Ensure 'closing' is a factor - to split criminalized and non-criminalized groups for means and t-tests
baseline_data$closing <- as.factor(baseline_data$closing)

# Select relevant variables for balance table 
vars <- c("married", "edyrs", "age", "children", "sw_locyrs", "disc_factor", "risktol_nodl")

# Compute summary table 
summary_table <- data.frame(
  Mean_Criminalized = # computing means for criminalized group
    sapply(vars, function(var) mean(baseline_data[[var]][baseline_data$closing == 1], na.rm = TRUE)), 
  Mean_NonCriminalized = # computing means for non-criminalized group
    sapply(vars, function(var) mean(baseline_data[[var]][baseline_data$closing == 0], na.rm = TRUE)),
  Pvalue = sapply(vars, function(var) {
    t.test(
      na.omit(baseline_data[[var]][baseline_data$closing == 0]),  # Non-criminalized 
     na.omit(baseline_data[[var]][baseline_data$closing == 1]),   # Criminalized 
      paired = FALSE,
      var.equal = TRUE
    )$p.value
  })
)
# Using stargazer to format the table and get it as a latex output 
stargazer(summary_table, 
          type = "latex",   
          summary = FALSE,  
          title = "Summary Statistics (Balance Table)",
          digits = 3, 
          out = "D:/SSE/Sem2_2/5313 - Dev Econ/Assignment 1/BalanceTable.tex" )

## PART 2 ##

# QUESTION 1 #

# Endline Regressions - without controls 
model1 <- lm(sept_any ~ closing, data = endline_data)
cluster_se <- vcovCL(model1, cluster = ~ wsid, type = "HC0") #clustering standard errors at worksite level using wsid

#Formatting the results 
stargazer(model1,
          type = "latex",
          se = cluster_se , 
          title = "Endline Regression Results - no controls",
          star.cutoffs = c(0.1, 0.05, 0.01),  # Significance stars (*, **, ***)
          digits = 3,
          out = "D:/SSE/Sem2_2/5313 - Dev Econ/Assignment 1/EndlineReg_c.tex" )

# QUESTION 3 #

#Endline Regressions - with controls 
# model 2 = baseline including individual characteristics- marital status, age, education and no. of children
model2 <- lm(sept_any ~ closing + edyrs + married + age + children, data = endline_data)
# model 3 = model 2 but with a risk tolerance measure 
model3 <- lm(sept_any ~ closing + edyrs + married + age + children + risktol_nodl, 
             data = endline_data)
#model 4 = model 3 along with discount factor 
model4 <- lm(sept_any ~ closing + edyrs + married + age + children + risktol_nodl + 
                disc_factor, data = endline_data)
#model 5 = model 4 with an additional control for number of years at the worksite 
model5 <- lm(sept_any ~ closing + edyrs + married + age + children + risktol_nodl + 
              disc_factor + sw_locyrs, data=endline_data)

# Computing clustered Standard Errors (clustered at worksite level using wsid)
cluster_se_list1 <- list(
  vcovCL(model2, cluster = ~ wsid, type = "HC0"),
  vcovCL(model3, cluster = ~ wsid, type = "HC0"),
  vcovCL(model4, cluster = ~ wsid, type = "HC0"),
  vcovCL(model5, cluster = ~ wsid, type = "HC0"))

#Formatting the results using Stargazer and getting the output in latex file
stargazer(model2, model3, model4, model5,
          type = "latex",  
          se = cluster_se_list1,
          title = "Endline Regression Results - with controls",
          star.cutoffs = c(0.1, 0.05, 0.01),  # Significance stars (*, **, ***)
          digits = 3,
          out = "D:/SSE/Sem2_2/5313 - Dev Econ/Assignment 1/EndlineReg_nc.tex" )  

# QUESTION 4 - Baseline Regressions checking for presence of attrition bias #

#filtering data into criminalized and non-criminalized baseline groups 
criminalized <- data %>% filter(el == 0 & closing == 1)
noncriminalized <- data %>% filter(el == 0 & closing == 0)

#Baseline attrition Regression - criminalized 
model6 <- lm(sept_any ~ attrited, data = criminalized)

#Baseline attrition Regression - non-criminalized 
model7 <- lm(sept_any ~ attrited, data = noncriminalized)

# Computing robust Standard Errors 
cluster_se_b1 <- vcovCL(model6, cluster = ~ wsid, type = "HC0")
cluster_se_b2 <- vcovCL(model7, cluster = ~ wsid, type = "HC0")
  
#Formatting the results and getting latex output 
stargazer(model6, # for the criminalized group
          type = "text", 
          se = cluster_se_b1, 
          title = "Baseline Regression Results-criminalized",
          star.cutoffs = c(0.1, 0.05, 0.01),  # Significance stars (*, **, ***)
          digits = 3, 
          out = "D:/SSE/Sem2_2/5313 - Dev Econ/Assignment 1/BaselineRegcrim.tex") 

stargazer(model7, # for the non-criminalized group 
          type = "text", 
          se = cluster_se_b2, 
          title = "Baseline Regression Results- Non-criminalized",
          star.cutoffs = c(0.1, 0.05, 0.01),  # Significance stars (*, **, ***)
          digits = 3, 
          out = "D:/SSE/Sem2_2/5313 - Dev Econ/Assignment 1/BaselineRegnocrim.tex") 

## PART 3 ##
# QUESTION 1 #
crimEl = mean(data$sept_any[data$closing ==1 & data$el==1], na.rm=TRUE) #a
crimBl = mean(data$sept_any[data$closing ==1 & data$el==0], na.rm=TRUE) #b
noncrimEl = mean(data$sept_any[data$closing ==0 & data$el==1], na.rm=TRUE)#c
noncrimBl = mean(data$sept_any[data$closing ==0 & data$el==0], na.rm=TRUE)#d

crimDiff = crimEl-crimBl #a-b
noncrimDiff = noncrimEl-noncrimBl #c-d
ElDiff = crimEl-noncrimEl # a-c
BlDiff = crimBl-noncrimBl #b-d

naiveDiD = ElDiff-BlDiff
#the difference was larger between the areas before the
#criminalisation than after, meaning that since the non-criminalized
#area har more STIs before, the criminalisation might have caused an
#increase in STIs

# QUESTION 3 #
library(ggplot2)

#creating df with the values to be used
plot_data <- data.frame(
  group = rep(c("Crim", "Non-Crim"), each = 2), # crim och non-crim groups
  time = rep(c("Baseline", "Endline"), 2), # baseline and endline
  value = c(crimBl, crimEl, noncrimBl, noncrimEl) # means
)

# creating the barplot with ggplot
ggplot(plot_data, aes(x = group, y = value, fill = time)) +
  geom_bar(stat = "identity", position = "dodge") + # bars next to each other
  geom_text(aes(label = round(value, 2)), vjust = -0.5, position = position_dodge(0.9)) + #numbers on the bars
  labs(title = "Test Positive, all worksites",
       x = "Group",
       y = "Percentage of positive tests",
       fill = "Time") + 
  theme_minimal() +
  scale_fill_manual(values = c("Baseline" = "coral", "Endline" = "pink")) #colors

## PART 4 ##
# QUESTION 1 #
#creating the did indicator
data$DiD = data$closing * data$el
View(data)

#estimating model
did_model <- lm(sept_any ~ closing + el + DiD, data = data)
summary(did_model)

#reestimating with clustered se:s
install.packages("fixest")
library(fixest)

did_model_fixest <- feols(sept_any ~ closing + el + DiD, 
                          data = data, 
                          cluster = data$wsid)

summary(did_model_fixest)

# QUESTION 2 #
did_model_fixest <- feols(sept_any ~ closing + el + DiD + married + age + children, 
                          data = data, 
                          cluster = data$wsid)

summary(did_model_fixest)

# QUESTION 3 #
#defining an interaction variable
data$closing_risktol <- data$closing * data$risktol_nodl

#model with clustered se:s
model_fixest_risktol <- feols(sept_any ~ closing + risktol_nodl + closing_risktol + married + age  + children, 
                              data = data, 
                              cluster = data$wsid)

summary(model_fixest_risktol)

#inluding did as well
# interaction variables
data$post_treatment <- data$el * data$closing
data$post_treatment_risktol <- data$el * data$closing * data$risktol_nodl

#estimating model w clustered se:s
did_model_fixest_risktol <- feols(sept_any ~ el + closing + post_treatment + 
                                    risktol_nodl + post_treatment_risktol + 
                                    married + age + edyrs + children, 
                                  data = data, 
                                  cluster = data$wsid)

summary(did_model_fixest_risktol)
