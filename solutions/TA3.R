# Week 3 Notebook

library(haven)
library(magrittr)
library(estimatr)
library(dplyr)
library("webuse")   # top import data from stata's website
library(lfe)
library(texreg)

# get data
download.file(url = "https://dss.princeton.edu/training/states.dta",
              destfile = "states.dta")

df = read_dta("states.dta")


summary(df)

# R default regression command
lm(csat ~ expense, data = df) %>%
  summary

# vce robust standard errors
lm_robust(csat ~ expense, 
          data = df,
          se_type = "stata")

# add more variables
lm_robust(csat ~ expense + percent + income + high + college, 
          data = df,
          se_type = "stata")


# load example dataset

df2 = webuse("nlsw88")

summary(df2)

# tab variable?
# tab occupation?

# standard regression
reg1 = lm(wage ~ hours,
          data = df2)

# regression with industry dummy variables
reg2 = lm_robust(wage ~ hours + factor(industry),
          data = df2,
          se_type = "stata")

# FE + clustering
reg3 = felm(wage ~ hours | industry | 0 | industry,
     data = df2)

reg4 = felm(wage ~ hours | industry + occupation | 0 | industry + occupation,
     data = df2)

screenreg(list(reg1,reg2,reg3,reg4),
          custom.header = list("OLS" = 1, "OLS robust" = 2, "clustered FE" = 3:4),
          custom.model.names = c(" ", "factor(Industry)", "industry", "industry+occupation"),
          omit.coef = "factor*",
          include.ci = FALSE)

modelsummary(list(reg1,reg2,reg3,reg4))

str(df)
