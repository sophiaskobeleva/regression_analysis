install.packages("dplyr") 
install.packages("foreign") 
install.packages("tableone") 
install.packages("MatchIt") 
install.packages("ggplot2") 
install.packages("cowplot") 
install.packages("haven") 
install.packages("car")

library(dplyr) 
library(foreign) 
library(tableone) 
library(MatchIt) 
library(ggplot2) 
library(cowplot) 
library(haven) 
library(car)
library(foreign)

df <- read_dta("matching_lab2.dta")

model <- lm(bweight ~ mbsmoke + mage + medu + mmarried, data = df)
smoking_mothers <- df %>% filter(mbsmoke == 1)
nonsmoking_mothers <- df %>% filter(mbsmoke == 0)

vars_to_compare <- c("mage", "medu", "mmarried")
table_of_balance <- CreateTableOne(vars = vars_to_compare, data = df, 
                                   strata = "mbsmoke", test = FALSE)
print(table_of_balance)

df$mbsmoke <- as_factor(df$mbsmoke) 
df$mage <- as.numeric(df$mage) 
df$medu <- as.numeric(df$medu) 
df$mmarried <- as_factor(df$mmarried)

matchit_model <- matchit(mbsmoke ~ mage + medu + mmarried, data = df,
                         method = "nearest")

matched_data <- match.data(matchit_model)
table_matched <- CreateTableOne(vars = c("mage", "medu", "mmarried"), data = matched_data, 
                                strata = "mbsmoke", test = FALSE)
print(table_matched)

matched_model <- lm(bweight ~ mbsmoke, data = matched_data) 
summary(matched_model)
