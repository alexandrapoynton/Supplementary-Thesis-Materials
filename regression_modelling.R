#importing dyadic and monadic data
stateyr <-read.csv("C:\\Users\\User\\Documents\\Honours\\Thesis\\Datasets\\Ready to Go\\noeu\\stateyearnoeu.csv")
dyadyr <-read.csv("C:\\Users\\User\\Documents\\Honours\\Thesis\\Datasets\\Ready to Go\\noeu\\dyadyearnoeu.csv")

#loading packages
library(dplyr)
library(car)

dyadyr$minpolity <- do.call(pmin,c(dyadyr[18:19],na.rm = TRUE))

#now make my new dataset to run regression analysis on
#df with variables i will consider
intsys <- dyadyr[,c(4, 5, 8, 10, 11, 14, 20, 23, 24)]
intsys <- intsys[!intsys$aresp_tdpr %in% c("10"),] #remove structural impossibilities
colnames(intsys)[3] = 'comp_in_td'
colnames(intsys)[6] = 'exp_i_to_j'
colnames(intsys)[7] = 'state_i_gdp'
colnames(intsys) [8] = 'state_j_gni'

#summary of variables
summary(intsys)

### RUN A MODEL ###
#make reproducible
set.seed(1)

#training and testing sets
sample <- sample(c(TRUE,FALSE), nrow(intsys), replace=TRUE, prob=c(0.7,0.3))
train <- intsys[sample,]
test <- intsys[!sample,]
#legacy - did not end up splittnig this way

#define the model
model <- glm(formula = comp_in_td ~ contiguity + inv_md + alliance + exp_i_to_j + state_i_gdp + state_j_gni + minpolity, family = "binomial", data = intsys)
options(scipen = 999)
summary(model)
summarystats <-summary(model)

#generate VIFs and check multicollinearity
vif(model)


