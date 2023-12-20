
###LAOS###

## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results")

## set variables ##
files=c("D:/ff-dev/results/20N_100E","D:/ff-dev/results/30N_100E")

## data quality check ##
quality_1 = ff_dqc(files[1])
print(quality_1$byfeature)
quality_2 = ff_dqc(files[2])
print(quality_2$byfeature)

## data preperation ##
laos_train = ff_prep(country = "LAO", end = c(2021,12),sample_size=0.01, exc_features = c("pop2020","pop202","pop2030"))
laos_test =  ff_prep(country = "LAO", start = c(2022,6), exc_features = c("pop2020","pop202","pop2030"))

## Run XGBoost ##


## Predict ##
