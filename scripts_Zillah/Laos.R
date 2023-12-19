
###LAOS###

## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results")

## set variables ##
laos= c("20N_100E","30N_100E")
files=c("D:/ff-dev/results/20N_100E","D:/ff-dev/results/30N_100E")

## data quality check ##
quality_1 = ff_dqc(files[1])
print(quality_1$summary)
quality_2 = ff_dqc(files[2])
print(quality_2$summary)

## data preperation ##
laos_train = ff_prep(country = "LAO", tiles=laos, end = c(2021,12), exc_features = c("pop2020","pop202","pop2030"), validation_sample = 0.1)

laos_test =  ff_prep(country = "LAO", tiles=laos, start = c(2022,6), end = c(2023,5), exc_features = c("pop2020","pop202","pop2030"))

## Run XGBoost ##


## Predict ##