
###LAOS###

## set variables ##
laos= c("20N_100E","30N_100E")
inputdir="D:/ff-dev/results"
files=c("D:/ff-dev/results/20N_100E","D:/ff-dev/results/30N_100E")

## data quality check ##
quality_1 = data_quality(files[1])
print(quality_1$summary)
quality_2 = data_quality(files[2])
print(quality_2$summary)
## data preperation ##


## Run XGBoost ##


## Predict ##
