## set environment ##

library(ForestForesight)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
data("countries")
countries=vect(countries)
#groups=unique(countries$group)
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")
groups = c("Lao People's Democratic Republic","Middle Africa 1", "Colombia","Peru", "Bolivia")
#dater=c(daterange("2022-07-01","2023-01-01"))
dates =daterange("2023-01-01","2023-05-01")
label_thresholds = c(1)
for(label_threshold in label_thresholds){
  cat("starting label threshold ",label_threshold,'\n')
  exp_name = paste0("threshold_",label_threshold,"_noXY_18months")
  for(group in groups){
    tryCatch({
      cat(" starting group ",group)
      countriessel=countries$iso3[which(countries$group==group)]
      traindata=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",
                        country=countriessel,start ="2021-01-01",end="2022-06-01",
                        fltr_features = "landpercentage",fltr_condition = ">0",
                        sample_size = 0.1,verbose=F,shrink="extract",
                        label_threshold = label_threshold,addxy=F,
                        groundtruth_pattern = "groundtruth6m")
      if(!dir.exists(file.path("D:/ff-dev/predictionsZillah/models/",group))){dir.create(file.path("D:/ff-dev/predictionsZillah/models/",group))}
      model=ff_train(traindata$data_matrix,eta = 0.2,gamma = 0.2,min_child_weight = 3,max_depth = 6,nrounds = 100,subsample = 0.3,verbose=F,
                     modelfilename = file.path("D:/ff-dev/predictionsZillah/models/",group,paste0(group,"_",exp_name,".model")),features=traindata$features)
      for(country in countriessel){
        cat("starting country ",country)
        for(dr2 in dates){
          cat(" starting date ",dr2)
          tiles=gfw_tiles[countries[countries$iso3==country],]$tile_id
          for(tile in tiles){
            cat(" starting tile ",tile)
            predset=ff_prep(datafolder = "D:/ff-dev/results/preprocessed/",tiles=tile,start = dr2,verbose=F,fltr_features = "landpercentage",fltr_condition = ">0",addxy=F,label_threshold = label_threshold)
            print("predict")
            prediction=ff_predict(model,test_matrix = predset$data_matrix,indices = predset$testindices,
                                  templateraster = predset$groundtruthraster,groundtruth = predset$groundtruth,verbose = F)
            #if(!dir.exists(file.path("D:/ff-dev/results/predictions/",country))){dir.create(file.path("D:/ff-dev/results/predictions/",country))}
            print("analyze")
            ff_analyze(prediction$predicted_raster,groundtruth = predset$groundtruthraster,
                       csvfile = paste0("D:/ff-dev/predictionsZillah/accuracy_analysis/", exp_name,".csv")
                       ,tile = tile,date = dr2,return_polygons = F,append = T,country=country,verbose=T)
          }
        }

      }

    }, error = function(e) {
      cat("Error occurred:", conditionMessage(e), "\n")
    })
  }

}


# Create an empty data frame to store results
result_df <- data.frame(File = character(),
                        Group = character(),
                        Precision = numeric(),
                        Recall = numeric(),
                        F05 = numeric(),
                        stringsAsFactors = FALSE)

# Analyse files
files <- list.files("D:/ff-dev/predictionsZillah/accuracy_analysis")

for (file in files[6]) {
  data <- read.csv(paste0("D:/ff-dev/predictionsZillah/accuracy_analysis/", file))
  print(unique(data$date))
#  sel_dates= unique(data$date)[9:13]
#  data= data[data$date==sel_dates,]
  FP <- sum(data$FP)
  TP <- sum(data$TP)
  FN <- sum(data$FN)
  pr <- TP / (TP + FP)
  re <- TP / (TP + FN)
  F05 <- 1.25 * pr * re / (0.25 * pr + re)

  # Add overall results to the data frame

  result_df <- rbind(result_df, c(file, "Overall", round(pr, 3), round(re, 3), round(F05, 3)))

  # Analyse groups
  for (group in unique(data$name)) {
    data_g <- data[data$name == group, ]
    FP_g <- sum(data_g$FP)
    TP_g <- sum(data_g$TP)
    FN_g <- sum(data_g$FN)
    pr_g <- TP_g / (TP_g + FP_g)
    re_g <- TP_g / (TP_g + FN_g)
    F05_g <- 1.25 * pr_g * re_g / (0.25 * pr_g + re_g)

    # Add group results to the data frame
    result_df <- rbind(result_df, c(file, group, round(pr_g, 3), round(re_g, 3), round(F05_g, 3)))
  }
}

# Print the resulting data frame
colnames(result_df) <- c("File", "Group", "Precision", "Recall", "F05")

print(result_df)
print(result_df[result_df$Group=="Overall",])
