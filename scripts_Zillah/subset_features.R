## set environment ##
Sys.setenv("xgboost_datafolder"="D:/ff-dev/results/preprocessed")
abr ="LAO"

laos_train = ff_prep(country = abr, start="2022-01-01", end = "2022-06-01",sample_size=0.6, shrink="extract")
model_all_features = ff_train(train_matrix = laos_train$data_matrix,
                              verbose=F,
                              nrounds = 150,eta = 0.4,
                              max_depth = 6,min_child_weight = 5,
                              subsample = 0.4,gamma = 0.05)

test_all_features =  ff_prep(country = abr,  start="2023-01-01", end = "2023-05-01", sample_size=0.6, shrink="extract")
shap_sample= test_all_features$data_matrix$features[sample(nrow(test_all_features$data_matrix$features), 1000),]
shap_values = shap.values(xgb_model = model_all_features, X_train = shap_sample)

# The ranked features by mean |SHAP|
shap_values$mean_shap_score
features_ranked = names(shap_values$mean_shap_score)

shap_long <- shap.prep(xgb_model = model_all_features, X_train = shap_sample)
# **SHAP summary plot**

png("D:/ff-dev/figures/shapplotLaostt.png", width = 450, height=450)
shap.plot.summary(shap_long)+
  ggplot2::theme(text = element_text(size = 14), legend.text = element_text(size = 10))
dev.off()


F05= list()
for(i in 2:length(features_ranked)){
  train_matrix=list(features= laos_train$data_matrix$features[,features_ranked[1:i]],
                    label=laos_train$data_matrix$label)
  model_sel_features = ff_train(train_matrix = train_matrix,
                                verbose=F,
                                nrounds = 150,eta = 0.4,
                                max_depth = 6,min_child_weight = 5,
                                subsample = 0.4,gamma = 0.05)
  test_matrix=list(features= test_all_features$data_matrix$features[,features_ranked[1:i]],
                    label=test_all_features$data_matrix$label)
  results=ff_predict(model_sel_features,test_matrix,threshold=0.2,
                     groundtruth=test_matrix$label,
                     indices= test_all_features$testindices,
                     templateraster = test_all_features$groundtruthraster)
  print(paste(features_ranked[i], " is added resulting in a F05 of: ", results$F0.5))
  F05[features_ranked[i]]=results$F0.5
}
