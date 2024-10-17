#
#
# identifier <- "GAB"
# download_folder <- "D:/WWF/preprocessed"
# #
# # ff_sync(ff_folder = download_folder, identifier = identifier)
# #
#
# features <- c("temperature", "lastmonth", "confidence", "totallossalerts")
#
# country_prepped <- ff_prep_refactored(datafolder = download_folder, country = identifier, sample_size = 1, inc_features = features)
#
# country_hash <- digest(country_prepped$data_matrix, algo = "md5")
#
# cat("Country hash", country_hash, "\n")
