ff_accuracyreport <- function(accuracy_paths, importance_paths=NULL, output_path, title = "Accuracy Analysis: Forest Foresight") {
  # Load required data
  for (i in accuracy_paths) {
    if (i == accuracy_paths[1]) { results <- read.csv(i)}else{results <- rbind(results,read.csv(i))}
  }

  pols <- terra::vect(get(data("degree_polygons")))

  # Prepare data
  results$UUID <- paste0(results$iso3, "_", results$coordname)
  names(results)[which(names(results) == "name")] <- "country"
  pols$UUID <- paste0(pols$iso3, "_", pols$coordname)

  # Helper function to calculate metrics
  calculate_metrics <- function(TP, FP, TN, FN) {
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    F0.5 <- (1.25 * precision * recall) / (0.25 * precision + recall)
    events <- TP + FN
    return(c(precision = precision, recall = recall, F0.5 = F0.5, events = events))
  }

  # Aggregate by date
  results_by_date <- aggregate(cbind(TP, FP, TN, FN) ~ date, data = results, FUN = sum)
  metrics_by_date <- as.data.frame(t(apply(results_by_date[, c("TP", "FP", "TN", "FN")], 1, function(row) calculate_metrics(row[1], row[2], row[3], row[4]))))
  names(metrics_by_date) <- sapply(names(metrics_by_date), function(x) gsub(".TP", "", x)[[1]][1])
  results_by_date <- cbind(results_by_date, metrics_by_date)
  results_by_date$date <- as.Date(results_by_date$date)

  # Aggregate by UUID
  results_by_UUID <- aggregate(cbind(TP, FP, TN, FN) ~ UUID, data = results, FUN = sum)
  metrics_by_UUID <- t(apply(results_by_UUID[, c("TP", "FP", "TN", "FN")], 1, function(row) calculate_metrics(row[1], row[2], row[3], row[4])))
  results_by_UUID <- cbind(results_by_UUID, metrics_by_UUID)

  # Merge spatial data
  spatialdata <- merge(pols, results_by_UUID, by = "UUID")
  names(spatialdata) <- c(names(spatialdata)[1:11], "precision", "recall", "F05", "events")
  spatialdata$F05 <- as.numeric(spatialdata$F05)
  spatialdata <- spatialdata[!is.nan(spatialdata$F05), ]

  # Change file extension to .png
  output_path <- sub("\\.pdf$", ".png", output_path)

  # Start PNG device
  png(output_path, width = 16.5, height = 11.7, units = "in", res = 300)

  # Set up layout for multiple plots
  if (!is.null(importance_paths)) {
    layout(matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2, byrow = TRUE))
  } else {
    layout(matrix(c(1, 2), nrow = 1, ncol = 2, byrow = TRUE))
  }

  # Plot 1: F0.5 Score Distribution Map
  par(mar = c(5, 4, 4, 2) + 0.1)
  col_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
  maxf05 <- max(spatialdata$F05, na.rm = TRUE) + 0.05
  minf05 <- min(spatialdata$F05, na.rm = TRUE) - 0.05
  breaks <- seq(minf05, maxf05, length.out = 10)
  plot(spatialdata, "F05",
       main = "F0.5 Score Distribution",
       col = col_palette,
       border = "#00000000",
       breaks = breaks,
       legend = TRUE)

  # Plot 2: Metrics Over Time
  par(mar = c(7, 5, 4, 5) + 0.1)  # Increased bottom margin for rotated labels

  # Calculate y-axis limits and breaks
  max_events <- max(results_by_date$events)
  y_breaks <- pretty(c(0, max_events), n = 10)

  # Plot events bars
  plot(results_by_date$date, results_by_date$events,
       type = "h", col = "lightgrey", lwd = 10,
       xlab = "", ylab = "",
       main = "Precision, Recall, and F0.5 Over Time",
       ylim = c(0, max(y_breaks)),
       axes = FALSE)

  # Add axes and labels
  axis(2, at = y_breaks, labels = format(y_breaks, scientific = FALSE, big.mark = ","), las = 1)
  mtext("Number of Events", side = 2, line = 3.5, bg="white")

  # Rotate x-axis labels 45 degrees
  axis(1, at = results_by_date$date, labels = FALSE)
  text(x = results_by_date$date, y = par("usr")[3] - 0.05 * (par("usr")[4] - par("usr")[3]),
       labels = format(results_by_date$date, "%Y-%m-%d"),
       srt = 45, adj = 1, xpd = TRUE, cex = 0.7)

  # Add right y-axis
  axis(4, at = seq(0, 1, 0.1)*max_events, labels = sprintf("%.1f", seq(0, 1, 0.1)), las = 1)
  mtext("Metric Value", side = 4, line = 3.5)

  # Add x-axis label
  mtext("Date", side = 1, line = 5.5)

  # Plot lines for precision, recall, and F0.5
  scalfac <- max_events
  lines(results_by_date$date, results_by_date$precision * scalfac, col = "blue", lwd = 2)
  lines(results_by_date$date, results_by_date$recall * scalfac, col = "red", lwd = 2)
  lines(results_by_date$date, results_by_date$F0.5 * scalfac, col = "green", lwd = 2)

  # Add points
  points(results_by_date$date, results_by_date$precision * scalfac, col = "blue", pch = 16)
  points(results_by_date$date, results_by_date$recall * scalfac, col = "red", pch = 16)
  points(results_by_date$date, results_by_date$F0.5 * scalfac, col = "green", pch = 16)

  # Add legend
  legend("topright", legend = c("Precision", "Recall", "F0.5", "Events"),
         col = c("blue", "red", "green", "lightgrey"),
         lty = c(1, 1, 1, 1), lwd = c(2, 2, 2, 10),
         pch = c(16, 16, 16, NA))

  # Plot 3: Model Importance (if importance_paths is provided)
  if (!is.null(importance_paths)) {
    for (i in importance_paths) {
      if (i == importance_paths[1]) { importance_results <- read.csv(i)}else{results <- rbind(importance_results,read.csv(i))}
    }
    if(length(importance_paths)>1){
      model_names =  paste(unique(importance_results$model_name),collapse = ", ")
      avg_importance <- aggregate(importance ~ feature, data = importance_results, FUN = mean)
      avg_importance$rank <- rank(-avg_importance$importance, ties.method = "first")
      avg_importance <- avg_importance[order(avg_importance$rank), ]
      importance_results = data.frame(model_name=model_names,feature=avg_importance$feature,rank=avg_importance$rank,importance=avg_importance$importance)
    }
    par(mar = c(5, 20, 4, 2))  # Adjust margins (bottom, left, top, right)
    importance_results <- importance_results[nrow(importance_results):1, ]
    barplot(importance_results$importance,
            horiz = TRUE,
            names.arg = importance_results$feature,
            las = 1,  # Make y-axis labels horizontal
            cex.names = 0.7,  # Adjust size of feature names
            cex.axis = 0.8,  # Adjust size of x-axis labels
            col = "lightgreen",
            xlab = "Importance",
            cex.lab = 1.2,  # Increase size of x-axis label
            main = importance_results$model_name[1],  # Use the first model name as title
            cex.main = 1.5,  # Increase size of title
            xlim = c(0, max(importance_results$importance) * 1.05))  # Extend x-axis slightly
  }

  # Add title to the entire page
  mtext(title, outer = TRUE, line = -2, cex = 1.5)

  # Close PNG device
  dev.off()
}