#' Colorized Cat Function
#'
#' This function wraps the base R `cat()` function and adds color and style
#' capabilities using the crayon package.
#'
#' @param ... Arguments to be concatenated and printed.
#' @param color Character string specifying the color to apply to the text.
#'   Must be a valid crayon color (e.g., "red", "blue", "green").
#' @param sep Character string to separate the arguments when concatenating.
#' @param fill Logical. If TRUE, a newline is appended after the output.
#' @param labels Character vector of labels for the lines printed.
#' @param append Logical. If TRUE, output will be appended to the file.
#' @param style Character string specifying additional style to apply.
#'   Must be a valid crayon style (e.g., "bold", "underline").
#' @param verbose. Logical. Whether the ff_cat function should print. Is always overridden by logfile if that is set
#' @param logfile Character. Path to a logfile that should be printed to.
#' @param timestamp Logical. Whether a timestamp should be added
#' @param auto_newline Logical. Whether a newline should be added
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' ff_cat("Hello, world!")
#' ff_cat("This is red text", color = "red")
#' ff_cat("The sky is", "blue", color = "blue")
#' ff_cat("Bold green text", color = "green", style = "bold")
#' ff_cat("Line 1", "Line 2", color = "cyan", fill = TRUE)
#'
#' @import crayon
#' @export
ff_cat <- function(..., color = NULL, sep = " ", fill = FALSE, labels = NULL,
                   append = FALSE, style = NULL, verbose = FALSE, logfile = NULL, timestamp = FALSE, auto_newline = TRUE) {
  # Combine all arguments into a single string
  text <- paste(..., sep = sep)
  if (auto_newline) {
    if (!endsWith(text, "\n")) {
      text <- paste0(text, "\n")
    }
  }
  if (timestamp) {
    text <- paste(Sys.time(), text)
  }
  # Apply color if specified
  if (!is.null(color)) {
    if (color %in% names(crayon:::builtin_styles)) {
      text <- do.call(color, list(text))
    } else {
      warning("Invalid color specified. Using default.")
    }
  }

  # Apply additional style if specified
  if (!is.null(style)) {
    if (style %in% names(crayon:::builtin_styles)) {
      text <- do.call(style, list(text))
    } else {
      warning("Invalid style specified. Ignoring.")
    }
  }

  # Use cat to output the colorized text
  if (verbose) {
    cat(text, sep = "", fill = fill, labels = labels, append = append)
  }
  if (has_value(logfile)) {
    if (timestamp) {
      cat(text, sep = "", fill = fill, labels = labels, append = append, file = logfile)
    } else {
      cat(paste(Sys.time(), text), sep = "", fill = fill, labels = labels, append = append, file = logfile)
    }
  }
}
