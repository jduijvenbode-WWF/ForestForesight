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
#' @param file A connection, or a character string naming the file to print to.
#' @param style Character string specifying additional style to apply.
#'   Must be a valid crayon style (e.g., "bold", "underline").
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
                   append = FALSE, file = "", style = NULL) {
  # Combine all arguments into a single string
  text <- paste(..., sep = sep)

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
  cat(text, sep = "", fill = fill, labels = labels, append = append, file = file)
}
