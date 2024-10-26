install.packages("styler")
library(styler)
code <- "EngD/Test.R"
style_file(code)

# styler with option

styled_code <- style(code, width.cutoff = 80)
