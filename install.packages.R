packages <- c("did", "readxl", "broom", "tidyverse", "panelView", "hrbrthemes", "ggplot2", "psych", "xtable")

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing)
} else {
  cat("All required packages already installed.\n")
}
