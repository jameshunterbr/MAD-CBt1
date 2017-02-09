## Programa para automatizar o download dos pacotes
## necessários para iniciar MAD-CB
## Author: James Hunter
## Date: 6/2/17
## Version: 1.0

pacotes <- c("tidyverse", "car", "caret", "caTools", "data.table", "DescTools",
             "e1071", "ggvis", "gmodels", "knitr", "lattice", "lubridate", 
             "nortest", "nycflights13", "outliers", "pROC", "psych", "RColorBrewer",
             "Rcpp", "readxl", "ROCR", "rpart", "rpart.plot", "seqinr", 
             "shiny", "swirl", "titanic", "yarrr")

install.packages(pacotes)
