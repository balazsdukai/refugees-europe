# Tree map of origin–destination
library(treemap)
a <- read.csv("~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/work/origin.csv")

treemap(a[7:36,], index = c("origin", "destination"),
        vSize = "percent", vColor = "destination", type = "categorical", palette="Paired")

