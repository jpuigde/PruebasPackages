# library(devtools)
# install_github("ramnathv/htmlwidgets") 
# install_github("smartinsightsfromdata/rpivotTable")
## Load rpivotTable
library(rpivotTable)
indicadors <- read.csv("cheking_indicadores.csv")
load("cheking_indicadores.RData")
data(mtcars)
## One line to create pivot table
rpivotTable(cheking_indicadores,  aggregatorName="Sum",cols ="id_version",rows="tar_ind_finmes_en_curso",vals="recuento",rendererName="Table")


names(cheking_indicadores)
