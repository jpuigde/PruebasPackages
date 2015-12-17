install.packages("radiant", repos = "http://vnijs.github.io/radiant_miniCRAN/", type = 'binary')
devtools::install_github("vnijs/radiant")

require(radiant)
radiant("analytics")
radiant("base")
radiant("quant")
radiant("marketing")


