library(curatedTBData)
library(SummarizedExperiment)
library(sva)
geo <- c("GSE112104", "GSE107994")
tt <- curatedTBData(geo, dry.run = FALSE, curated.only = TRUE)
# Combine assays from multiple experiments
combined <- combine_objects(tt, experiment_name = "assay_curated")
# Perform batch correction on the combined assay
my_formula <- "~ Age + Gender + TBStatus"
mod <- model.matrix(as.formula(my_formula), colData(combined))

assay(combined, "corrected_assay") <- ComBat(assay(combined, "assay1"), 
                                              batch = colData(combined)$Study, 
                                              mod = mod)
combined
