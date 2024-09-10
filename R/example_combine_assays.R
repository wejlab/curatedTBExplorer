library(curatedTBData)
library(SummarizedExperiment)
library(sva)

# All Datasets:
all <- c("GSE31348", "GSE36238", "GSE41055", "GSE54992",
         "GSE73408", "GSE107731", "GSE79362", "GSE84076",
         "GSE89403", "GSE94438", "GSE107991", "GSE107992",
         "GSE107993", "GSE107994", "GSE101705", "GSE107104",
         "GSE112104", "GSETornheim", "GSE19435", "GSE19439",
         "GSE19442", "GSE19443", "GSE19444", "GSE22098",
         "GSE29536", "GSE37250", "GSE39939", "GSE39940",
         "GSE40553", "GSE42825", "GSE42826", "GSE42827",
         "GSE42830", "GSE42831", "GSE42832", "GSE50834",
         "GSE56153", "GSE69581", "GSE83456", "GSE83892",
         "GSEBruno", "GSE25534", "GSE28623", "GSE34608",
         "GSE62147", "GSE81746", "GSE62525", "GSE74092",
         "GSE6112", "GSE152218")

# Progression Datasets:
progression <- c("GSE79362","GSE94438", "GSE107993", "GSE112104", "GSETornheim")


# Perform batch correction on the combined assay
# my_formula <- "~ Age + Gender + TBStatus"
# mod <- model.matrix(as.formula(my_formula), colData(combined))

assay(combined, "corrected_assay") <- ComBat(assay(combined, "assay1"),
                                              batch = colData(combined)$Study,
                                              mod = mod)
combined

# This is the combined set of column data with progressor relevant information.
dunno <- as.data.frame(colData(combined))[, c("TBStatus", "MeasurementTime", "PatientID", "PreviousTB", "Progression", "TimeToTB", "Study")]
