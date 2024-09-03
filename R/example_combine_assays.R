library(curatedTBData)
library(SummarizedExperiment)
library(sva)
geo <- c("GSE79362","GSE94438", "GSE107993", "GSE112104", "GSETornheim")
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

GSE79362 <- curatedTBData("GSE79362", dry.run = FALSE, curated.only = TRUE)
GSE94438 <- curatedTBData("GSE94438", dry.run = FALSE, curated.only = TRUE)
GSE107993 <- curatedTBData("GSE107993", dry.run = FALSE, curated.only = TRUE)
GSE112104 <- curatedTBData("GSE112104", dry.run = FALSE, curated.only = TRUE)
GSETornheim <- curatedTBData("GSETornheim", dry.run = FALSE, curated.only = TRUE)



totalNameCount <- unlist(tt[1]@colData@nrows) + tt[2]@colData@nrows + tt[3]@colData@nrows + tt[4]@colData@nrows + tt[5]@colData@nrows


totalNameCount <- GSE79362@colData@nrows + GSE94438@colData@nrows + GSE107993@colData@nrows + GSE112104@colData@nrows + GSETornheim@colData@nrows

print(totalNameCount)



dunno <- as.data.frame(colData(combined))[, c("TBStatus", "MeasurementTime", "PatientID", "PreviousTB", "Progression", "TimeToTB", "Study")]
