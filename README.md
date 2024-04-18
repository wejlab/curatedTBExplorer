# curatedTBExplorer
CuratedTBData is a R data package that is accessible via Bioconductor and GitHub. This program facilitates integrative studies across TB gene expression datasets by improving the reliability and replicability of signature construction and validation. For research purposes, standardized gene expression measurements and clinical annotations are currently provided by the curated TBData. MultiassayExperiment objects, which effectively manage the raw data, several normalized versions of the data, metadata, and certain intermediate analysis findings, are where these data are maintained in R. We have reprocessed all 49 datasets and created standardized workflows for data preprocessing and annotation. In addition, the curatedTBData offers capabilities for subsetting and aggregating the transcriptome datasets with currently available tools, like ComBat and ComBat-Seq. In order to showcase the usefulness of the curatedTBData, we carried out an organized analysis and verification of several published TB gene signatures derived from blood. We investigate the prediction power of TB gene signatures in relation to distinguishing PTB from LTBI and healthy individuals (Control) in the 49 carefully selected studies. To train and assess gene signature ensembles, we also leverage dataset and datatype aggregation methods that are integrated into curatedTBData and are based on our prior research. Moreover, we demonstrate the use of an ensemble learning approach that combines the predictive power of several gene signatures.

## Before Installation

* Install BiocManager and related packages
``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("DESeq2")
BiocManager::install("GenomicRanges")
BiocManager::install("MultiAssayExperiment")
BiocManager::install("SummarizedExperiment")
BiocManager::install("TBSignatureProfiler")
BiocManager::install("curatedTBExplorer")
```


## Installation

The TBSignatureProfiler requires R Version 4.1.

* Install the development version of the package from Github:

``` r
if (!requireNamespace("devtools", quietly=TRUE))
  install.packages("devtools")
devtools::install_github("wejlab/curatedTBExplorer")
```
