# cpue.rfmo

An R package for standardizing catch-per-unit-effort (CPUE) data for Regional Fisheries Management Organizations (RFMOs).

## Overview

`cpue.rfmo` provides a comprehensive toolkit for analyzing longline fisheries data from multiple regional fisheries management organizations including:

- **IOTC** (Indian Ocean Tuna Commission)
- **ICCAT** (International Commission for the Conservation of Atlantic Tunas)  
- **IATTC** (Inter-American Tropical Tuna Commission)

The package standardizes CPUE data across different fleets (Japanese, Korean, Taiwanese, Seychelles, Chinese, Brazilian, and others) and supports analysis of multiple tuna and billfish species.

## Installation

Install the development version from GitHub:

```r
# Install devtools if not already installed
if (!require(devtools)) install.packages("devtools")

# Install cpue.rfmo from GitHub
devtools::install_github("hoyles/cpue.rfmo")

# Load the package
library(cpue.rfmo)
```

**Note:** The package may produce warnings during loading (usually 19) but these can be safely ignored.

## Key Features

### Data Preparation and Cleaning
- Fleet-specific data preparation functions (`dataprep_JPIO`, `dataprep_TW`, `dataprep_SY`, `dataprep_CNIO`, `dataprep_TW_EPO`)
- Data cleaning and validation (`dataclean_JPIO`, `dataclean_CNIO`)
- Regional structure setup (`setup_IO_regions`, `setup_AO_regions`)
- Trip identification and clustering (`make_lbidmon`, `make_clid`)
- Date/time processing (`makedmy`)
- Weight and aggregation functions (`mk_wts`, `aggregate_data`)

### Statistical Modeling
- Formula generation for different model types (`make_formula_IO`, `make_formula_IOx`, `make_formula_AO`)
- Delta-lognormal modeling (`do_deltalog`)
- Support for multiple modeling approaches:
  - Log-normal models (`summarize_and_store`)
  - Delta-lognormal models (`summarize_and_store_dl`)
  - Negative binomial models
  - Quasi-Poisson models
- Model summarization and storage (`summarize_and_store`, `summarize_and_store_dl`)

### Species Composition Clustering and PCA Analysis
- Cluster analysis based on species composition (`clust_PCA_run`)
- Principal Component Analysis (PCA) on catch composition
- Multiple clustering methods with species catch ratios
- Cluster assignment and validation
- PCA plotting and interpretation (`plotPCA = TRUE`)

### Visualization and Reporting
- CPUE index plotting (`doplot_cpue`, `doplot_yr_cpue`)
- Window setup for plots (`make_index_windows`)
- Model diagnostic plots and figures
- Results preparation and summarization (`prep_indices`, `summarize_and_store`)
- Index compilation across regions and model types
- Figure generation with standardized formatting and saving

## Supported Species

The package supports analysis of major tuna and billfish species:

- **Albacore** (ALB)
- **Bigeye Tuna** (BET)  
- **Yellowfin Tuna** (YFT)
- **Southern Bluefin Tuna** (SBT)
- **Skipjack Tuna** (SKJ)
- **Swordfish** (SWO)
- **Blue Marlin** (BLM)
- **Striped Marlin** (MLS)
- **Sailfish** (SAS)
- **Sharks** (SHK)
- And others

## Usage Example

```r
library(cpue.rfmo)

# Load and prepare Japanese longline data
dat <- read.csv("your_data.csv")
prepdat <- dataprep_JPIO(dat)
prepdat <- setup_IO_regions(prepdat, regY=TRUE, regB=TRUE, regA=TRUE)

# Create trip identifiers
dat <- make_lbidmon(prepdat)

# Run clustering analysis
cluster_results <- clust_PCA_run(
  r = 1, 
  ddd = dat, 
  allsp = c("alb", "bet", "yft"), 
  regtype = "regA", 
  ncl = 4
)

# Generate CPUE model formula
formula <- make_formula_IO(
  runsp = "bet", 
  modtype = "logn", 
  dohbf = TRUE, 
  addboat = TRUE, 
  addcl = TRUE
)

# Fit model and generate plots
model <- glm(formula, data = dat, family = "gaussian")
doplot_cpue(results, vartype = "lognC", mdti = "Model 1", 
            regstr = "Region A", runreg = 1)
```

## Dependencies

The package requires several R packages:

```r
# Core dependencies
library(splines)
library(lunar)
library(lubridate)
library(dplyr)
library(data.table)

# For mapping and visualization
library(maps)
library(mapdata) 
library(maptools)

# For statistical modeling
library(mgcv)
library(randomForest)
library(cluster)
library(nFactors)
```

## Regional Structures

The package supports multiple regional stratification schemes:

- **Indian Ocean regions**: regY, regY1, regY2, regB, regB1, regB2, regB3, regA, regA1-regA5
- **Atlantic Ocean regions**: regB (bigeye regions)
- **Pacific Ocean regions**: Various EPO regional structures

## File Structure

```
cpue.rfmo/
├── R/
│   ├── dataprep_functions.R
│   ├── regional_functions.R
│   ├── cluster_functions.R
│   ├── modeling_functions.R
│   ├── plotting_functions.R
│   ├── reporting_functions.R
│   └── summarize_store_functions.R
├── inst/
│   └── CITATION
├── man/
│   └── [function documentation]
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

## Contributing

This package is developed for scientific research in fisheries management. If you encounter issues or have suggestions for improvements, please contact the package maintainer.

This package is developed for scientific research in fisheries management. If you encounter issues or have suggestions for improvements, please contact the package maintainer.

## Citation

To cite the **cpue.rfmo** package in publications, please use:

Hoyle, S.D. (2025). *cpue.rfmo: An R Package for CPUE Standardization in Regional Fisheries Management Organizations*. R package version 0.1.0. Available at: https://github.com/hoyles/cpue.rfmo

You can generate a citation in R with:

```r
citation("cpue.rfmo")
```

## License

This package is distributed under the Creative Commons License (CC BY-NC-SA 4.0). You are free to share and adapt the material for non-commercial purposes, provided you give appropriate credit and distribute under the same license.

## Contact

For questions about the package or methodological approaches, please refer to the RFMO working group documentation or contact the package maintainer through the GitHub repository.

---

**Note**: This package is designed for scientific research and fisheries management applications. Users should be familiar with CPUE standardization methodologies and the specific data formats used by different RFMOs.