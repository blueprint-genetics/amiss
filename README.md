# AMISS Framework

## Installation

### System dependencies

We tested package installation with R version 4.5.1. on Ubuntu 24.04, Arch Linux release `base-20250817.0.405639`, MacOS Sequoia 15.6 and Windows 11.

#### Linux

If using Linux, installation of dependencies may depend on some system level development packages.

On Ubuntu (tested with 24.04 Docker image):

```
sudo apt install \
	libx11-dev git libcurl4-openssl-dev \
	libssl-dev make libgit2-dev zlib1g-dev pandoc \
	libfreetype6-dev libjpeg-dev libpng-dev \
	libtiff-dev libicu-dev libfontconfig1-dev \
	libfribidi-dev libharfbuzz-dev libxml2-dev cmake
```

On Arch Linux (tested with `base-20250817.0.405639` Docker image):

```
pacman -S \
	base-devel gcc-fortran git curl openssl make cmake libgit2 zlib \
	pandoc freetype2 libxml2 harfbuzz fribidi \
	fontconfig libjpeg libpng icu libtiff
````

#### MacOS

On MacOS (tested on Sequioa 15.6), package installation should automatically download binaries and thus no additional development libraries should be necessary.

However, if you use homebrew to install R, be sure to use `--cask` to avoid having to install all packages from source:

```
brew install --cask r
```

#### Windows

On Windows (tested on Windows 11), package installation automatically downloads binaries and thus no additional development libraries should be necessary.

### Package installation using `devtools`

Using the devtools package, you can install directly from GitHub:

```
if (!require("devtools", quietly = TRUE))
  install.packages("devtools")

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("pcaMethods", ask=FALSE)
devtools::install_github("blueprint-genetics/amiss")
```

## Installation troubleshooting 

### Bad credentials error

If installation with `devtools::install_github` fails with the error below:

```
Using github PAT from envvar GITHUB_PAT
Error: Failed to install 'unknown package' from GitHub:
 HTTP error 401.
 Bad credentials
```

This suggests that there is an expired or otherwise invalid authentication token on the computer. You can either remove it as discussed on [Stack Overflow](https://stackoverflow.com/questions/70908295/failed-to-install-unknown-package-from-github) or create a new, valid authentication token as discussed on [GitHub](https://github.com/orgs/community/discussions/140956).

This repository does not require any authentication, and thus you can work also around this by passing the `auth_token=NULL` parameter to `install_github`:

```
devtools::install_github("blueprint-genetics/amiss", auth_token=NULL)
```

Alternatively, you can download the package manually and then install using `install_local`:

First clone the repository:

```
git clone https://github.com/blueprint-genetics/amiss.git
cd amiss
```

and then in R:

```
if (!require("devtools", quietly = TRUE))
  install.packages("devtools")

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("pcaMethods", ask=FALSE)
devtools::install_github(".")
```

## Data

- [Obtain and annotate ClinGen variants](docs/instructions/annotation.md)
- [Obtain additional annotations from CADD data](docs/instructions/cadd_data_download.md)

## Parameters
The framework can be configured using JSON files that determine which preprocessing and imputation steps are taken.
The file [`combination_orig.json`](combination_orig.json) in the repository contains parameters that largely match the original code of the manuscript. [`combination_minimal.json`](combination_minimal.json) produces the smallest dataset for processing and uses only zero imputation, and thus should be quickest to run. 

For different options available for parameter values, see [`parameter_grid.json`](parameter_grid.json).

## Usage

To run the framework with a single set of parameters up to computation of the classification statistics (note: this may take a long time):

```
library(amiss)
library(magrittr)
source("R/imputation_definitions.R")

create_dir("output")

# Parse 
S01_parse_vcf("clinvar_20190624.vep.vcf", cadd_snv_filename = "CADD_clingen.tsv", cadd_indel_filename = "CADD_clingen_indel.tsv", output_root_dir = "output/data", parameters_path = "combination_minimal.json")

# Preprocess
S02_preprocess_data("output/data/quality-clingen.restriction-missense.transcript-canonical", parameters_path = "combination_minimal.json", output_path = "output/data", seed=10)

# Imputation and classifier training
impute_and_train(training_path = "output/data/preprocessed_training_data.csv",
                 outcome_path = "output/data/training_outcomes.csv",
                 output_path = "output/trained",
                 mice_hyperparameter_grids = mice_hyperparameter_grids,
                 other_hyperparameter_grids = other_hyperparameter_grids,
                 single_value_imputation_hyperparameter_grids = single_value_imputation_hyperparameter_grids,
                 parameter_list=rjson::fromJSON(file = "combination_minimal.json"),
                 cores = 1, seed = 10, lean = TRUE)

# Prediction
predict_on_test_set(test_path = "output/data/preprocessed_test_data.csv",
		    training_path = "output/data/preprocessed_training_data.csv",
                    outcome_path = "output/data/test_outcomes.csv",
                    tr_output_path = "output/trained",
                    results_dir_path = "output/results",
                    parameter_list=rjson::fromJSON(file = "combination_minimal.json"),
                    seed = 10)
                    
# Result CSVs now in output/results
```


# Comparison of missing data handling methods for variant pathogenicity predictors

## Description

This project compares methods for handling missing data in variant annotations for the purpose of building variant pathogenicity predictors.

See [the paper](docs/paper/paper.pdf) for a more detailed description. 

## Results data

The result files are available in [Zenodo with DOI 10.5281/zenodo.6656616](https://doi.org/10.5281/zenodo.6656616).

## Open science

This project conforms to the principles of open science:

- Open data:
  - We use and reference publically available datasets and will citably archive any data and code we produce using [Zenodo](https://zenodo.org/) with a DOI
- Open source:
  - The source code is freely available under the [MIT license](https://github.com/blueprint-genetics/amiss/blob/master/LICENSE) at [GitHub](https://github.com/blueprint-genetics/amiss)
- Open notebook:
  - You can follow development from the start on GitHub at https://github.com/blueprint-genetics/amiss
  - The research plan is available [in the GitHub repository](https://github.com/blueprint-genetics/amiss/blob/master/docs/research_plan/research_plan.md) (see [section above](#description) for producing a PDF version)
- Open access:
  - We will upload a preprint of the resulting paper(s) on [biorXiv](https://www.biorxiv.org/)
  - We will submit the results for publication in a peer-reviewed open access journal
- Open communication:
  - We intend to present the results in public scientific conferences
- Open collaboration:
  - We welcome
    - [ideas, bug reports and comments](https://github.com/blueprint-genetics/amiss/issues)
    - [code contributions through GitHub pull requests](https://github.com/blueprint-genetics/amiss/pulls)

## Disclaimer

We license our code with the MIT license (see the [LICENSE](LICENSE) file), but note that the license of the entire system may depend on the licenses of individual libraries.

