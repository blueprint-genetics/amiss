# Framework

## Installation

Using the devtools package, you can install directly from GitHub:

```
devtools::install_github("blueprint-genetics/amiss")
```

## Data

- [Obtain and annotate ClinGen variants](docs/instructions/annotation.md)
- [Obtain additional annotations from CADD data](docs/instructions/cadd_data_download.md)

## Parameters

The framework can be configured using JSON files that determine which preprocessing and imputation steps are taken.

The file [combination_orig.json`](combination_orig.json) in the repository contains parameters that largely match the original code of the manuscript. [`combination_minimal.json`](combination_minimal.json) produces the smallest dataset for processing and uses only zero imputation, and thus should be quickest to run. 

For different options available for parameter values, see [`parameter_grid.json`](parameter_grid.json).

## Usage

To run the framework with a single set of parameters up to computation of the classification statistics (note: this may take a long time):

```
library(amiss)

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
                    outcome_path = "output/data/test_outcomes.csv",
                    tr_output_path = "output/trained",
                    results_dir_path = "output/results",
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

