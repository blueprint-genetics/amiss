
# Comparison of missing data handling methods in building variant pathogenicity metapredictors

## Description

This project will compare methods for handling missing data in variant annotations for the purpose of building variant pathogenicity metapredictors.

See [the research plan](docs/research_plan/research_plan.md) for a more detailed description.

To generate a PDF version with a working references section:

```
$ git clone https://github.com/blueprint-genetics/amiss.git
$ cd amiss/docs/research_plan
$ make
```

[Pandoc](https://pandoc.org/) and a [LaTeX distribution](https://www.latex-project.org/) must be installed.

## Manuscript draft

A draft of the manuscript that will detail materials, methods and results, can be found [here](docs/paper/paper.pdf).

## Preliminary results

Performance of downstream classifiers, grouped by imputation method:

- [MCC, random forest classifiers](results/rf_mcc_boxplots.pdf)
- [MCC, logistic regression classifiers](results/lr_mcc_boxplots.pdf)
- [AUC-ROC, random forest classifiers](results/rf_roc_boxplots.pdf)
- [AUC-ROC, logistic regression classifiers](results/lr_roc_boxplots.pdf)

Stochastic imputation methods were each run to produce 10 completed training sets and 10 completed test sets. A classifier was fit on each completed training set, producing 10 models, and each was evaluated on each completed test set. Deterministic methods were run once on both.

- The top plot in each file shows the 100 performance estimates (10 models times 10 completed test sets)
- The middle plot shows the distribution of means computed over the completed test sets (i.e. mean generalization performance with model fixed). This shows how performance varies due to variance in stochastic test set imputation.
- The bottom plot shows the distribution of means computed over the completed training sets (i.e. mean generalization performance with completed test set fixed). This shows how performance varies due to variance stochastic in training set imputation.

## Instructions

To reproduce the main results: (roughly estimated execution times in parentheses)

Note: the following instructions assume that you clone the repository into your home folder. Remember to adjust the paths if another base directory is used.


1. Clone the git repository with `git clone https://github.com/blueprint-genetics/amiss.git`
2. [Acquire and annotate variant data](docs/instructions/annotation.md) (~3 h)
3. [Acquire CADD annotations](docs/instructions/cadd_data_download.md) (300 GB download!)
4. Using the repository root as working directory, run the scripts in order:
    - 01_parse_vcf.R (<1 hour)
    - 02_1_expand_data.R (<1 min)
    - 02_2_contract_data.R (<1 min)
    - optional: use Rstudio to view 03_descriptive_stats.Rmd ([knitted pdf here](R/03_descriptive_stats.pdf)) (1-5 min)
    - 04_impute_and_train.R (tested with 24 cores available: ~24 hours)
    - 05_test_prediction.R (1-2 hours)
    - TODO: 06_analyze_main_results.R

5. To reproduce simulation results, run also
    - WIP: 07_generate_simulated_data.R
    - TODO: 08_run_simulations.R
    - TODO: 08_analyze_results.R

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
