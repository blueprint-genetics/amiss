# Running the full experiment set with docker

To obtain the prebuilt image, go to the Zenodo repository linked in the [README](../../README.md). You can load the image for use with a local docker installation using

```
docker load -i amiss_run_2.tar.gz
```

The loaded image has the tag `amiss:run_2_no_cache`.

In order to run the full set of experiments using the prebuilt image, you can use the following:

```
docker run --rm -e OPENBLAS_NUM_THREADS=1 -e AMISS_CORES=24 -v <full path to data on host system>:/amiss_data -v <full path to cloned repository on host system>/output:/amiss/output amiss:run_2_no_cache > std_out.txt &
```

The data must have been downloaded and annotated beforehand as described [here](annotation.md) and [here](cadd_data_download.md).
Make sure that the output folder (`<full path to cloned repository on host system>/output`) is writable for docker.

The two environment variables control how the code is parallelized. `OPENBLAS_NUM_THREADS` controls how many threads a single process can use to parallelize linear algebra operations. `AMISS_CORES` controls how many processes are forked to run in parallel. `OPENBLAS_NUM_THREAD` only affects the performance of linear algebra operations while `AMISS_CORES` affects most steps of the computation, so we prefer to set `OPENBLAS_NUM_THREADS` to 1 and control the parallelization with `AMISS_CORES`.

Note that running the full experiment set may take a long time. 

# Building the docker image

To build the docker image yourself, run the following command from the repository root folder:

```
docker build -t amiss:amiss .
```

This builds an image with the tag `amiss:amiss`.

# Running individual scripts locally

You can also run individual scripts. 

When run in this way, the downloaded and annotated data is expected to be in a folder called `amiss_data` and present in the same directory as the repository root (i.e. not inside the repository root, but at the same level; the same directory should contain both `amiss_data` and `amiss` directories).

## Installing prerequisites


To install the needed packages, you need to run `R/install_packages.R`. 

Note that the code was developed using R 3.6.0. In order to obtain the versions of packages used for the paper, run (in an interactive session):

```
options(repos="https://mran.microsoft.com/snapshot/2019-07-05")
source("R/install_packages.R")
```

## Parsing and preprocessing data

All experiments depend on the data having been first parsed and preprocessed:

```
Rscript R/01_parse_vcf.R
Rscript R/02_preprocess_data.R
```

The data will be in `output/data/`.

## Descriptive statistics on training set

To obtain descriptive statistics on the training set, run
```
Rscript R/03_descriptive_stats.R
```

The output files will be in `output/stats/`.

## Main experiment

To run the main experiment, first run the imputation and training script, and then the prediction and evaluation script:

```
Rscript R/04_run_impute_and_train.R
Rscript R/05_run_prediction.R
Rscript R/08_analyze_results.R
```

The trained models and logs will be in the root of `output/`, and the results will be in `output/results/`.

## Additional missingness simulation experiment

To generate the datasets with additional missingness:

```
Rscript R/06_generate_simulated_data.R
```

The generated datasets will be in `output/sim/simulated_data/`.

To run the imputation, training, prediction and evaluation workflows on the datasets:

```
Rscript R/07_run_simulations.R
```

The output will be in subfolders of `output/sim/simulated_data`, intended to be gathered with the next scripts:

```
Rscript R/09_analyze_simulation_results.R
Rscript R/10_simulations_plots.R
```

whose output will be in `output/sim/` and `output/sim/plots`, respectively.

## Cross-validation experiment

To run the cross-validation experiment:

```
Rscript R/11_cross_validation.R
```

Output will be in `output/cv`.
