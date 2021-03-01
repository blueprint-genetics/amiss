# Materials and methods

We implement a framework that enables comparison of imputation methods by their contribution to the performance of machine-learning classifiers, specifically for prediction of variant pathogenicity. For this purpose, the framework preprocesses variant data to a usable format, performs imputation, trains a classifier, and computes relevant performance statistics.

This framework is used to perform three experiments:

1. a simulation experiment, where additional missing values are induced in the dataset several times, and the framework is used on each resulting dataset, described in subsection \ref{simulations},
2. a cross-validation experiment, where the framework is used on datasets produced using repeated random sub-sampling, described in subsection \ref{crossvalidation}, and 
3. a main experiment, where the framework is used once with more comprehensive hyperparameter ranges for imputation methods, described in subsection \ref{mainexp}.

## Data

### ClinGen dataset

The dataset consists of ClinGen[@clingen] expert-reviewed single-nucleotide variants from ClinVar[@clinvar2013; @clinvar2016; @clinvar2017], downloaded on 28 June 2019. We annotated the variants using the Ensembl Variant Effect Predictor (VEP)[@vep] version $96$ with Ensembl transcripts and dbNSFP3.5 [@dbnsfp1; @dbnsfp3]. We selected transcripts annotated canonical by VEP for each variant and removed the other transcripts, and where values were present for several transcripts, discarded values unrelated to the selected transcript. 
Any variants whose canonical VEP-annotated transcript ID did not match that from dbNSFP were discarded.
In addition, we incorporated annotations used by CADD [@cadd2018], matching them by transcript to the VEP-annotated Ensembl transcripts. At this stage, the dataset contained $12282$ rows.

## Preprocessing {#preprocessing}

The overall preprocessing process is depicted in Figure \ref{figure:preprocessing}. 

![Preprocessing diagram. Data processing is divided into annotation, data parsing and preprocessing steps. Some preprocessing actions are deferred to the training phase, as their results may change due to simulated additional missing values.\label{figure:preprocessing}](preprocessing.pdf){height=100%, width=100%}

The feature vectors of some sets of variants may be equal (that is, duplicated). We removed variants with duplicated feature vectors (N$=320$), retaining only one variant from each equivalence class formed by duplicated feature vectors. We also chose to drop variants of uncertain significance (VUS, N $=1157$) from the dataset.

The data was then randomly split into training and test subsets, with $70~\%$ (N$=7536$) of variants in the training set and $30~\%$ (N$=3269$) of variants in the test set.

We formed a binary outcome vector by defining variants classified as pathogenic or likely pathogenic to belong to the positive class (N $=5090$ in the training set, N $=2218$ in the test set), and variants classified as benign or likely benign to belong to the negative class (N $=2446$ in the training set, N $=1051$ in the test set).

Categorical features were transformed to dummy variables, with an extra category denoting a missing value. It is important to note that thus no imputation methods designed specifically for categorical features were tested, and categorical features were not treated by the used numerical imputation methods.

Use of categorical features with high class imbalance within certain levels may obfuscate the performance of the imputation methods. This is due to allowing the classifier to learn to classify all variants with that level into either the positive or the negative class, and therefore ignoring all other features upon which imputation may have been performed. VEP-predicted variant consequence is one such feature. For this reason, we removed variants with consequences for which either class had less than $5~\%$ of overall variants of that consequence (removing $4930$ variants from the training and $2139$ variants from the test sets). A prediction tool developer might prefer to not make such restrictions to retain wide applicability, but it is important that they carefully analyze their results. They should especially pay attention to whether their method is significantly better than simple assignment to the majority class in such cases.  
For features where the missingness implied the default value *a priori*, missing values were replaced by default values.

--------------------------------------------------------------------------------
Feature name        Feature interpretation                      Default value
-------------       -----------------------                     --------------
`motifDist`         "Reference minus alternate allele           $0$
                    difference in nucleotide frequency 
                    within an predicted overlapping motif" 
                    [@cadd2014, Supplementary information]    

`gnomAD_exomes_AF`  gnomAD allele frequency from exomes         $0$

`gnomAD_genomes_AF` gnomAD allele frequency from genomes        $0$
--------------------------------------------------------------------------------

Table: Features imputed with default values.


The final processed dataset contains $3736$ variants divided into a training set with a total of $2606$ variants, of which $1088$ belong to the positive class and $1518$ belong to the negative class, and a test set with a total of $1130$ variants, of which $476$ belong to the positive class and $654$ belong to the negative class.

To minimize issues due to singular matrices with some imputation methods (e.g. MICE linear regression, MICE predictive mean matching), we removed features with fewer than $1~\%$ unique values. For feature pairs with high correlation (Pearson correlation coefficient $> 0.9$), we kept only one of the features. Removal of features with few unique values or high correlation is performed as part of the training process, just before imputation, since they may be affected by introduction of additional missing values for simulations (see [Simulations](#simulations)). It is possible that some imputation methods would be able to deal with feature sets for which the above treatment was not done, and gain an advantage due to the additional information. However, we chose to use the same restricted feature set for all imputation methods for simplicity and comparability.

A mock-up illustrating the preprocessed data format is shown in Table \ref{mockup}.

---------------------------------------------------------------------------------------------------------------------------------------------------------
Variant                                SIFT        DNase-seq levels    gnomAD AF               mirSVR              Non-synon.      Intronic          $\dotsc$
------------------------------------   ---------   ----------          ----------              ---------           --------        --------          -------
1:215625828:T:C                        NA          $0.3126$            $0.0$                   NA                  $0$             $0$                  

2:39022774:T:C                         $0.001$     $0.2607$            $4.070\mathrm{e}{-06}$  NA                  $1$             $0$                  

2:47410217:G:A                         $0.033$     $0.3818$            $0.0$                   NA                  $1$             $0$                  

2:47797737:C:T                         NA          $0.5263$            $0.0$                   NA                  $0$             $1$                  

13:32399139:A:G                        NA          $0.0750$            $0.0$                   $-0.1513$           $0$             $0$                  

---------------------------------------------------------------------------------------------------------------------------------------------------------

Table: Mockup of data after preprocessing\label{mockup}. Variant identification information is not used in imputation or training. gnomAD allele frequency has a value exactly $0$ on rows $1$, $3$, $4$, and $5$ due to *a priori* imputation.

### Features

The initial feature set was defined manually to include a variety of traditional tools and annotations from both dbNSFP3.5 and the annotation set for CADD while excluding any metapredictors from the feature set. See supplementary information.

[^supp]: Add the stats to supplementary docs & Fix reference when numbers for supplementary materials are fixed

### Missingness

Missingness percentages of the features conditional on the predicted variant or transcript[^which] consequence are presented in Figure \ref{heatmap}. 
As expected, `INTRONIC`, `UPSTREAM`, `DOWNSTREAM`, `NON-CODING_CHANGE` and `5PRIME_UTR` variants have the most missingness, exhibiting large missingness percentages across protein-related features and splicing predictions, leaving mainly features related to regulation[^biocheck]. `INFRAME` variants have similar pattern as previously described variants, but have observed values in features describing variant position in coding sequence and protein codon. Such features are partially observed in `SPLICE_SITE` variants, possibly due to some of them also being interpretable as coding variants. `SPLICE_SITE` variants have a feature describing the distance to a splice site completely observed, but curiously, it is `3PRIME_UTR` variants instead that have observations of splicing predictions[^splice].  `NON-SYNONYMOUS` variants have, as expected, nearly completely observed feature vectors, except for mostly unobserved splicing predictions and incompletely observed gnomAD allele frequencies, MutPred predictions and REMAP [@remap] annotations.

Since many features are only applicable to variants of a specific consequence, there will be few complete cases.
Further, the only complete cases can be formed by variants that are interpretable as having several consequences, even if only one is assigned for the purposes of analysis. Such variants are, for example, splicing variants. They can often also be interpreted to be non-synonymous or intronic depending on their position[^biocheck]. The complete cases are thus the very few variants that have both splicing predictions as well as the features available for non-synonymous variants.

[^splice]: ask somebody about this

[^biocheck]: check with somebody that this is accurate

![Missingness heatmap of the training set\label{heatmap}. Each cell displays the proportion of missing values of the indicated feature (horizontal axis) in the predicted molecular consequence class (vertical axis).](figures/missingness_heatmap.pdf){height=100%, width=100%}

[^which]: Which? Kind of both?

To assess whether there is informative missingness in the data, we computed each feature's correlation to the outcome indicator (see Supplementary information). For some features, there is moderately large correlation, the largest of which is for MutPred ($\approx -0.33$).

## Imputation methods

### Univariate imputation

The simplest imputation methods impute every missing value within a feature with the same value, which may either be a constant or a statistic estimated from the observed values of the feature. These methods are called univariate imputation methods.

In the case of missingness indicators, features with identical missingness patterns produce identical indicator vectors, of which only one is kept.

### Multiple imputation by chained equations

Multiple imputation by chained equations (MICE) [@vanbuuren2006; @vanbuuren2007; @mice] is an algorithm that iteratively imputes single features conditional on other features. In short, a MICE method

1. uses univariate imputation to sequentially impute each feature conditional on the observed values of other features 
2. reimputes each feature conditional on the imputed data from the previous iteration

Step 2 is repeated until some maximum number of iterations or some measure of convergence is reached.

We used the R `mice` package [@mice] to perform the imputation.
Each method was run with maximum $10$ iterations.

### Other imputation methods {#other}

Besides unconditional univariate methods and MICE methods, we tested three popular imputation methods: k-Nearest Neighbors (k-NN), Bayesian Principal Components Analysis (BPCA) [@bpca] and missForest [@missforest].

For k-NN, you must have enough complete cases to start imputation, depending on $k$. As described above, the data had very few complete cases, and thus the largest $k$ that could be used was $2$.

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
Imputation method                      Description                                                          Implementation
--------------------------             -------------------------------------------------------------        ---------------------------------------
Zero imputation                        Replace by $0$                                                       Custom

Maximum imputation                     Replace by maximum observed value within feature                     Custom

Minimum imputation                     Replace by minimum observed value within feature                     Custom

Median imputation                      Replace by median observed value within feature                      Custom

Mean imputation                        Replace by mean observed value within feature                        Custom

Outlier imputation                     Given observed values $F_{obs}$ of feature $F$,                      Custom
                                       replace by $|\text{max}(F_{obs})-\text{min}(F_{obs})|\cdot 10$           

Missingness indicator augmentation     For each feature, perform zero imputation                            Custom
                                       and create a binary feature 
                                       indicating original missing values

Predictive mean                        Sampling from observed values similar to a predicted value           `pmm` in package `mice`
matching               

Random forest                          Predict values using random forest                                   `rf` in package `mice`

Linear regression                      Predict values from a linear regression model                        `norm.predict` in package `mice`

Bayesian linear                        Predict values from a linear regression model with added             `norm` in package `mice`
regression                             uncertainty modeling noise and variability of parameter estimates

BPCA [@bpca]                           Predict values via Bayesian principal components analysis            `bpca` in package `pcaMethods`[@pcaMethods]

k-NN                                   Predict values as mean of $k$ nearest neighbors wrt. other features  `knnImputation` in package `DMwR` [@DMwR]

MissForest [@missforest]               Predict values using random forest                                   `missForest` in package `missForest` [@missforestpkg]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

Table: Used imputation methods


## Framework

The framework performs imputation and classifier training and evaluation on a preprocessed training and test dataset pair. The preprocessed training set is used to filter out features that have insufficiently unique values or that are heavily correlated with some other feature, and this filtering is matched on the test set. 

Each imputation method is used on the training set, producing at least one imputed dataset for each combination of hyperparameters (see below), and classifiers are trained on each dataset.  

Multiple imputation methods can be used to produce several imputed datasets using the same hyperparameter configuration, and we make use of this in the main experiment. For probabilistic single imputation methods, the method can be run multiple times with different seeds, producing a set of imputed datasets analogous to that generated via multiple imputation. We apply this approach to MissForest.  
For each completed dataset from a probabilistic or multiple imputation method, we train a separate classifier (performing its usual hyperparameter search and model selection procedure separately on each dataset). 

To maximize the performance of each imputation method for fair comparison, hyperparameter grids were defined for each method for which different hyperparameters[^hyper] could be passed. For the simulation and cross-validation experiments we decided to save time by using fewer hyperparameter configuration, sampling a maximum of 8 hyperparameter configurations for each imputation method used on a dataset. The hyperparameter grids are described in table \ref{imphyps}. 

Method                  Varied hyperparameters          Number of combinations
--------------          -----------------------         ------------------------
MICE PMM                `donors`, `ridge`, `matchtype`  $180$
MICE regression         None                            $1$
MICE Bayes regression   None                            $1$
MICE Random forest      `ntree`                         $36$
k-NN                    `k`                             $5$ (effectively $2$)
BPCA                    `nPcs`, `maxSteps`              $112$
MissForest:             `mtry`, `ntree`                 $12$
Simple methods          None                            $1$ each

Table: Statistics on hyperparameter grids for imputation methods. For k-NN, only two values of `k` succeed, see subsection [Other imputation methods](#other).\label{imphyps}

After imputations of the training set and classifier training, the hyperparameter configuration with highest downstream classifier performance (or highest mean performance, in case of multiple imputation methods), and its associated classifiers, are selected and stored for each method, and that hyperparameter configuration is used when imputing the test set. Finally, performance is computed using the associated classifiers and associated test set (or test sets, for probabilistic and multiple imputation) of each imputation method.

The process is depicted visually in figure \ref{flowchart}.

[^hyper]: Are these usually called hyperparameters in imputation methods? Technically I think they are hyperparameters even if not called that, but it might be better to conform to common terminology.

![Framework flowchart. Features are filtered on the training data according to their correlations and variance, and the feature set of the test data is synchronized to match the filtered feature set. Afterwards, the training data is imputed using each imputation method, with each hyperparameter configuration. In the main experiment, multiple imputation methods are set to produce 10 imputed datasets per hyperparameter configuration. Only one dataset is set to be produced in the simulation and cross-validation experiments. Every dataset is then used to train a classifier, and for each imputation method the best-performing hyperparameter configuration is selected by the training-set performance of the corresponding classifier. For multiple imputation methods, the mean performance of the corresponding classifier set is used. The test set is then imputed with the selected hyperparameter configuration for each method, and corresponding classifier is used to predict on the imputed test set(s). Classifier performance is then evaluated on each set of predictions. \label{flowchart}](body.pdf){height=60%, width=60%}

### Classifiers

We restricted our analysis to two common classification methods. We chose to study both a simpler, less flexible baseline method, and a more complex and highly flexible machine learning method in order to see whether classifier flexibility affects the choice of best imputation method. In this study, we refer to classifiers that are trained and predict on datasets imputed by an imputation method as *downstream classifiers* of that imputation method. For the baseline downstream classifier, we chose logistic regression (LR), a standard statistical method for two-class problems; for the more complex method, we chose random forest (RF) [@randomforests], a highly flexible machine-learning method. Both methods have been used in existing variant pathogenicity predictors (e.g. KGGSeq [@kggseq] and later versions of CADD [@cadd2018] utilize logistic regression, while e.g. MutPred [@mutpred], PON-P [@pon-p], REVEL [@revel] and Meta-SNP [@metasnp] utilize random forest).

Logistic regression is used with the base R `glm`, and Random Forest is used via the package `randomForest` [@randomforestpkg]. Both are trained and applied to test data via functionality from the `caret` package [@caret].

The random forest was trained using the out-of-bag (OOB) performance for model selection. `glm` does not offer any tuning parameters. 


## Metrics

We use *Matthews' correlation coefficient* (MCC) as our main evaluation metric since it is less misleading than other common classification performance metrics in imbalanced datasets [@chicco2020]. 

MCC is defined as
$$MCC = \frac{TP \times TN - FP \times FN}{\sqrt{(TP+FN)(TP+FP)(TN+FP)(TN+FN)}}.$$

We also present results with the *area under ROC curve* (AUC-ROC, or just AUC) metric, defined as the area under the receiver operating characteristic curve.

Finally, we compute the root-mean-square error (RMSE) to compare imputed and original values when using simulated additional missing values. RMSE is defined as

$$\sqrt{\frac{1}{N} \sum_{i = 1}^{N}(\hat{y}_i - y_i)^2 }$$

where $y_i$ is the $i$th true value, $\hat{y}_i$ is the $i$th prediction (in this case, the imputed value), $N$ is the number of predictions (in this case, the number of imputed values).

## Simulation {#simulations}

We performed simulations with additional missing values in order to

1. compare the sensitivity of imputation methods to various levels of missingness, 
2. relate downstream classifier performance and an imputation method's predictive performance, i.e. the capability of the imputation method to predict the original value underlying a missing value.

A common strategy for studying imputation methods' performance is simulation of missing values either on fully simulated data, or on the complete subsets of real datasets. 
In our context of application, the number of complete cases in the dataset is very low and thus could not be used as the basis for simulating additional missing values.  
Instead, we chose to take the full, incomplete dataset as the basis of simulations, and used the following strategy:

1. Create many simulated datasets based on the full dataset with additional missing values using `ampute` [@ampute] on the dataset while varying missingness percentage
2. Impute each simulated dataset with each imputation method
3. Compute RMSE for each simulated dataset with respect to values that were observed in the original dataset but missing in the simulated dataset
4. Train a classifier on each imputed simulated training dataset, and evaluate performance on imputed original test set

We use nine percentage categories of additional missingness, from $10~\%$ to $90~\%$, each producing $100$ datasets with the respective amount of additional missingness. We therefore have $900$ simulated datasets on which the framework is executed. To reduce the computational requirements, we downsample the hyperparameter grids of each imputation method (in comparison to the main experiment). In addition, since MissForest is significantly more computationally expensive than other imputation methods, it is not included in the simulation experiment.

### Amputing additional missing values

The input matrix of `ampute` is required to be complete, so we partitioned the data according to missingness patterns. This forms a set of matrices for which every feature is either fully observed or fully missing. We then used each of the fully observed submatrices as inputs to `ampute`, and combined the resulting output back to form a matrix of the original size.

![Partitioning and simulating missingness. White blocks represent available values in a data matrix, while dark gray blocks represent original missing values and light gray blocks represent additional missing values. The rows of the original dataset are partitioned to matrices with equal missingness patterns. Each feature in such matrices is now either completely observed or completely missing, and the complete features can be treated together as a fully observed matrix. Additional missingness is simulated separately using `ampute` on the fully observed matrices, which are then combined.](simulations.pdf){height=100%, width=100%}

### Simulation evaluation

Many imputation studies are specifically built to assess an imputation method's capability to predict the original values from the observed values of a dataset with missing data.
Thus, they use the RMSE between the original dataset and an imputed dataset as a metric of performance of the imputation method.
However, a model with the lowest RMSE is not in general the best one in the statistical inference context [@fimd, chapter 2.6]. In short, the best model wrt. RMSE is linear regression estimated via least squares, and the deterministic nature of a regression prediction necessarily ignores uncertainty due to the missing data. 
The same argument cannot be applied to the predictive context, but the same end result may apply when missingness is informative. 

Consider a situation where missingness is highly informative. Then a perfect imputation method (with respect to RMSE; i.e. one where RMSE would equal $0$) would impute the dataset with the original values. However, the informativeness in the missing values would be lost. The loss of information could, in principle, be avoided by adding missingness indicators before imputing, but this comes with the increase in dimensionality (doubling the number of features in the worst case) and thus cannot be seen as a universal solution.

Since RMSE cannot be used as a universal metric of imputation method performance, we perform a lean version of the framework on each simulated dataset. That is, we impute each simulated dataset using a sparser hyperparameter grid (downsampled to eight hyperparameter configurations separately for each simulated dataset) and producing only one dataset each when using probabilistic methods. A classifier of each type is trained on the imputed simulated datasets, the best performing imputation hyperconfiguration is chosen by the highest performing classifier trained on a dataset imputed via that configuration. Performance is estimated on the test set imputed with the winning configuration. We compute, in addition, RMSE of each imputation method on each dataset, and can thus investigate whether low RMSE on the training set predicts high downstream classifier performance on the test set. [^necessary]

[^necessary]: Is this block necessary? Does it suffice to say (with a single sentence) that we run the framework on the datasets?

As mentioned in the [Preprocessing section](#preprocessing), due to the removal of features with fewer than $1~\%$ unique values and features that highly correlate with another feature, the addition of missing values may lead to differing feature sets between different simulated datasets. Especially large numbers of additional (MCAR) missing values may lead to fewer unique values, and both increase or decrease correlation between features by chance.

## Cross-validation experiment {#crossvalidation}

In the cross-validation experiment, the training data is used to produce $100$ training/test dataset pairs via repeated random sub-sampling with a $70~\%$ split.
The framework is run separately on each pair, after which the results can be used to estimate imputation methods' relative robustness to variation from sampling. 
For speed, hyperparameters for imputation methods are downsampled to eight hyperparameter configurations each, and multiple imputation methods are set to produce only a single dataset. However, due to the fewer required executions of the framework (when compared to the simulations), it was feasible to also include MissForest in the set of methods.

## Main experiment {#mainexp}

In the main experiment, the framework is run once with the full hyperparameter grid for each imputation method on the full training and test datasets, and MissForest is included in the set of methods.
We also utilize the multiple datasets generated by multiple imputation methods to estimate the performance variability that arises from randomness in the imputation of both training and test sets. 

## Challenges

### Dataset composition

Even after *a-priori* imputation, there is a very high proportion of missing values in the dataset (~ $31~\%$). The missingness exhibits a general pattern, and is not e.g. monotone (see [@little-rubin, pp. 6--7]). In addition, missingness appears in most features, and the number of complete cases ends up minuscule ($2$ rows in training data), and removal of any single feature would not significantly increase the number of available complete cases (see figure \ref{heatmap}). The missingness can also not in general be assumed to be MCAR, though due to the arguments by Sarle [@sarle1998] and Ding \& Simonoff [@ding-simonoff2010] mentioned earlier, we expect this has little effect on classification performance. 

### Studying imputation methods developed for statistical inference in a predictive context {#parameter-reuse}

As mentioned in subsection \ref{reuse-intro}, the differences in intended usage between statistical inference and prediction make use of existing implementations--many of which were designed for the former purpose--difficult. The first and main issue is that out-of-the-box implementations often do not provide an easy way to reuse learned parameters from an earlier run. This makes it difficult to use parameters from the training set on the test set.

Some ways to deal with this are

- *Reimplementation*. One can reimplement the method in a way that allows reuse of parameters, fully solving the problem. However, this is work-intensive, and requires deep understanding of each imputation method.
- *Ignore*. One can ignore the issue, and allow the imputation method to re-estimate its parameters when imputing the test set. However, this may lead to diminished classifier performance on the test set, as the distributions of imputed values may differ between training and test sets and thus may confound the classifier.
- *Concatenation of incomplete datasets*. When imputing the test set, concatenate the training set and test set, impute the combined dataset and then remove rows belonging to the training set. This reduces the difference between the distributions compared to *Ignore*, but may be computationally expensive.
- *Concatenation to imputed training data*. One can make a variation on *Concatenation of incomplete datasets* by imputing the training set before concatenation. This is faster than *Concatenation of incomplete datasets* if the imputation method does not run in linear time with respect to the size of the input dataset. However, it may introduce additional issues, since it will use the imputed values in the training set to estimate imputations for the test set. 
- *Concatenation of single observation*: A second variation of *Concatenation of incomplete datasets* is to concatenate only one observation from the test set at a time, and repeating this until the full test set is imputed. This variation makes it possible to predict on a single observation at a time. This is computationally expensive.

*Reimplementation* and *Concatenation of single observation* are the only options that allow imputation of test observations independently from each other. The others perform imputation with parameters estimated from other observations that are being predicted on at the same time.

Imputation method / family  Implementation  Out-of-the-box parameter reuse
--------------------------- --------------- -------------------------------
MICE                        `mice`          No
BPCA                        `pcaMethods`    No
k-NN                        `DMwR`          Yes
Simple methods              custom          Yes
MissForest                  `missForest`    No

The package `mlr`[@mlr2016] offers wrapper functionality that allows use of any prediction method offered by the package also for univariate imputation, along with functionality for correct reimputing data with previously learned parameters, but using this option is difficult in a dataset with very few complete cases. We did not explore this possibility in this work. Investigation of the imputation performance of methods originally intended for prediction might merit further study.

We choose to implement option *Ignore* due to its simplicity for methods where out-of-the-box parameter reuse is not available. There is a possibility that this will give an advantage to simple methods and k-NN even if MICE, BPCA and MissForest would otherwise outperform them. However, this comparison still represents the situation as it shows itself to the practitioner that may not have the time or expertise to make use of the other options.

## Implementation

The framework and experiments were implemented with R [@rproject] and organized into $11$ executable scripts:

-----------------------------------------------------------------------------
Number  Description                 File name
------  --------------------------- -----------------------------------------
1.      Parsing of VCF file         `01_parse_vcf.R`

2.      Preprocessing               `02_preprocess_data.R`

3.      Computation of              `03_descriptive_stats.R`
        descriptive statistics 

4.      Execution of training set   `04_run_impute_and_train.R`
        imputation and classifier 
        training for main experiment 

5.      Execution of test set       `05_run_prediction.R`
        imputation and prediction 
        for main experiment 

6.      Generation of datasets with `06_generate_simulated_data.R`
        additional missing values 

7.      Execution of imputation and `07_run_simulations.R`
        classifier training on 
        datasets with additional 
        missing values 

8.      Process and plot test       `08_analyze_results.R`
        set performance statistics 
        for main experiment        

9.      Process test set            `09_analyze_simulation_results.R`
        performance statistics for
        classifiers trained on
        datasets with 
        additional missing values 

10.     Plot test set performance   `10_simulations_plots.R`
        statistics for classifiers 
        trained on datasets with 
        additional missing values 

11.     Perform repeated random     `11_crossvalidation.R`
        sub-sampling 
        cross-validation and 
        process and plot its 
        results 
-------------------------------------------------------------

The scripts are intended to be executed in order, but users may choose only run a subset if they are only interested in a subset of the results. To run the main experiment, one must run 1., 2., 4., 5., and 8.; to run the simulations, one must run 1., 2., 6., 7., 9. and 10.; to run the crossvalidation experiment, one must run 1., 2. and 11.

## Availability of source code and requirements

Project name: AMISS

Project home page: [https://github.com/blueprint-genetics/amiss](https://github.com/blueprint-genetics/amiss)

Operating system(s): Linux

Programming language: R [@rproject]

Other requirements: The software was run with R 3.6.0 with packages vcfR [@vcfR], futile.logger [@futile.logger], tidyr [@tidyr], here [@here], magrittr [@magrittr], ggcorrplot [@ggcorrplot], mice [@mice], foreach [@foreach], doParallel [@doParallel], ggplot2 [@ggplot2], iterators [@iterators], missForest [@missforestpkg; @missforest], DMwR [@DMwR], doRNG [@doRNG], rngtools [@rngtools], lattice [@lattice], itertools [@itertools], randomForest [@randomforestpkg], ModelMetrics [@ModelMetrics], stringr [@stringr], gridExtra [@gridExtra], digest [@digest], purrr [@purrr], caret [@caret;@caretpkg], testthat [@testthat] and e1071 [@e1071], and pcaMethods [@pcaMethods] via BioConductor[@bioconductor] BiocManager [@BiocManager].

License: MIT

Any restrictions to use by non-academics: CADD annotations[^caddann] require commercial users to contact authors for licensing. dbNSFP [@dbnsfp3] annotations may require licenses for commercial use and must be reviewed individually.

[^caddann]: I am not sure whether this refers to all annotations or just the score, which I don't use.

## Availability of supporting data and materials

The data sets supporting the results of this article are available in the Zenodo repository, DOI [@amiss-zenodo-doi].

