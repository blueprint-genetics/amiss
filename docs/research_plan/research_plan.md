25th June 2019

# Introduction

A popular approach to predicting pathogenicity of single-nucleotide variants is using pre-existing tools to build so-called *metapredictors*. 
Metapredictors are models that use as their input the outputs of other, previously trained models (referred to in this text as *component predictors*).
Metapredictors hope to improve on their component predictors by combining them and providing additional labeled training samples. 
A common issue with this approach, however, is the often high proportion of *missing values*: many tools are unable to produce a value for every variant. 
The mechanisms causing the missingness are often not obvious and may depend on aspects of the tools that are difficult to inspect, such as the original training data of the component predictor. 
It is not in general useful to train a metapredictor to predict only when all inputs are available. 
First of all, removal of incomplete training samples is likely to bias the training data. Secondly, and more importantly, declining to predict on incomplete inputs would greatly diminish the metapredictor's applicability to practical settings.

As such, missing values must be handled in a way that enables both training and prediction on potentially incomplete feature vectors. 
The first option for achieving this is using models that have built-in capabilities for missing-value handling. 
The second option is to use *imputation*, which refers to filling in each missing value with some estimate of the corresponding unobserved value.

The traditional categorization of missingness [@little1987] is as follows:

- MCAR (missing completely at random), which denotes missingness that is independent of both the observed values and the unobserved values underlying the missing values;
- MAR (missing at random), which denotes missingness that is independent of the unobserved values, but may depend on observed values; and 
- MNAR (missing not at random), denotes missingness that is not independent of of either unobserved or observed values.

Most imputation methods have been developed assuming that the data to be imputed is either MCAR or MAR. However, it was observed in [@ding2010] that in a predictive setting (specifically classification with tree models), it is more useful to consider the relationship between missingness status and the response variable. The division is to informative missingness, where missingness status is dependent on the response variable, and not informative missingness, where missingness status is independent of the response variable.

To our knowledge, comparisons of missing value handling methods in the context of pathogenicity metapredictors has not been previously published.
Most studies in the field report using a single choice of an imputation method (e.g. KNN imputation in [@dong2014], [@Rychkova115956], BPCA in [@IOANNIDIS2016877]). 
In addition, few studies on imputation focus on a predictive rather than an explanatory setting (in the sense of [@shmueli2010]), and it would be interesting to obtain evidence to either confirm or contradict [@ding2010]'s observation on the high importance of informativeness of missingness compared to the traditional missingness categorization.

We propose to study the effects of different choices of missing value handling methods on a variant pathogenicity metapredictor. 
We will model the metapredictor as a random forest, and train it on ClinGen expert reviewed missense variants, thus using the highest-quality publicly available labeling. 
We will characterize the variants with existing scores from tools contained in the dbNSFP3.5 database in order to match feature sets commonly appearing in literature. 
The thus obtained feature matrix will be imputed using a set of imputation models (tbd), and then used to train a random forest classifier. 
The performance of this classifier will then be used to evaluate the suitability of the imputation method. 

To properly inform configuration of the imputation methods, we propose performing for each feature

1. an analysis on observed patterns of missingness,
2. an analysis of the likely mechanism of missingness,
3. an assessment whether imputation is the appropriate approach for that feature. 

In addition to imputation, we will use the reduced-feature paradigm for missing value handling as described in [@reduced_models] and compare its performance to those of the imputation methods.

# Results

We will use a process analogous to *multiple imputation* [@little1987] to gain an understanding of probabilistic imputation methods. 
Each probabilistic imputation method will be used $N$ times, the performance statistics averaged, and confidence intervals calculated for each statistic. See the subsection on [imputation method comparison][Imputation method comparison] for more information. 
We will also present classification performances obtained after applying deterministic single imputation methods such as mean, mode and constant imputation.

For each feature, we will compute correlation against the missingness vector of every other feature, providing understanding of the relation of observed values to missingness. We will present a hierarchically clustered heatmap constructed from this correlation matrix. 

We will compute another correlation matrix using solely the missingness indicators, thus describing links between features' missingness statuses.

We will also compute both above types of correlation matrices grouped by the pathogenicity label. This will allow us to assess the informativity of missingness by seeing if the distributions seem to differ markedly.

For each feature, we will attempt to discern from literature any likely computational or biological causes of missingness. 
We hope this will lead to a catalogue of missingness mechanisms of in-silico tools, informing future research and tool use.

The process of hyperparameter search for both the more complex imputation methods and for the random forest models will produce a set of configurations and associated performance metrics, allowing to assess e.g. whether the choice of an imputation method has a significant effect on the best choice of hyperparameters for a random forest.

# Impact

- The performance statistics allow us to select the missing data handling methods that produce best results in building variant pathogenicity metapredictors, informing future studies and possibly providing insight on the limitations and capabilities of the methods.
- The manual analysis of missingness is valuable to inform further studies and may provide biological insight.
- The correlations relate different in-silico tools to each other and to the missingness distributions of each other, increasing understanding of the in-silico tool scores.
- The performance statistics' confidence intervals produced by the multiple-imputation-style process may provide interesting information on the behavior of probabilistic imputation methods.

# Data 

## Training labels

For ground-truth labels, we obtain the set of ClinGen expert panel reviewed missense SNVs and their classifications.
We will use Ensembl Variant Effect Predictor version 96 to annotate the variants with all values from dbNSFP3.5c. 
Variants will be identified by their chromosome, position, and reference and alternative alleles. 

## Feature sets

The feature set will contain:

- Most tools in dbNSFP that are *not* themselves metapredictors: this rules out scores from tools such as MetaSVM or REVEL. 
- the non-population-specific gnomAD allele frequency from exomes

Three variations will be used:

1. The feature set, with missing values imputed with each imputation method
2. The feature set, with missing values imputed with each imputation method, with missingness indicator columns added
3. The feature set, with no imputation

# Methods

## Metrics

We will evaluate the performance of imputation methods by proxy, using a random forest classifier trained on imputed data.
We will evaluate the performance of each fitted random forest classifier by prediction performance on the test set.
As the main metric, we will use the Matthews correlation coefficient (MCC). For completeness, we will also compute and present most other common metrics used to evaluate classification performance. 

## Data split

After parsing the annotated VCF into an analyzable format, we will divide the dataset into a *training set* and a *test set*, containing 70 % and 30 % of the variants, respectively.

## Correlation matrices

We will compute the correlation matrices described in [Results][Results] on the training set using Pearson's correlation coefficient. 
In the case of categorical variables, only correlations of missingness indicators will be computed.
However, categorical variables' missingness can be treated by having an additional category representing 'missing', and thus it we expect relatively little gain from the use of imputation methods over these variables.

## Imputation method comparison

To obtain a baseline performance, we will use grid-search to configure and train a random forest model on the subset of data with no missing values. 
We will record the best-performing configuration's performance statistics on the training set and on the test set.

Then, for each imputation method, we perform grid-search over their parameters using $k$-fold cross-validation to choose the best-performing model.
That is, 

1. for each imputation method
    1. for each set of parameters
        1. perform imputation on every feature on the **training set**
        2. use grid-search to configure and train a random forest model on imputed data
        3. select best random forest configuration by its out-of-bag performance
    2. select best imputation parameter set by the performance of the best random forest configuration
    3. using the best imputation configuration:
        1. impute every feature on the **test set** $j$ times
        2. using the best random forest configuration, train and predict on each imputed copy of the test set
        3. form confidence intervals using multiple imputation methodology (see [@little1987])

We will perform the procedure for both [feature sets][Feature sets] 1 and 2.

## Parameter sets

For imputation methods, we will select the hyperparameter grid depending on the maximum of the times it takes to run each imputation method (the exact value will be determined after first experiments). The granularity should, however, be set to be the same also for the faster methods so that they are not unfairly advantaged. 

For random forest, we will select the hyperparameter grid so that the training can be completed in a reasonable amount of time (as above, exact value determined after first experiments).

## Assessing informativeness of missingness

To assess the informativeness of missingness, we will compare the performance of the methods obtained using feature set 1 to performances obtained using feature set 2 in [Feature sets][Feature sets]. If there are significant differences in at least one method, the missingness can be inferred to be informative. We can also assess the capabilities of different methods in making use of the informativeness.

# Challenges

Each in-silico tool is built on a complex computational model, making it likely that the missingness mechanism of some tools are MNAR. However, due to the predictive nature of the study, this is not necessarily an issue.

It is possible that the mechanism for obtaining the in-silico tool values causes additional unnecessary missingness, both in dbNSFP and in the VEP annotation process. 
To estimate whether this is the case, we will check for some small sample of tools whether we can observe some small number of values directly (either by local installation or via interfaces provided on the internet). 

This study will not be immune to the circularity issues described in [@Grimm2015]. 
Metapredictors will tend to have inflated performance estimates due to the difficulty of obtaining datasets that are completely disjoint from the training sets of every component score (termed type 1 circularity), due to how research practices cause heavy imbalances in the neutral versus pathogenic ratios within genes (termed type 2 circularity) and due to the fact that many tools' scores have been utilized in the selection of variants for further classification (see last paragraph in *Discussion* in [@Grimm2015]).  

Indeed, note that variants that have been reviewed by ClinGen's expert panels have necessarily been detected in genotypes sequenced in a clinical context, since the expert panels are interested only in reviewing variants that have been suspected of pathogenicity. Even benign variants are thus likely to be more "pathogenic-like" than an average variant, since they have been selected for a work-intensive review process. 

These issues must be taken into account when interpreting results. However, since we are comparing different methods' performances on the same dataset, the comparative performances should remain valid.
