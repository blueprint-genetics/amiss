# Materials and methods

## Data

### ClinGen dataset

The dataset consists of ClinGen[@clingen] expert-reviewed single-nucleotide variants from ClinVar[@clinvar2013; @clinvar2016; @clinvar2017].[^1]

[^1]: include date obtained?

Variants were annotated using the Ensembl Variant Effect Predictor (VEP)[@vep] version 96 and dbNSFP3.5[@dbnsfp1; @dbnsfp3]. In addition, we incorporated annotations used by CADD[@cadd2018], matching by VEP-annotated Ensembl transcripts. [^3]

[^3]: add variant number

### Features

The initial feature set was defined manually[^4].

[^4]: add selection rationale

- Observed missingness patterns

## Preprocessing 

For each variant, transcript specific values from dbNSFP were chosen to match the Ensembl canonical transcript annotated by VEP. 
Variants whose canonical VEP-annotated transcript ID did not match that from dbNSFP were discarded.

Missing values were replaced by default values for features where the missingness implied the default value *a priori* (e.g. a prediction of effect of amino acid substitution for a protein may be imputed with the neutral value (usually $0$) when a variant is intronic) .[^5]

[^5]: Add table here or in supp. materials

Categorical variables were processed to dummy variables. 

A binary outcome vector was formed by defining that variants classified as pathogenic or likely pathogenic as belonging to the positive class, and variants classified as benign or likely benign as belonging to the negative class. [^12]

[^12]: How many in each?

The final feature vectors of some sets of variants[^8] may be equal (i.e. duplicated). In such cases, only one variant [^9] was kept.

[^8]: How many?

[^9]: With the lowest chromosomal position; write this in supplementary notes

Use of categorical variables with high class imbalance within certain levels may obfuscate the performance of the imputation methods due to allowing the classifier to learn to classify all variants with that level into either the positive or the negative class, and therefore ignoring all other features upon which imputation may have been performed. VEP-predicted variant consequence is one such feature. Variants with consequences for which either class had less than $5 \%$ of overall variants of that consequence were removed[^10][^11].

[^10]: Removing how many?

[^11]: Explain that this would not be done in ordinary training practice.

To match an ordinary machine-learning process and to avoid issues with certain imputation methods[^sing], features with fewer than $1 \%$ unique values were removed.[^6]

[^6]: List these

[^sing]: e.g. in PMM one often got failures due to singular matrices before application of these steps

For feature pairs with high correlation, only one of the features was kept[^7].

[^7]: List removed features

The final processed dataset contains $n$ [^13] variants characterized by $m$[^14] features.

- Note that features removal leads to "largest feature set that works with all methods"; more elaborate analysis might find larger sets for some of them and thus give an advantage that is now lost

[^13]: How many?

[^14]: How many?

### Data split

The data was randomly split into training and test subsets, with $70 \%$ ($N=n$[^train]) of variants in the training set and $30 \%$[^perc] ($N=m$[^test]) of variants in the test set.

[^train]: How many?

[^test]: How many?

[^perc]: Check exact final percentages and add them


## Imputation methods

### Univariate imputation

The simplest imputation methods impute every missing value within a feature with the same value, which may either be a constant or a statistic estimated from the observed values of the feature.

 Simple imputation methods          Value
 --------------------------         ------
 Zero imputation                    $0$
 Maximum imputation                 Maximum observed value within feature
 Minimum imputation                 Minimum observed value within feature
 Median imputation                  Median observed value within feature
 Mean imputation                    Mean observed value within feature
 Unique-value[^unique] imputation   For observed values $F_{obs}$ of feature $F$, 
                                    $|\text{max}(F_{obs}) - \text{min}(F_{obs})| \cdot 10$ 
 Missingness indicator              For each feature, perform zero imputation 
                                    and create a binary feature[^mis_ind] 
                                    indicating original missing values

[^unique]: or "outlier"

[^mis_ind]: Make sure to note that duplicated indicator vectors added here are removed

### Multiple imputation by chained equations

Multiple imputation by chained equations (MICE)[@mice] 

- PMM
- RF
- norm
- norm.predict

### Other stuff, need name

- BPCA
- k-NN
- MissForest

## Classifiers

- Logistic regression
- Random Forest

## Simulation

- Addition of missingness to already incomplete dataset
- Added missingness patterns
- Note that different simulations may lead to different feature sets due to preprocessing

## Evaluation metrics

- MCC
- AUC-ROC
- RMSE for simulations
- Ranging over multiple imputation datasets
- (Non-)reuse of parameters from training in test process

