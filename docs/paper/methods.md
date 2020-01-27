# Materials and methods

## Data

### ClinGen dataset

The dataset consists of ClinGen[@clingen] expert-reviewed single-nucleotide variants from ClinVar[@clinvar2013; @clinvar2016; @clinvar2017].[^1]

[^1]: include date obtained?

We annotated the variants using the Ensembl Variant Effect Predictor (VEP)[@vep] version 96 and dbNSFP3.5[@dbnsfp1; @dbnsfp3]. In addition, we incorporated annotations used by CADD[@cadd2018], matching by VEP-annotated Ensembl transcripts. [^3]

[^3]: add variant number

### Features

The initial feature set was defined manually[^4]. We excluded any metapredictors from the feature set.

- Caveat: lack of explicit variable selection may disadvantage logistic regression
    - It is not clear whether this affects the relative performances of imputation methods

[^4]: add inclusion rationale

- Described observed missingness patterns

- Caveat: missing values in covariance matrices
    - This can be understood as a consequence of monotonous missingness (though the missingness is not fully monotonous)
    - Monotonous missingness should not be a problem in itself, even though we don't specifically use the information of its presence.

## Preprocessing 

- Caveat: centering and scaling not done, except for BPCA, which requires it.
    - Neither scaling nor centering should affect random forest, logistic regression (intercept takes care of centering) and at least Van Buuren does not mention either centering or scaling to matter in MICE (which makes sense, since one wouldn't want to lose the original scaling when doing inference).

For each variant, we chose transcript specific values from dbNSFP to match the Ensembl canonical transcript annotated by VEP. 
Variants whose canonical VEP-annotated transcript ID did not match that from dbNSFP were discarded.

Missing values were replaced by default values for features where the missingness implied the default value *a priori* (e.g. a prediction of effect of amino acid substitution for a protein may be imputed with the neutral value (usually $0$) when a variant is intronic) .[^5]

Feature name        Feature interpretation              Default value
-------------       -----------------------             --------------
`motifECount`                                           $0$
`motifEScoreChng`                                       $0$
`motifEHIPos`                                           `FALSE`
`tOverlapMotifs`                                        $0$
`motifDist`                                             $0$
`gnomAD_exomes_AF`  gnomAD allele frequency from exomes $0$

[^5]: Add table here or in supp. materials

Categorical variables were processed to dummy variables. 

- Caveat: No imputation methods designed specifically for categorical variables used

- Dummy variable coding is not full rank, but this is fixed by the removal of highly correlated variables.

- Caveat: Setting all missing categorical values to zeros in dummies; no imputation method does anything for categorical variables
    - Though very few categorical variables (3)

We formed a binary outcome vector by defining that variants classified as pathogenic or likely pathogenic as belonging to the positive class, and variants classified as benign or likely benign as belonging to the negative class. [^12]

[^12]: How many in each?

The final feature vectors of some sets of variants[^8] may be equal (i.e. duplicated). In such cases, we kept only one variant [^9].

[^8]: How many?

[^9]: With the lowest chromosomal position; write this in supplementary notes

Use of categorical variables with high class imbalance within certain levels may obfuscate the performance of the imputation methods due to allowing the classifier to learn to classify all variants with that level into either the positive or the negative class, and therefore ignoring all other features upon which imputation may have been performed. VEP-predicted variant consequence is one such feature. For this reason, we removed variants with consequences for which either class had less than $5 \%$ of overall variants of that consequence[^10][^11].

[^10]: Removing how many?

[^11]: Explain that this would not be done in ordinary training practice.

To match an ordinary machine-learning process and to avoid issues with certain imputation methods[^sing], we removed features with fewer than $1 \%$ unique values.[^6]

[^6]: List these

[^sing]: e.g. in PMM one often got failures due to singular matrices before application of these steps

For feature pairs with high correlation, we kept only one of the features[^7].

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

## Training

### Classifiers

Both logistic regression and random forest classifiers are trained using the `caret` package[@caret].
Logistic regression is trained with the base R `glm`, and random forest as implemented in the package `randomForest`.

#### Classifier hyperparameter search{#class-hyper-search}

The random forest was trained using the out-of-bag (OOB) performance for model selection. 

- Caveat: we use OOB for model selection of RF, but MCC for selection of imputation hyperparameters.

`glm` does not offer any tuning parameters. 

### Imputation hyperparameter search

In order to maximize the performance of each imputation method for fair comparison, hyperparameter grids were defined for each method for which different hyperparameters[^hyper] could be passed. 

### Multiple datasets from probabilistic[^probstoc] methods

Multiple imputation methods are naturally designed to draw multiple datasets from the estimated imputation model[^micedraw]. 
We utilize the multiple datasets to estimate the performance variability that arises from randomness in the imputation of both training and test sets. 

For each completed dataset from a probabilistic imputation method, we train a separate classifier (performing its usual hyperparameter search and model selection procedure separately on each dataset, see section [Classifier hyperparameter search](#class-hyper-search)). 
The training set performances of each classifier trained on a completed dataset produced by the same imputation model and the same imputation hyperparameters are averaged. The hyperparameter configuration with the highest mean classifier performance is selected for each imputation model[^model]. Each classifier trained on datasets produced by the winning hyperparameter configuration is retained for test set performance evaluation.

[^model]: Model, or model type?

Another use of multiply imputed datasets would be that one can in principle train a classifier on each. 
In prediction, one would average results, hypothetically leading to better overall performance[^sarleform]. 
However, this is even more resource intensive than the usual setting.  
The benefits of this approach are not explored in this paper. 

[^sarleform]: IMO this fits one of the formulae in Sarle. Check

One can also simply run any probabilistic single imputation methods multiple times with different seeds.

[^micedraw]: Does MICE actually estimate a model and draw from it $m$ times, or just estimate a model $m$ times? IIRC, the model is represented across iterations as simply the previous completed datasets. This makes it sound like it would be the latter.

[^probstoc]: Are probabilistic and stochastic equivalent in this context?

[^hyper]: Are these usually called hyperparameters in imputation methods? Technically I think they are hyperparameters even if not called that, but it might be better to conform to common terminology.

- Caveat: I do not monitor RF convergence.
- Caveat: I optimize (out-of-bag) accuracy, not MCC in RF training and HP selection
- Caveat: RF hp grid is not big. However IMO it doesn't need to be.

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

- Caveat: should missingness indicators + logistic regression have interactions between variable + its indicator?

[^unique]: or "outlier"

[^mis_ind]: Make sure to note that duplicated indicator vectors added here are removed. 

### Multiple imputation by chained equations

- Caveat: I do not explore *joint modeling* (JM) models, presented as the canonical alternative to MICE for performing multivariate multiple imputation.

Multiple imputation by chained equations (MICE), or *fully conditional specification* (FCS)[@vanbuuren2007][^micefcs], refers to iteratively imputing single variables conditional on other variables. In short, a fully conditional specification algorithm 

1. uses univariate imputation to sequentially impute each variable[^varvsfeat] conditional on the observed values of other variables
2. reimputes each variable conditional on the imputed data from the previous iteration

Step 2 is repeated until some maximum number of iterations or some measure of convergence is reached.

We used the R `mice` package [@mice] to perform the imputation. The following univariate imputation methods provided by `mice` were used[^imphypmention]:

[^imphypmention]: Should I also present the hyperparameters that were searched over?

[^varvsfeat]: Try to use only feature or variable? It seems more natural to use variable here, due to the more statistical context.

[^micefcs]: Does it suffice to cite only MICE here? The paper makes this statement.

- Caveat: I use a "low" number of iterations and completions ($10$ for both). They are considered good numbers in a statistical inference context, but it is not fully clear whether the same applies in predictive contexts.

Method                      `mice` model
-------------------------   ------------- 
Predictive mean matching    `pmm`
Random forest               `rf`
Linear regression           `norm.predict`
Bayesian linear regression  `norm`

- Caveat: convergence issues with mice::RF
- Caveat: norm.predict particularly vulnerable to re-estimation parameters from test set

### Non-MICE imputation

We used several popular models falling outside the multiple imputation framework.

Method          R package
--------------  ---------------
BPCA            `pcaMethods`
k-NN            `DMwR`
MissForest      `missForest`

- Caveat: in k-NN, you must have enough complete cases to even start imputation. The number depends on $k$. In our case, the largest $k$ that could be used was $3$[^checkk].
- Caveat: BPCA required scaling of the dataset, unlike all other methods. Somehow reuse of scaling parameters from training set made the results still worse, so there is likely something wrong with how I am using the model.

[^checkk]: Check max $k$

### Built-in method in random forest

Random forest offers a method for handling missing data by iteratively making use of the proximities of observations [@randomforestlink]. This is only available for the training phase. [^randomforestmiss]

[^randomforestmiss]: The other way described on the page amounts to mean and mode imputation.

## Simulation

### Main idea

In addition to the main experiment, we wished to gain some understanding into whether the rankings of the imputation methods would be affected by different percentages of missingness. 
A common strategy for studying this is simulation of missing values either on fully simulated data, or on the complete subsets of real datasets. 
In our context of application, the number of complete cases in the dataset is very low and thus cannot be used as the basis for simulation.

Instead, we chose to take the full, incomplete dataset as the basis of simulations. 

1. Create many simulated datasets based on the full dataset with additional missing values using `ampute`[@ampute] on the dataset while varying mechanism and percentage
2. Impute each simulated dataset with each imputation method (using a downsampled hyperparameter grid compared to main experiment)
3. Compute RMSE for each simulated dataset with respect to values that were observed in the full dataset but missing in the simulated dataset
4. Train a classifier on each completed dataset, and evaluate performance on completed test set

### Amputing additional missing values

The simulation of additional missing values was performed using `ampute` implemented in `mice`, which allows simulation of each of MCAR, MAR and MNAR missingness mechanisms. This allows us to assess the change in performance with increasing levels of MAR and MNAR, common in real data, instead of just MCAR, which is usually the only mechanism generated in articles presenting new imputation methods. The input matrix of `ampute` is required to be complete, so we partitioned the data according to missingness patterns. This forms a set of matrices for which every feature is either fully observed or fully missing. We then used each of the fully observed submatrices as inputs to `ampute`, and combined the resulting output back to form a matrix of the original size.

- Caveat: MAR and MNAR mechanisms may depend on different variables in different original missingness patterns
- Might want a diagram here

### "Lean" experiment

Many imputation studies are specifically built to assess an imputation methods capability to predict the original values from the observed values of a dataset with missing data.
Thus, they use the RMSE between the original dataset and an imputed dataset as a metric of performance of the imputation method.
However, a model with the lowest RMSE is not in general the best one in the statistical inference context [@fimd, chapter 2.6]. In short, the best model wrt. RMSE is linear regression estimated via least squares, and the deterministic nature of a regression prediction necessarily ignores uncertainty due to the missing data. 
The same argument cannot be applied to the predictive context, but the same end result may apply when missingness is informative. 

Consider a situation where missingness is highly informative. Then a perfect imputation method (with respect to RMSE; i.e. one where RMSE would equal $0$) would impute the dataset with the original values. However, the informativeness in the missing values would be lost. The loss of information could, in principle, be avoided by adding missingness indicators before imputing, but this comes with the increase in dimensionality (doubling the number of features in the worst case) and thus cannot be seen as a universal solution.

Since RMSE cannot be used as a universal metric of imputation method performance, we perform a leaner version of the main experiment on each simulated dataset. That is, we impute each simulated dataset using a smaller hyperparameter grid (downsampled separately for each simulated dataset) and producing only one dataset each when using probabilistic methods. As in the main experiment, a classifier of each type is trained on the imputed simulated datasets, the best performing imputation hyperconfiguration is chosen by the highest performing classifier trained on a dataset imputed via that configuration, and performance is estimated on the test set imputed with the winning configuration. We can thus investigate whether low RMSE on the training set implies high downstream classifier performance on the test set.

- Note that different simulations may lead to different feature sets due to preprocessing

## Performance evaluation

We use two main metrics to evaluate the performance of resulting classifiers.

The *Matthews' correlation coefficient* (MCC) is defined as
$$MCC = \frac{TP \times TN - FP \times FN}{\sqrt{(TP+FN)(TP+FP)(TN+FP)(TN+FN)}},$$

- Why MCC? Because has less issues e.g. w/ overoptimistic estimates in problems with high class imbalance, unlike accuracy or $F_1$

The *area under ROC curve* (AUC-ROC, or just AUC) is defined as the area under the receiver operating characteristic curve.

The root-mean-square error (RMSE) is defined as

$$\sqrt{\frac{1}{N} \sum_{i = 1}^{N}(\hat{y}_i - y_i)^2 }$$

where $y_i$ is the $i$th true value, $\hat{y}_i$ is the $i$th prediction (in this case, the imputed value), $N$ is the number of predictions (in this case, the number of imputed values).

### Ranging over multiple imputation datasets

Due to the production of multiple completed versions of both training and test datasets, we can observe the variability in performance of classifiers downstream of probabilistic imputation methods due to both the variation in the imputations of the training set and the variation in the imputations of the test set.

### Issues with studying imputation methods developed for statistical inference in a predictive context

Due to the differences in intended usage between statistical inference and prediction, imputation methods developed for the former have some additional difficulties in their use for the latter. The first and main issue is that out-of-the-box implementations often do not provide an easy way to reuse parameters from an earlier run. This makes it difficult to use parameters from the training set on the test set.

Some ways to deal with this are

1. Reimplementing the method in a way that allows reuse of parameters
2. Ignoring the issue, and imputing the test set allowing the imputation method to re-estimate its parameters
3. For imputing the test set, concatenate the training set and test set, impute the combined dataset and then remove rows belonging to the training set
4. 3., but concatenating the test set with an imputed training set
5. 3., but concatenating only one observation from the test set to the training set, and repeating this until the full test set is imputed

Each of these options lead to different advantages and disadvantages.

1. is work-intensive, and requires deep understanding of each imputation method. Fully solves the problem.
2. leads to diminished classifier performance on the test set, as the distributions of imputed values may differ between training and test sets[^dimperf]
3. This reduces the difference between the distributions compared to 2. 
4. Faster than 3., but probably introduces biases
5. Avoids the issue where it the size of the test set affects the performance on the test set, and makes it impossible to predict on a single observation at a time. Really slow.

Options 1. and 5. are the only ones that allow imputation of test observations independently from each other; options based on 2. necessarily perform imputation with parameters estimated from the other observations that are being predicted on at the same time. This is in general undesirable.

Imputation method / family  Implementation  Out-of-the-box parameter reuse
--------------------------- --------------- -------------------------------
MICE                        `mice`          No
BPCA                        `pcaMethods`    No
k-NN                        `DMwR`          Yes
Simple methods              custom          Yes
MissForest                  `missForest`    No

The package `mlr`[@mlr2016] offers wrapper functionality that allows use of any prediction method offered by the package also for univariate imputation, along with functionality for correct reimputing data with previously learned parameters, but we did not explore this possibility in this work. Investigation of the imputation performance of methods originally intended for prediction could be an opportunity for future work.

We choose to implement option 2. due to its simplicity when out-of-the-box parameter reuse is not available. 

- Caveat: this may bias the comparison in favor of k-NN \& the simple methods.

[^dimperf]: I am not fully convinced of this yet.

### Circularity in variant databases

Grimm et al.[@grimm2015] described several biasing factors in variant effect predictor training and performance evaluation using data from commonly used variant databases, e.g. the tendency for variants within the same gene being classified as all pathogenic or all neutral, or simply due to difficulty of finding datasets completely disjoint with the training set. Mahmood et al. [@mahmood2017] further analysed existing variant effect predictors[^effectvspath] using datasets generated from functional assays, and found drastically lower performance compared to earlier reported estimates.

Our approach is not immune to these biases, and we expect that any reported performance metrics will be overoptimistic. However, we expect that the main result of the study, the relative performance rankings of imputation methods, will not be affected by the biases. The classifiers built described in this work are not intended to outperform earlier approaches or be used for variant effect prediction.

[^effectvspath]: Make sure to distinguish between variant effect prediction and variant pathogenicity prediction

