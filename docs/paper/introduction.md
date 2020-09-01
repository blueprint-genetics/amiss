# Introduction

## Variant pathogenicity prediction

Adoption of next-generation sequencing (NGS) technology has greatly increased the scalability of genetic sequencing in both research and clinical genetics.
Whole-exome and whole-genome sequencing (WES and WGS) are now becoming standard methodology, allowing detection of variants in a much broader set of loci than was previously feasible.
However, the numbers of detected variants from each sample present problems especially in clinical contexts, for example performing genetic testing to identify pathogenic variants for diagnostic purposes. Manual exploration of even stringently filtered variant lists is time-consuming, while timely diagnosis is often needed.

*In-silico* variant effect and pathogenicity prediction tools such as SIFT [@sift2001], PROVEAN [@provean2012], MutationTaster2 [@mutationtaster; @mutationtaster2], LRT [@lrt] and FATHMM [@fathmm2013] have been developed to inform the selection of variants for further testing in research contexts (though also often used in clinical variant interpretation, see ACMG/AMP guidelines [@acmg-amp]). Gene and variant prioritization tools such as VAAST [@vaast], PHEVOR [@phevor], FunSeq [@funseq], PHIVE [@phive] and Phen-Gen [@phengen] have been developed to rank variants. See reviews by Peterson \& al. [@peterson2013], Niroula \& Vihinen [@niroula-vihinen2016], and Eilbeck \& al. [@eilbeck2017].

Many traditional *in-silico* prediction tools are in general based on evolutionary conservation (e.g. SIFT, FATHMM, PROVEAN[^recite]), with some adding predicted change in protein function (e.g. PolyPhen2 [@polyphen2], MutPred [@mutpred], MutPred2 [@mutpred2]). 

[^recite]: do I need to cite these again, or is the one-paragraph distance sufficiently short?

Another class of predictors, called *metapredictors* or *ensemble predictors*, such as REVEL [@revel], CADD [@cadd2014;@cadd2018], DANN [@dann], Eigen [@eigen], PON-P [@pon-p] and MetaSVM and MetaLR [@dong2014], are developed by using machine learning to combine outputs from several previously developed tools. Metapredictors often report high performance [@li2018; @ghosh2017]. Since they are able to jointly utilize tools that are aimed at variants with different predicted consequences by, for example, combining missense variant effect prediction tools and splicing variant effect tools, they are often also able to predict for wide ranges of variants. 

Since traditional tools have been developed using different data, different methods and at different times, they may differ in the set of variants for which prediction is possible, even if their intended use cases match. That is, SIFT may be able to make a prediction on a missense variant for which MutationTaster is not. In practice, this means that the input features for metapredictors have *missing values*. A similar situation occurs when, for example, incorporating allele frequency information from gnomAD [@gnomad]: variants that were not observed in the study will have a missing value as their allele frequency. 


## Aim

The high prevalence of missing values in annotated variant data implies that the chosen method for handling missingness will have a major impact on the performance of variant pathogenicity metapredictors.
Our aim is to identify missingness handling methods most likely to produce good results in this context. This includes comparing achieved classification performance, difficulty of implementation, and computation time. For this purpose, we present a framework implemented in the `R` language [@rproject] that treats variant data with a variety of methods and then trains and evaluates a classifier on each treated dataset. 

We focus on the comparison of imputation methods, as they are the most generally applicable and allow use of any machine learning method on the imputed dataset. These properties are shared by the missingness indicator augmentation method, which we therefore also include. We limit our evaluation to imputation of numerical features, as categorical features have a relatively natural treatment through an additional category denoting missingness.

We perform three experiments based on executing the above-mentioned framework on the ClinGen [@clingen] subset of variants from ClinVar [@clinvar2013;@clinvar2016;@clinvar2017]. 
In the first experiment, we repeatedly simulate additional missing values in the training dataset. We then use thus generated datasets and the framework to evaluate, for each imputation method,

1. the effect of increasing missingness on classification performance,
2. the relationship between imputation error, which is the difference between original known values and imputed values, and classification performance.

In the second experiment, we perform repeated random sub-sampling cross-validation on the training set to assess whether there are differences in the methods' susceptibility to changes in dataset composition.

In the third, main experiment, we use the full data and wider parameter grids to execute the framework to obtain a realistic estimate of the effect of imputation method choice in variant pathogenicity metapredictor construction.

# Background

## Missing data

Data with and without missing values are often called *incomplete* and *complete* data, respectively.
Consider a matrix of $A$ that represents the unobserved, underlying values that would be obtained by data collection in the absence of any missing data generation mechanisms.
The subset of values of $A$ that are observed in data collection is denoted $A_{obs}$, and the subset of missing values of $A$ is denoted $A_{mis}$. The values of $A_{mis}$ are not known when analysing any real dataset. $M$ is the missingness indicator matrix whose values are $0$ when the corresponding value of $A$ is observed, and $1$ when the corresponding value of $A$ is missing.

Traditionally, missing data mechanisms are classified into *missing completely at random* (MCAR), *missing at random* (MAR) and *missing not at random* (MNAR) [@little-rubin].
In a missing data process with an MCAR mechanism, the probability of a value being missing does not depend on any observed or unobserved values. 
With an MAR mechanism, the probability of a value being missing may depend on observed values, and with an MNAR mechanism, the probability of a value being missing may depend on both observed and unobserved values.
Van Buuren notes data from a truly random sample of a population as an example of MCAR [@fimd, subchapter 1.2]; measurements for individuals that were not selected in the sample do not depend on the underlying values of the individual or the observed values of other individuals. Note however, that missing values for measurements on variants obtained by a random sample of human subjects is not MCAR. The sampling is not over the set of possible variants, but over individuals harboring those variants, and the likelihood of a variant's value being missing thus depends on the variant's allele frequency. [^discuss] 
Further, for non-MCAR missingness a good example could be gnomAD [@gnomad] allele frequency. A missing value may indicate that the variant was not present in the study population, and thus imply that it might also be extremely rare in healthy individuals. Very low allele frequencies thus end up with missing values, and thus exhibit MNAR behavior. Alternatively, it might indicate the variant is present, but was not observed due to residing in a difficult-to-sequence locus, thus exhibiting MAR behavior dependent on position. 

The above classification is essential in statistical inference, and the validity of different methods depends on which mechanisms appear in the data. However, Sarle [@sarle1998] notes that "The usual characterizations of missing values as *missing at random* or *missing completely at random* are important for estimation but not prediction", and Ding \& Simonoff [@ding-simonoff2010] provide evidence in support of this statement in the use of classification trees.
In an interesting reversal, the presence of *informative missingness* [@sarle1998; @ding-simonoff2010] in the data, i.e. missingness being dependent on the response variable conditional on $A_{obs}$, may actually lead to improved predictive accuracy compared to complete data [@ding-simonoff2010].

[^discuss]: Are there actually study designs that sequence a set of people and then just grab a set of variants (removing duplicates) and then annotate them? IIRC the case is actually that we would count how many times each variant occurs in the sample. Here missingness implies that it was not observed; this does depend on the allele frequency though...

### Missingness handling in prediction

Rather than inferring properties of the statistical distribution of variant annotations, our aim is production of predictions of pathogenicity, be it a classification into pathogenic and non-pathogenic, a class probability or some other metric. Our aim is thus not statistical inference, for which most missingness handling methods have been developed. 
It is not clear how the performance of a missingness handling method in one context relates to its performance in another.
Overall, there is a significant distinction between these aims, which leads to a variety of differences in the concerns that one must account for [@shmueli2010; @breiman2001].  

Most studies introducing imputation methods focus on problems where missingness may occur in the model estimation phase, but where data is assumed to be complete at prediction time. Similarly, comparisons of missingness handling methods in machine learning tend to consider missing values only in the context of the training set.
In this context, it suffices to design methods which facilitate model training in a way that maximizes generalization performance. In our use case, missing values proliferate also in the test set, and as such any methods must also allow treatment of the data on which we predict.

\label{reuse-intro}This raises additional challenges. Most imputation methods are not implemented in a way that easily allows reuse of learned parameters. That is, it is difficult to first estimate imputation method parameters on the training set, and then impute the test set using the same parameters. This leads to diminished prediction accuracy, as the distributions of imputed data differ in the training and test sets. Even worse, since the parameters for test set imputation are estimated from the test set, the content and size of the test set itself may affect the predictions for individual observations, and make it impossible to predict one observation at time. 


### Methods for missingness handling

Most of both machine learning models and statistical models are unable to directly use data with missing values. 
To use such data, we can adopt one of the following strategies. 

**Removal of incomplete observations**. Simple removal of incomplete observations is a valid approach when all data we wish to predict on can be assumed to be complete. This is not a viable alternative for variant pathogenicity prediction, since the predictor would be unable to predict on any variant whose feature vector contains any missing values. The number of such variants grows as more features with missing values are added, and as such would greatly restrict the available set of features. 

**Model-based approach**.
There exist classification methods that can train and predict directly on feature vectors with missing values. Such models may require explicit specification of the mechanism behind missing values. E.g. Marlin [@marlin] presented a variation on linear discriminant analysis (LDA) that can both be trained and predict on feature vectors with missing values. Random forest [@randomforests] offers a method for handling missing data by iteratively making use of the proximities of observations [@randomforestlink], but is only available for the training phase.
CART offers a missingness handling method [@cart] via so-called surrogate splits, but this method is not suitable for an ensemble of trees like random forest (see [@ishwaran2008]). 
The `randomForestSRC` R package [@rfsrc] implements a missingness handling strategy that is usable in both training and prediction phases [@ishwaran2008]. [^notused]

[^notused]: Should I drop this sentence? We don't use this method due to time constraints and the very large number of moving parts

**Imputation**. Imputation is the process of replacing the missing values in data by explicit values. When done exactly once, after which the imputed (or *completed*) dataset is analyzed in the ordinary fashion, the process is termed *single imputation*. Common single imputation strategies are [@little-rubin]:

- constant imputation, for example imputation with zeroes, 
- unconditional mean imputation, where missing values within the same feature are replaced by the mean of the observed values of that feature,
- conditional mean imputation, where replacing values can be means dependent on the observed values of other features, for example by modeling with regression, and 
- drawing from a predictive distribution, where values can be estimated by, for example, addition of a random deviate to a conditional mean estimate, or by drawing the value randomly from the set of observed values.

If aiming to perform statistical inference on the imputed data, one has to carefully ensure that, in addition to unbiasedness of any parameter estimates, uncertainty introduced by missingness is correctly reflected in the estimated standard errors. This is in contrast to designing methods simply for accurate prediction of the underlying value (which is yet again distinct from only facilitating the accurate prediction of the response variable).

Indeed, naïvely imputing data with a single imputation method is misleading as use of even highly accurate single imputation methods will cause underestimation of the standard errors in inference [@fimd, subchapter 2.6].
The uncertainty can be properly incorporated via two main avenues: *likelihood-based approaches* and *multiple imputation* (see [@little-rubin]). 
Likelihood-based approaches account for missing values by integrating out the parameter representing the missingness generating process, and do not require explicit imputation of missing values [@little-rubin].

As opposed to single imputation, the main idea of multiple imputation for statistical inference is to impute the incomplete data multiple times with draws from predictive distributions, fit separate models on each imputed dataset, and *pool* the parameter estimates [@little-rubin].  
In classification, there are several ways to utilize the set of imputed datasets. The first is to obtain estimates of the variability of the classification performance due to the randomness from draws from a predictive distribution.
The second would be that one can in principle train a classifier on each, averaging results in the prediction phase [@marlin; @williams].
The benefits of this latter approach are not explored in this paper.

**Reduced models**.
A somewhat brute-force approach to missingness handling in classification is to train a separate predictor for each combination of missing features using only the available features for each subset. This approach is called the reduced models [@marlin] or reduced-feature models [@saar-tsechansky2007] approach, and is observed by Saar-Tsechansky \& Provost to consistently perform well [@saar-tsechansky2007]. However, a naïve implementation of reduced models easily leads to very high computational time and storage requirements, and the hybrid and on-demand approaches described by Saar-Tsechansky and Provost [@saar-tsechansky2007] are not trivial to implement. 

**Missingness indicator augmentation**. A conceptually simple method is to add, for each original feature, an extra indicator feature that takes the value $1$ in observations with the original feature missing, and $0$ when the original feature is observed [@missingnessind]. The missing values of the original feature are then filled with zeroes, and identical indicator features may be removed. The downside with this method is that it may lead to a significant increase in dimensionality of the data (doubling it in the worst case).

### Missingness handling in existing tools

**Missingness causes and handling in traditional tools.** Most missingness in variant annotation data can be attributed to the following causes:

1. Insufficient information related to a sequence
2. Inapplicability to the variant

The first cause describes situations where, for example, protein function change prediction tools based on metrics computed from multiple aligment are unable to find sufficiently many matches to the input sequence. In these cases, the tool has no information on which to base its estimates. Another example of this cause is values estimated from scientific studies, e.g. allele frequencies, functional properties of proteins or expression levels of transcripts. Variants that are unobserved in the cohorts, excluded from genotyping microarrays or inside hard-to-sequence loci will have no observed allele frequency.

The second cause refers to attempts to annotate a variant with a tool that is inapplicable to its annotated molecular consequence. For example, a tool predicting change in protein function cannot produce an estimate for an intergenic variant.

**Missingness handling in existing metapredictors**. Strategies for missingness handling vary significantly between different existing metapredictors. REVEL [@revel] uses k-NN imputation when a variant's missingness is $\leq 50 \%$, and mean imputation when missingness is $> 50 \%$. CADD [@cadd2014; @cadd2018] and DANN [@dann] use a mix of manually defined default values to replace missing values, with added missingness indicators for certain features, and mean imputation for genome-wide measures (see Supplementary information for [@cadd2014]). 
M-CAP replaces missing values with constants representing maximally pathogenic prediction for each component tool [@mcap]. Eigen [@eigen] utilizes separate strategies for training and test phases. Eigen builds several weighted linear combinations of its features depending on variant type, only requiring annotations applicable to a specific variant to be available. Learning the weights is based on pairwise correlation, which can be estimated in the presence of some missing values. In the test phase, Eigen performs mean imputation for features that are applicable to the specific variant. KGGSeq ignores any variants that has missing values in its features [@kggseq]. PRVCS [@prvcs] removes variants with missing values in the training phase, and replaces missing values of a feature by its population mean in the test phase.

### Multiconsequence predictors

Missingness due to inapplicability could be avoided by training separate models on each molecular consequence, and using only features fully applicable to each consequence. This would still leave the first cause, and thus still require missingness handling, even if to a lesser degree. The dimensionalities of the feature spaces of each consequence-specific classifier would be lower without a loss of information, possibly increasing performance (see also section above on reduced models). However, this requires each consequence category to have sufficiently many observations to train a classifier. In contrast, a single classifier for predicting regardless of consequence class can be trained on the whole data set, possibly allowing the predictor to generalize information between variants with different consequences.

We choose to focus on building a single classifier for single-nucleotide variants and small indels.

