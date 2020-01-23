# Introduction

## Variant pathogenicity prediction

- existing methods, how they deal with missingness

## Missingness handling

*Missing data* is common in real datasets. 
Consider a matrix of $A$ that represents the unobserved, underlying values that would be obtained by data collection in the absence of any missing data generation mechanisms.
The subset of values of $A$ that are observed in data collection is denoted $A_{obs}$, and the subset of missing values of $A$ is denoted $A_{mis}$. Of course, the values of $A_{mis}$ will not be known when analysing any real dataset. $M$ is the missingness indicator matrix whose values are $0$ when the corresponding value of $A$ is observed, and $1$ when the corresponding value of $A$ is missing.

- Define prediction accuracy

Missing data processes may be classified into *missing completely at random* (MCAR), *missing at random* (MAR) and *missing not at random* (MNAR).[@little-rubin] 
In a missing data process with a MCAR mechanism[^mechanism], the probability of a value being missing does not depend on any observed or unobserved values. 
In a missing data process with a MAR mechanism, the probability of a value being missing may depend on observed values. 
In a missing data process with a MNAR mechanism, the probability of a value being missing may depend on both observed and unobserved values.

[^mechanism]: Check whether one should only use mechanism, process, or both

### Statistical inference[^statinfovsexp] vs. prediction

[^statinfovsexp]: Use word "explanation" like Shmueli, or "statistical inference"; or "estimation", like Sarle?

#### Statistical inference

Much of missing data literature is focused on treatment of missing data in the context of statistical inference[^infcontext]. 

- Validity of complete cases

In such cases, the methods are designed to ensure, in addition to unbiasedness, that the uncertainty introduced by missingness is correctly reflected in the standard errors. This is in contrast to designing methods simply for accurate prediction of the underlying value[^bs]. 
Indeed, naïvely imputing data with a single imputation method[^singleimp] is misleading as use of even highly accurate single imputation methods will cause underestimation of the standard errors[^naive].

[^singleimp]: Define single imputation

The uncertainty can be properly incorporated via two main avenues: *multiple imputation* (MI) and *maximum likelihood*[^mimlorder] (ML) based approaches (see [@little-rubin]). 
The latter naturally accounts for missing values by modeing both the data generating process and the missingness generating process at the same time[^ml], and does not require explicit imputation of missing values. 
A drawback of this method is the restriction to models that can be estimated via maximum likelihood[^mlelab]. 
The former instead is based on production of multiple complete datasets, on which separate models are fitted and whose estimates are then pooled.

[^infcontext]: Cite examples; kind of a difficult statement to properly evaluate. Stated in a different format [@shmueli2010] on page 296.

[^bs]: Statement needs a lot more precision, and citations

[^ml]: Add citations

[^mimlorder]: Might want to switch up the order, or not.

[^naive]: Find citation from Van Buuren. Use quote about machine learning imputation being the even more dangerous version of regression imputation

[^mlelab]: Elaborate, examples of models both MLE and not MLE

#### Prediction

A less commonly studied problem[^vague] is missingness handling in the prediction, as opposed to explanation. 
See [@breiman2001] and [@shmueli2010] for discussion of the differences of predictive and explanatory statistics[^predvsexp]. 

An important observation regarding the distinction is that the *theoretically correct model* may not be the *best model* with regards to prediction accuracy[@shmueli2010].

#### Missing values at estimation time

Most studies introducing imputation methods focus on problems where missingness may occur in the model estimation phase, but where data is assumed to be complete at prediction time. 
In this context, it suffices to design methods which facilitate model estimation[^estim] in a way that maximizes prediction accuracy.

#### Missing values both at estimation and prediction time

Sarle notes that "The usual characterizations of missing values as *missing at random* or *missing completely at random* are important for estimation but not prediction."[@sarle1998][^inlinequote] Ding & Simonoff [@ding-simonoff2010] provide real-world evidence[^realworld] in support of this statement in the use of classification trees[^trees].
In a predictive context, the presence of *informative missingness* (as defined by Sarle[@sarle1998]) in the data, i.e. missingness being dependent on the response variable conditional on $X_{obs}$, may actually lead to improved predictive accuracy compared to complete data.[@citeproperly]

One additional challenge that arises in this context is that most imputation methods are not implemented in a way that easily allows reuse of learned parameters. That is, it is difficult to first estimate imputation method parameters on the training set, and then impute the test set using those same parameters. This leads to diminished prediction accuracy, as the data the distributions of imputed data differ in the training and test sets. Even worse, since the parameters for test set imputation are estimated from the test set, the content and size of the test set itself may affect prediction accuracy.

[^vague]: can we to quantify this?

[^trees]: Check this

[^predvsexp]: Reread these and possibly elaborate

[^inlinequote]: Check correct academic formatting for this sort of inline quote

[^realworld]: Can I use this expression?

- Saar-Tsechansky & Provost
    - Predictive value imputation vs. distribution-based imputation

[^estim]: Should I use "model estimation" as a term at all, and just refer to model training? There are two perspectives here, the machine learning and the statistics perspective. Machine learning might be considered a subset of predictive statistics.
