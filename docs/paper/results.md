# Results

This section is arranged as follows. First, we present results from the added-missingness simulations. We evaluate both the relation of downstream classifier performance and RMSE, and how increasing missingness percentage affects downstream classifier performance. Next, we present results from the repeated random sub-sampling cross-validation experiment, shedding light on robustness of missingness handling methods to dataset composition. We then present results pertaining the main experiment, comparing the downstream classifier performances of missingness handling methods. Finally, we compare the missingness handling methods with respect to their running times.

## Simulation experiments

### RMSE

We found that RMSE and classification performance as measured by MCC did not correlate in datasets with simulated additional missing values (for plots of RMSE against MCC for each imputation method, see supplementary information). This is especially clear in the case of outlier imputation, where RMSE is--as expected--much higher than with any other method, while MCC was comparable to other methods.

### Missingness percentage

The effect of additional MCAR missingness on MCC performance of downstream classifiers is displayed in figures \ref{figure:rf-missperc} and \ref{figure:lr-missperc}. Fitted LOESS (locally estimated scatterplot smoothing) curves are shown. 

When the downstream classifier is a random forest, missingness indicator augmentation as well as mean, minimum, zero and median imputations show similar curves, with their average performances dropping from slightly below $\mathrm{MCC} = 0.85$ at $30~\%$ missingness to slightly above $\mathrm{MCC} = 0.75$ at $70~\%$ missingness. MICE random forest and MICE PMM have overall slightly lower mean performance than the previously mentioned methods, but otherwise show similar shapes.
Outlier and maximum imputations suffer more drastically, with mean performances dropping to just above $\mathrm{MCC} = 0.70$ at $70~\%$ missingness. MICE regression and MICE Bayes regression demonstrate a curious effect where average downstream classifier performance increases at first as missingness increases, before starting their descent. BPCA shows hints of a similar but more muted trend. This may be related to a phenomenon noted by Poulos \& Valle [@poulos-valle2018] in the context of prediction on categorical variables, where introduction of additional missing values prior to imputation may improve classifier performance.

![Random forest MCC against actual missingness percentage, with fitted LOESS curves (red). \label{figure:rf-missperc}](figures/rf_mcc_versus_miss_pct_observed.pdf){height=100%, width=100%}

When the downstream classifier is logistic regression, outlier imputation shows a dramatic drop immediately between $30~\%$ and $40~\%$ missingness and stabilises slightly above $\mathrm{MCC} = 0.20$. Maximum imputation shows a clear linear downward trend, while minimum imputation, zero imputation and BPCA show a much smaller one. Missingness augmentation and all MICE methods show very light reductions in average performance as missing value percentage grows. Median and especially mean imputation show practically no performance reduction due to increasing MCAR missingness. MICE Bayes regression, BPCA and PMM and especially MICE regression all show much larger variability in their performance than other methods.

![Logistic regression MCC against actual missingness percentage, with fitted LOESS curves (red). \label{figure:lr-missperc}](figures/lr_mcc_versus_miss_pct_observed.pdf){height=100%, width=100%}

## Cross-validation experiment {#cross-val-results}

The variability of downstream classifier performance evaluated via repeated random sub-sampling cross-validation is displayed in figure \ref{figure:cv-perf}. 

![Boxplots describing down-stream classifier performance estimated via repeated random sub-sampling cross-validation. Diamonds were added to emphasize median values. The training set was further randomly split $100$ times into $70 \% / 30 \%$ subsets, and the framework was executed for each of the resulting pairs. \label{figure:cv-perf}](figures/si_cv_double_boxplots.pdf){height=100%, width=100%}

The random forest classifier outperforms logistic regression regardless of imputation method, and the lowest performing imputation method wrt. random forest classifiers (MissForest) has higher mean performance than the highest performing imputation method wrt. logistic regression (missingness indicators).  

For both logistic regression and random forest, single imputation methods and k-NN appear to have equivalent performance, though in conjunction with logistic regression outlier imputation seems to perform slightly worse. With regard to random forest classifiers, MICE random forest, BPCA, MICE Bayes regression, and MICE PMM perform slightly or somewhat worse than simple imputation methods, but display greater differences in conjunction with logistic regression, where MICE random forest and BPCA are clearly preferable to MICE Bayes regression and MICE PMM. MICE ordinary regression and MissForest perform worse on average with both classifier types, but when combined with logistic regression, MICE ordinary regression brings mean classification performance down close to that of a coin flip.

## Main experiment

In order to assess the differences in downstream classifier performance due to selection of imputation method, we show mean performance metrics for classifiers trained on datasets produced by each missingness handling method, sorted by MCC, for random forest classifiers (table\ \ref{table:rf-perf}) and for logistic regression classifiers (table\ \ref{table:lr-perf}). Single imputation methods (mean, missingness indicator, median, k-NN, zero, minimum, maximum, outlier) produced only a single dataset, and as such also produced only a single set of performance statistics. MICE ordinary regression imputation was unable to produce imputations in the main experiment, and is thus excluded.

In the case of random forest classification, maximum imputation has the best average performance with respect to MCC\ ($= 0.848$). It is very closely followed by missingness indicator augmentation, k-NN imputation and mean imputation, and then zero, median, minimum and outlier imputation methods. MICE random forest imputation is the highest performing MICE method (${\mathrm{MCC}=0.817}$), with slightly better MCC than Bayes regression and PMM. The final group, with distinctly worse downstream classifier MCC, consists of BPCA ($\mathrm{MCC} = 0.781$) and MissForest ($\mathrm{MCC} = 0.756$).

Method                     MCC      AUC-ROC   Sensitivity Specificity $F_1$   Precision
-------------------------  ----     --------  ----------- ----------- ------  -----------
Maximum                    $0.848$  $0.975$   $0.912$     $0.937$     $0.913$ $0.914$
Missingness indicators     $0.846$  $0.975$   $0.908$     $0.937$     $0.910$ $0.913$
k-NN imputation            $0.844$  $0.973$   $0.918$     $0.930$     $0.911$ $0.905$
Mean                       $0.844$  $0.974$   $0.914$     $0.934$     $0.912$ $0.910$
Zero                       $0.840$  $0.976$   $0.905$     $0.933$     $0.906$ $0.907$
Median                     $0.839$  $0.975$   $0.916$     $0.925$     $0.907$ $0.899$
Minimum                    $0.838$  $0.975$   $0.903$     $0.934$     $0.906$ $0.909$
Outlier                    $0.837$  $0.974$   $0.908$     $0.930$     $0.906$ $0.904$
MICE RF                    $0.817$  $0.976$   $0.889$     $0.927$     $0.894$ $0.898$
MICE Bayes regression      $0.812$  $0.975$   $0.876$     $0.932$     $0.890$ $0.904$
MICE PMM                   $0.812$  $0.975$   $0.869$     $0.936$     $0.888$ $0.909$
BPCA                       $0.781$  $0.974$   $0.779$     $0.976$     $0.860$ $0.959$
MissForest                 $0.756$  $0.952$   $0.818$     $0.927$     $0.853$ $0.892$


Table: Mean test set performance metrics for a random forest classifier trained and tested on data sets treated with each missingness handling method \label{table:rf-perf}

Mean classification performance is lower across the board for logistic regression. Missingness indicator augmentation is the winner in this scenario with $\mathrm{MCC} = 0.700$. k-NN imputation ($\mathrm{MCC} = 0.682$) and BPCA ($\mathrm{MCC} = 0.680$) receive second and third place, after which mean imputation, zero imputation, minimum imputation, maximum imputation, outlier and median imputations have essentially equal performance (MCC ranging from $0.674$ to $0.669$). MissForest and MICE methods (MCC between $0.645$ and $0.591$) perform noticeably worse.

Method	                MCC	    AUC-ROC Sensitivity	Specificity	$F_1$	Precision
----------------------- ------- ------- ----------- ----------- ------  ----------
Missingness indicators	$0.700$ $0.925$ $0.840$     $0.862$     $0.828$ $0.816$
k-NN	                $0.682$ $0.924$ $0.817$     $0.865$     $0.816$ $0.816$
BPCA	                $0.680$ $0.923$ $0.828$     $0.855$     $0.817$ $0.806$
Mean	                $0.674$ $0.920$ $0.815$     $0.859$     $0.812$ $0.808$
Zero	                $0.673$ $0.923$ $0.809$     $0.864$     $0.811$ $0.812$
Minimum	                $0.672$ $0.922$ $0.813$     $0.859$     $0.810$ $0.808$
Maximum	                $0.670$ $0.914$ $0.828$     $0.846$     $0.812$ $0.796$
Outlier	                $0.670$ $0.906$ $0.863$     $0.813$     $0.815$ $0.771$
Median	                $0.669$ $0.921$ $0.815$     $0.855$     $0.809$ $0.803$
MissForest	            $0.645$ $0.929$ $0.695$     $0.921$     $0.769$ $0.868$
MICE RF	                $0.642$ $0.909$ $0.761$     $0.874$     $0.787$ $0.815$
MICE PMM	            $0.601$ $0.898$ $0.709$     $0.874$     $0.751$ $0.810$
MICE Bayes regression	$0.591$ $0.879$ $0.785$     $0.805$     $0.763$ $0.750$

Table: Mean test-set performance metrics for a logistic regression classifier trained and tested on data sets treated with each missingness handling method \label{table:lr-perf}

![Boxplots describing down-stream classifier performance with respect to MCC. Diamonds were added to emphasize median values. Variance represents variation in probabilistic or multiple imputation, with $10$ classifiers, each trained on a training set imputed with the same method, used to predict on $10$ test sets imputed using the same method. \label{figure:mcc_boxplots}](figures/MCC_double_boxplots.pdf){height=100%, width=100%}

![Boxplots describing down-stream classifier performance with respect to AUC-ROC. Diamonds were added to emphasize median values. Variance represents variation in probabilistic or multiple imputation, with $10$ classifiers, each trained on a training set imputed with the same method, used to predict on $10$ test sets imputed using the same method. \label{figure:auc_boxplots}](figures/AUC_double_boxplots.pdf){height=100%. width=100%}

The MCC and AUC-ROC performances of downstream classifiers are illustrated in figures \ref{figure:mcc_boxplots} and \ref{figure:auc_boxplots}, respectively. For multiple imputation and MissForest the variability of performance due to randomness in the imputation is also visible. Variability in downstream classifier performance is seen to be smaller for random forest compared to logistic regression in all methods except for MissForest, and variability in general larger in methods with lower mean performance. 

Using AUC-ROC to measure performance makes it more difficult to compare imputation methods due to the very small absolute differences. For a random forest classifier (table \ref{table:rf-perf}), the mean AUC-ROC of all methods, with the exception of MissForest, is within $0.002$ of each other. For logistic regression (table \ref{table:lr-perf}), the range is wider, and is also visually distinguishable in figure \ref{figure:auc_boxplots}. Interestingly, here MissForest has the upper hand. However, looking back at table \ref{table:lr-perf} we notice that MissForest shows the highest specificity but also the lowest sensitivity of all methods. The imbalance is reflected in the relatively poor MCC and $F_1$ scores, but ignored by AUC-ROC.

The results are not very different from the [cross-validation experiment results](#figure:cv-perf). When compared using MCC, simple imputation methods are largely interchangeable, though missingness indicators dominate when using a logistic regression classifier, and maximum imputation is dominant when using a random forest classifier. However, when considering the variance shown in the cross-validation experiment, the differences between simple imputation methods are likely to be due to chance. It is important to note that here the variance for multiple imputation methods is due to the multiple imputation (or multiple runs of a probabilistic imputation method) of training and test sets, and thus is not directly comparable to that of the cross-validation experiments. MICE ordinary regression is not included here due to it failing in the main experiment.

### Results grouped by consequence

To assess whether the strongly distinct missingness patterns exhibited within different variant consequence classes, we also computed performance statistics separately within certain consequence classes, specifically `DOWNSTREAM`, `UPSTREAM`, `INTRONIC`, and `Other`, an aggregate of all remaining consequence classes. `Other` is formed by the consequence classes for which either 1) there are overall few variants, and the class indicator gets filtered out due to non-zero variance, or 2) there is high correlation with another feature. Results are shown in figure \ref{figure:mcc_boxplots_perconseq}.

Consequence          Occurrences
------------        ------------
`INFRAME`           14
`NONCODING_CHANGE`  14
`5PRIME_UTR`        18
`3PRIME_UTR`        27
`SPLICE_SITE`       53
`DOWNSTREAM`        464
`NON_SYNONYMOUS`    578
`UPSTREAM`          632
`INTRONIC`          806

Table: Numbers of occurrence of variant consequence classes.\label{conseqfreq}

Some consequence classes were indeed very sparse, as seen in table \ref{conseqfreq}, and the pre-imputation filtering process eliminated the dummy variables encoding class membership of `INFRAME`, `NONCODING_CHANGE`, `5PRIME_UTR`, `3PRIME_UTR`, and `SPLICE_SITE`. 
In addition, membership to class `NON_SYNONYMOUS` was strongly (negatively) correlated to the missingness of LRT predictions, and was also eliminated by the pre-imputation feature filtering. Thus these classes were grouped into `Other`.

![Boxplots describing down-stream classifier performance with respect to MCC, conditional on variant consequence. Diamonds were added to emphasize median values. Variance represents variation in probabilistic or multiple imputation, with $10$ classifiers, each trained on a training set imputed with the same method, used to predict on $10$ test sets imputed using the same method. \label{figure:mcc_boxplots_perconseq}](figures/MCC_double_boxplots_perconseq.pdf){height=100%, width=100%}

Performances show large differences in different consequence classes. `Other` is mostly non-synonymous variants, as described above, and shows good performance for both classifier types, though random forest is always superior. Mean imputation performs best with the random forest classifier, while missingness indicator augmentation and minimum imputation maximize logistic regression performance. The results are essentially equal to the general results depicted in figure \ref{figure:mcc_boxplots} where consequence classes are not distinguished.

The consequence class `INTRONIC` shows a very different and interesting situation. First it is important to note that even after the preprocessing steps which removed variants in consequence classes with high class imbalance, only $\approx 8.1~\%$ of `INTRONIC` variants are positive. However, the robustness of MCC to class imbalance allows us to compare the performances. In this consequence class, logistic regression is virtually useless, though missingness indicator augmentation and maximum imputation seem to allow some discriminatory power. 
It seems that in `INTRONIC`, logistic regression learns to classify almost everything as negative (see supplementary information for plots of sensitivity and specificity conditional on consequence class).
With most imputation methods, MCC is barely above zero. In contrast, the random forest classifier performs well with any imputation method, though the high variation in MissForest imputations seems to allow also for instances of poor performance.  

Compared to `Other`, prediction on variants of `DOWNSTREAM` consequence shows lower performance in both classifiers. For random forest, the relative orders of imputation methods are basically the same as in the main experiment and the cross-validation experiment. Unlike in all other consequence classes, logistic regression seems to perform better with MICE methods and MissForest than most simple imputation methods and k-NN, though maximum and outlier imputation do seem to outperform them even here.

In `UPSTREAM` variants, performance is again lower than in `Other`, but not as low as in `DOWNSTREAM`. Random forest classification shows the common pattern, where simple imputation methods and k-NN seem slightly preferable to MICE methods, and MissForest performs noticeably worse. Logistic regression classification looks, in this case, to be disadvantaged by the same methods that dominated `DOWNSTREAM` (that is, maximum and outlier imputation).

As mentioned earlier, minor differences between simple imputation methods have a good chance of being attributable to randomness. 

## Running time

Running times were recorded in the main experiment for the best hyperparameter configurations of all imputation methods and are presented in table \ref{runtimes}. For methods that were used to produce multiple datasets (MICE methods and MissForest) the overall time was recorded and then divided by the number of produced datasets ($10$). The machine used to run the software was a CentOS Linux server with two Intel Xeon CPU E5-2650 v4 @ 2.20GHz processors and $512$ GiB of memory. The analysis was run with parallelization using $24$ processes. Each imputation was run inside a single process, and thus running times should not be affected based on whether an imputation method itself offers parallelization features.



----------------------------------------------------------------------------
Method                                    LR (elapsed (s))  RF (elapsed (s))
------------------------------------    ------------------ -----------------
Zero                                    $0.006$             $0.006$

Maximum                                 $0.007$             $0.007$

Minimum                                 $0.007$             $0.007$

Mean                                    $0.009$             $0.009$

Median                                  $0.011$             $0.011$

Outlier                                 $0.014$             $0.014$

Missingness indicator augmentation      $0.478$             $0.478$

MICE Bayes regression                   $5.379$             $5.379$

MICE regression                         $5.839$             $5.839$

k-NN                                    $12.792$            $12.948$

BPCA                                    $14.884$            $19.511$

MICE PMM                                $15.427$            $14.921$

MICE RF                                 $107.036$           $104.278$

MissForest                              $362.352$           $362.352$

---------------------------------------------------------------------------

Table: Running time for imputing the training set with the best hyperparameter configurations in the main experiment.\label{runtimes}

MICE regression and MICE Bayes regression show similar running times and finish between 5 and 6 seconds, while k-NN, MICE PMM and BPCA take somewhat longer. MICE random forest and especially MissForest take much longer. Simple imputation methods are much faster than all other methods.

