# Discussion

Random forest classifiers defeated logistic regression overall, with the worst-performing combination of missingness handling method and random forest still reaching higher performance than best-performing combination of missingness handling method and logistic regression. This was expected, due to both the inherent capability of tree predictors such as those in random forest to ignore irrelevant features, and the more flexible decision boundary that random forests are able to form. Choice of missingness handling method thus cannot compensate for an unsuitable classification method.

The high comparative performance of k-NN with both classifiers is surprising. The depletion of complete cases greatly limits the possible values for $k$. Even more importantly, the few complete cases are the only possible neighbors to any point; when $k$ equals the number of complete cases, every missing value in a specific feature will be imputed with the average value of that feature in the set of complete cases. k-NN imputation with $k$ equaling the number of complete cases can thus be interpreted to be a form of unconditional imputation, since in such a case every missing value will be imputed with a single value, irrespective of the values of other features.

The computational cost of different methods varies dramatically. Many of the highest-performing methods take a minuscule portion of time spent during the overall training and prediction process, while more costly methods dominate the time requirements. The difference between zero imputation ($0.007$ second runtime on our dataset) to MissForest ($363.018$ seconds) may be inconsequential for a single sample, but is compounded with large cohorts or high-throughput diagnostic work. For example, for $100\ 000$ WGS samples the difference in total computational time with the simplest and most complex method will be over a year, potentially translating into hundreds of thousands of euros of additional cloud computing and storage costs.

Simple imputation methods overall could be considered the winning group of the comparison presented in this paper, but mean imputation might further be pointed out as the favorite in that group. In addition to the consistently high MCC in all experiments, it also displays high resilience to performance degradation due to increasing MCAR missingness, and low variance in relation to sampling. It is also one of the fastest methods to compute, trivial to implement and is always applicable.

Considering the complexity of the prediction task, the features, the large degree of missingness, and the sophistication of available missingness handling strategies, it is somewhat surprising that the best performance is gained using simple unconditional imputation strategies.

## Possible explanations to simple imputation superiority

### Informativeness

A putative explanation is the presence of some informativeness in the missingness mechanism. If the presence of a missing value in certain variables correlates with the pathogenicity of the variant, simple imputation methods would be given an advantage: when every imputed value (within a given feature) is replaced by a single specific value, the classifier may learn to correlate that single value with the outcome. This is less likely to happen when using more sophisticated imputation methods, which make it harder for the classifier to learn which values were likely imputed.
However, logistic regression is less flexible and thus less capable of representing such potentially discontinuous dependencies, and yet missingness indicator augmentation did not perform dramatically better than most simple imputation methods. This would imply that informativeness does not hold a large role, even though we did find it to be present in the data. It may also be that the dimensionality increase due to the additional indicator features disadvantages the classifier enough to neutralize the benefit from informativeness.

### Inability to transfer training parameters to test set

As described in [Methods and materials](#parameter-reuse), many available implementations of imputation methods do not allow reuse of parameters between imputation of training and test sets. Thus the estimated distributions on which imputed values are based will differ at least slightly, and thus disadvantage any methods for which parameter reuse was not possible.

## Additional challenges

Grimm et al. [@grimm2015] described several biasing factors in variant effect predictor training and performance evaluation using data from commonly used variant databases, e.g. the tendency for variants within the same gene being classified as all pathogenic or all neutral, or simply due to difficulty of finding datasets completely disjoint with the training set. Mahmood et al. [@mahmood2017] further analysed existing variant effect predictors using datasets generated from functional assays, and found drastically lower performance compared to earlier reported estimates.

Our approach is not immune to these biases, and we expect that any reported performance metrics will be overoptimistic. However, we expect that the main result of the study, the relative performance rankings of missingness handling methods, is not affected by the biases. The classifiers built described in this work are not intended to outperform earlier approaches or be directly used for variant effect prediction.

## Conclusions

It appears that it is unnecessary to use sophisticated missingness handling methods to treat missing values when building variant pathogenicity metapredictors. Instead, simple unconditional imputation methods and even zero imputation give higher performance and save significant computational time, leading to considerable cost savings if adopted. This highlights the conceptual separation between missingness handling methods for prediction and imputation for statistical inference, the latter of which requires carefully constructed techniques to reach correct conclusions.

## Further work

There are several ways to improve and expand on this work. The dataset could be extended to include variants from wider sources, and the effect of [circularity](#circularity) could be estimated using additional datasets. It would likely also be possible to make changes to or reimplement methods whose implementation does not currently support reuse of parameters. Reduced models and its hybrid variants would make an interesting point of comparison if implemented. Another possible extension of the work would be to broaden the focus from missingness handling to various other design choices that may affect predictor performance, such as using random search in place of grid search or downsampling data to improve class balance.

# Potential implications

We compared a variety of commonly used missingness handling methods in order to assess their suitability for using machine learning to build variant pathogenicity metapredictors. The analysis will help pathogenicity predictor researchers choose missingness handling methods to maximize the performance of their tools. 
