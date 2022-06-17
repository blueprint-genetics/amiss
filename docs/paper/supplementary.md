# Supplementary information

Figures \ref{figure:rf-rmse} and \ref{figure:lr-rmse} show downstream classifier performance in the simulation experiment, plotted against RMSE across columns for a subset of the imputation methods, plotted with equal scales for each method. Outlier imputation is excluded here since its RMSE values are much higher than other methods. As can be seen in the figure, RMSE and MCC are not correlated, especially well illustrated by maximum, minimum and zero imputation, where high MCC is reached even as RMSE is consistently higher than for other methods. 

Figures \ref{figure:rf-rmse-free} and \ref{figure:lr-rmse-free} are as above, except with only the MCC scale fixed. Outlier imputation and BPCA have drastically higher RMSE, but outlier imputation still shows good MCC values with a random forest classifier.

Figures \ref{figure:sensitivity-boxplots-perconseq} and \ref{figure:specificity-boxplots-perconseq} show the sensitivity and specificity, respectively, for test set variants of different consequence classes in the main experiment. Logistic regression seems to have little discriminatory power in `INTRONIC` variants when combined with most missingness handling methods, as sensitivity is near zero, implying all variants in this consequence class are being classified as benign. Missingness indicators, outlier imputation and maximum imputation seem to allow the method to better detect pathogenic variants, though still at a lower rate than random forest.

Figure \ref{figure:feature-to-outcome-correlation} shows correlations in the training data from feature values to the positive outcome indicator.

Figure \ref{figure:feature-missingness-to-positive-outcome-corr} shows correlations in the training data from the missingness indicators of each feature to the positive outcome indicator. Consequence.x, CpG, EncodeDNAse.sum, EncodeH3K9ac.sum, GC, Length, minDistTSE and minDistTSS have no missing values and thus have an empty correlation bar. For most features, the correlation is negative, implying that missingness in those features decreases the likelihood of pathogenicity. Correlations from missingness indicators of EncodetotalRNA.sum, gnomAD_exomes_AF and gnomAD_genomes_AF are positive, implying that missingness in those features increases the likelihood of pathogenicity.

Figures \ref{figure:feature_histograms_pg1} through \ref{figure:feature_histograms_pg8} show the histograms of feature values for each feature on the training set.

![Random forest MCC against RMSE averaged across columns, equal scales \label{figure:rf-rmse}](supp_figures/rf_rmses_fixed_scale.pdf){height=100%, width=100%}

![Logistic regression MCC against RMSE averaged across columns, equal scales \label{figure:lr-rmse}](supp_figures/lr_rmses_fixed_scale.pdf){height=100%, width=100%} 

![Random forest MCC against RMSE averaged across columns, free `x` scale \label{figure:rf-rmse-free}](supp_figures/rf_rmses_free_x.pdf){height=100%, width=100%}

![Logistic regression MCC against RMSE averaged across columns, free `x` scale \label{figure:lr-rmse-free}](supp_figures/lr_rmses_free_x.pdf){height=100%, width=100%} 

![Sensitivity boxplots for both RF and LR classifiers, conditional on variant consequence.\label{figure:sensitivity-boxplots-perconseq}](supp_figures/Sensitivity_double_boxplots_perconseq.pdf){height=100%, width=100%} 

![Specificity boxplots for both RF and LR classifiers, conditional on variant consequence. \label{figure:specificity-boxplots-perconseq}](supp_figures/Specificity_double_boxplots_perconseq.pdf){height=100%, width=100%} 

![Correlations of features to positive outcome indicator. \label{figure:feature-to-outcome-correlation}](supp_figures/feature_to_outcome_correlation.pdf){height=100%, width=100%} 

![Correlations of features' missingness indicators to positive outcome indicator. \label{figure:feature-missingness-to-positive-outcome-corr}](supp_figures/feature_missingness_to_outcome_correlation.pdf){height=100%, width=100%} 

\begin{figure}
\includegraphics[page=1]{supp_figures/feature_histograms.pdf}
\caption{Histograms of the observed values of features, page 1.  \label{figure:feature_histograms_pg1}}
\end{figure}

\begin{figure}
\includegraphics[page=2]{supp_figures/feature_histograms.pdf}
\caption{Histograms of the observed values of features, page 2.  \label{figure:feature_histograms_pg2}}
\end{figure}

\begin{figure}
\includegraphics[page=3]{supp_figures/feature_histograms.pdf}
\caption{Histograms of the observed values of features, page 3.  \label{figure:feature_histograms_pg3}}
\end{figure}

\begin{figure}
\includegraphics[page=4]{supp_figures/feature_histograms.pdf}
\caption{Histograms of the observed values of features, page 4.  \label{figure:feature_histograms_pg4}}
\end{figure}

\begin{figure}
\includegraphics[page=5]{supp_figures/feature_histograms.pdf}
\caption{Histograms of the observed values of features, page 5.  \label{figure:feature_histograms_pg5}}
\end{figure}

\begin{figure}
\includegraphics[page=6]{supp_figures/feature_histograms.pdf}
\caption{Histograms of the observed values of features, page 6.  \label{figure:feature_histograms_pg6}}
\end{figure}

\begin{figure}
\includegraphics[page=7]{supp_figures/feature_histograms.pdf}
\caption{Histograms of the observed values of features, page 7.  \label{figure:feature_histograms_pg7}}
\end{figure}

\begin{figure}
\includegraphics[page=8]{supp_figures/feature_histograms.pdf}
\caption{Histograms of the observed values of features, page 8.  \label{figure:feature_histograms_pg8}}
\end{figure}

