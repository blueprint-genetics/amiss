# Supplementary information

Figures \ref{figure:rf-rmse} and \ref{figure:lr-rmse} show downstream classifier performance in the simulation experiment, plotted against RMSE across columns for a subset of the imputation methods, plotted with equal scales for each method. Outlier imputation is excluded here since its RMSE values are much higher than other methods. As can be seen in the figure, RMSE and MCC are not correlated, especially well illustrated by maximum, minimum and zero imputation, where high MCC is reached even as RMSE is consistently higher than for other methods. 

![Random forest MCC against RMSE averaged across columns, equal scales \label{figure:rf-rmse}](supp_figures/rf_rmses_fixed_scale.pdf){height=100%, width=100%}

![Logistic regression MCC against RMSE averaged across columns, equal scales \label{figure:lr-rmse}](supp_figures/lr_rmses_fixed_scale.pdf){height=100%, width=100%} 

Figures \ref{figure:rf-rmse-free} and \ref{figure:lr-rmse-free} are as above, except with only the MCC scale fixed. Outlier imputation and BPCA have drastically higher RMSE, but outlier imputation still shows good MCC values with a random forest classifier.

![Random forest MCC against RMSE averaged across columns, free `x` scale \label{figure:rf-rmse-free}](supp_figures/rf_rmses_free_x.pdf){height=100%, width=100%}

![Logistic regression MCC against RMSE averaged across columns, free `x` scale \label{figure:lr-rmse-free}](supp_figures/lr_rmses_free_x.pdf){height=100%, width=100%} 

![Sensitivity boxplots for both RF and LR classifiers, conditional on variant consequence.\label{figure:sensitivity-boxplots-perconseq}](supp_figures/Sensitivity_double_boxplots_perconseq.pdf){height=100%, width=100%} 

![Specificity boxplots for both RF and LR classifiers, conditional on variant consequence. \label{figure:specificity-boxplots-perconseq}](supp_figures/Specificity_double_boxplots_perconseq.pdf){height=100%, width=100%} 

![Correlations of features to positive outcome indicator. \label{figure:feature-to-outcome-correlation}](supp_figures/feature_to_outcome_correlation.pdf){height=100%, width=100%} 

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


Feature                         Correlation
----------                      -------------
SIFT_score                      $-0.189378772$
LRT_score                       $-0.159592305$
LRT_Omega                       $-0.159592305$
MutationTaster_score            $-0.182399579$
MutationAssessor_score          $-0.182245651$
FATHMM_score                    $-0.182399579$
PROVEAN_score                   $-0.189378772$
MutPred_score                   $-0.329505858$
fathmm.MKL_coding_score         $-0.181899687$
GenoCanyon_score                $-0.181899687$
integrated_fitCons_score        $-0.181899687$
GERP.._NR                       $-0.181899687$
GERP.._RS                       $-0.181899687$
phyloP100way_vertebrate         $-0.181899687$
phyloP20way_mammalian           $-0.181899687$
phastCons100way_vertebrate      $-0.181899687$
phastCons20way_mammalian        $-0.181899687$
SiPhy_29way_logOdds             $-0.181899687$
gnomAD_exomes_AF                NA
gnomAD_genomes_AF               NA
Length                          NA
GC                              NA
CpG                             NA
relcDNApos                      $-0.147305537$
relCDSpos                       $-0.191722091$
relProtPos                      $-0.191722091$
Dst2Splice                      $-0.144680026$
minDistTSS                      NA
minDistTSE                      NA
mirSVR.Score                    $-0.013687644$
mirSVR.E                        $-0.013687644$
mirSVR.Aln                      $-0.013687644$
EncodeH3K4me1.sum               $-0.072553296$
EncodeH3K4me2.sum               $-0.057581662$
EncodeH3K4me3.sum               $-0.028741015$
EncodeH3K9ac.sum                NA
EncodeH3K9me3.sum               $-0.040669376$
EncodeH3K27ac.sum               $-0.033193642$
EncodeH3K27me3.sum              $-0.072553296$
EncodeH3K36me3.sum              $-0.046979023$
EncodeH3K79me2.sum              $-0.029970682$
EncodeH4K20me1.sum              $-0.049270622$
EncodeH2AFZ.sum                 $-0.070604548$
EncodeDNase.sum                 NA
EncodetotalRNA.sum              $0.127156815$
RemapOverlapTF                  $-0.227295102$
RemapOverlapCL                  $-0.227295102$
LRT_pred.NA                     NA
LRT_pred.U                      NA
LRT_pred.D                      NA
LRT_pred.N                      NA
Dst2SplType.NA                  NA
Dst2SplType.DONOR               NA
Dst2SplType.ACCEPTOR            NA
Consequence.x.DOWNSTREAM        NA
Consequence.x.NON_SYNONYMOUS    NA
Consequence.x.SPLICE_SITE       NA
Consequence.x.UPSTREAM          NA
Consequence.x.STOP_GAINED       NA
Consequence.x.INTRONIC          NA
Consequence.x.CANONICAL_SPLICE  NA
Consequence.x.FRAME_SHIFT       NA
Consequence.x.SYNONYMOUS        NA
Consequence.x.5PRIME_UTR        NA
Consequence.x.INFRAME           NA
Consequence.x.3PRIME_UTR        NA
Consequence.x.NONCODING_CHANGE  NA

Table: Correlation of feature missingness indicator to positive outcome indicator.
