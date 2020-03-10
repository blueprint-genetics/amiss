
numeric_features <- make.names(c(
  # From dbNSFP
  "SIFT_score",
  "LRT_score",
  "LRT_Omega",
  "MutationTaster_score",
  "MutationAssessor_score",
  "FATHMM_score",
  "PROVEAN_score",
  "M-CAP_score",
  "MutPred_score",
  "fathmm-MKL_coding_score",
  "GenoCanyon_score",
  "integrated_fitCons_score",
  "GERP++_NR",
  "GERP++_RS",
  "phyloP100way_vertebrate",
  "phyloP20way_mammalian",
  "phastCons100way_vertebrate",
  "phastCons20way_mammalian",
  "SiPhy_29way_logOdds",
  "gnomAD_exomes_AF",
  "gnomAD_genomes_AF",

  # From CADD annotations
  "Length",
  "GC",
  "CpG",
  "motifECount",
  "motifEHIPos",
  "motifEScoreChng",
  # "(Intron)",
  # "(Exon)",
  "relcDNApos",
  "relCDSpos",
  "relProtPos",
  "Dst2Splice",
  "minDistTSS",
  "minDistTSE",
  # "targetScan",
  "mirSVR-Score",
  "mirSVR-E",
  "mirSVR-Aln",
  "tOverlapMotifs",
  "motifDist",
  "EncodeH3K4me1-sum",
  "EncodeH3K4me2-sum",
  "EncodeH3K4me3-sum",
  "EncodeH3K9ac-sum",
  "EncodeH3K9me3-sum",
  "EncodeH3K27ac-sum",
  "EncodeH3K27me3-sum",
  "EncodeH3K36me3-sum",
  "EncodeH3K79me2-sum",
  "EncodeH4K20me1-sum",
  "EncodeH2AFZ-sum",
  "EncodeDNase-sum",
  "EncodetotalRNA-sum",
  # "Grantham",
  "RemapOverlapTF",
  "RemapOverlapCL"
))

categorical_features <- make.names(c(
  # From dbNSFP
  "LRT_pred",

  # From CADD annotations
  "Dst2SplType",
  "Consequence.x"
))

default_imputations <- list(
  motifECount = 0,
  motifEScoreChng = 0,
  motifEHIPos = 0,
  tOverlapMotifs = 0,
  motifDist = 0,
  gnomAD_genomes_AF = 0,
  gnomAD_exomes_AF = 0
)