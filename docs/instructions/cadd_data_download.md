# Matching CADD training data and ClinVar on the fly

In order to enable processing on an ordinary machine, we can avoid storing of the full 300 GB file on-disk.
Instead, we store only the rows which refer to ClinGen variants.

First, we create a file containing the `CHROM`, `POS`, `REF`, and `ALT` fields of those ClinVar variants that belong to the ClinGen subset.

```
$ egrep "reviewed_by_expert_panel|practice_guideline" clinvar_20190624.vcf | cut -f 1,2,4,5 > clingen_variants.tsv
```

Next, we filter the CADD SNV file while we download it with the patterns from the above-created file, leaving only those rows that represent variants found in the ClinGen subset.

```
$ curl https://krishna.gs.washington.edu/download/CADD/v1.5/GRCh38/whole_genome_SNVs_inclAnno.tsv.gz  | gzip -cd | grep -wFf clingen_variants.tsv > CADD_anno.tsv
```

We separately download the header for the file.

```
$ curl https://krishna.gs.washington.edu/download/CADD/v1.5/GRCh38/whole_genome_SNVs_inclAnno.tsv.gz  | gzip -cd | head -n 2 > CADD_header
```

In the same way, we filter and download the CADD indel file and its header.
```
$ curl https://krishna.gs.washington.edu/download/CADD/v1.5/GRCh38/InDels_inclAnno.tsv.gz  | gzip -cd | grep -wFf clingen_variants.tsv > CADD_anno_indel.tsv
```

```
$ curl https://krishna.gs.washington.edu/download/CADD/v1.5/GRCh38/InDels_inclAnno.tsv.gz  | gzip -cd | head -n 2 > CADD_anno_indel_header
```

Finally, we concatenate the files with their headers.

```
$ cat CADD_header > CADD_clingen.tsv && cat CADD_anno.tsv >> CADD_clingen.tsv
$ cat CADD_anno_indel_header > CADD_clingen_indel.tsv && cat CADD_anno_indel.tsv >> CADD_clingen_indel.tsv
```

The resulting files are tiny compared to the original CADD files.

```
$ du -h CADD_anno_indel.tsv CADD_clingen.tsv
3.2M	CADD_anno_indel.tsv
5.0M	CADD_clingen.tsv
```
