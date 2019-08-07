# Annotation

## Obtain VEP

[Docker](https://www.docker.com/) must be installed.
On Ubuntu, run

```
apt-get install docker-ce
```

We used VEP in a Docker container following instructions from [VEP documentation](http://www.ensembl.org/info/docs/tools/vep/script/vep_download.html#docker)
```
$ docker pull ensemblorg/ensembl-vep
```

Create a folder for the cache data:

```
$ mkdir $HOME/vep_data
$ chmod a+rwx $HOME/vep_data
```

and run the install script for VEP, which then downloads the caches. 

```
$ docker run -t -i -v $HOME/vep_data:/opt/vep/.vep ensemblorg/ensembl-vep perl INSTALL.pl
```

Answer 'n' to `Do you want to continue installing the API (y/n)?`

Answer 'y' to `Do you want to install any cache files (y/n)?`

Answer '300' to selection of species and assembly (file `homo_sapiens_vep_96_GRCh38.tar.gz`)

Answer 'y' to `The VEP can use plugins to add functionality and data.
Plugins will be installed in /opt/vep/.vep/Plugins
Do you want to install any plugins (y/n)?`

Answer `1` (dbNSFP) to selection of plugins to install

## Obtaining data for dbNSFP

dbNSFP requires download and preprocessing of its data in addition to the plugin.

```
$ mkdir $HOME/dbnsfp_data
$ cd $Home/dbnsfp_data
$ wget ftp://dbnsfp:dbnsfp@dbnsfp.softgenetics.com/dbNSFPv3.5c.zip
```

Following instructions for GRCh38 in https://github.com/Ensembl/VEP_plugins/blob/release/96/dbNSFP.pm :

```
$ unzip dbNSFPv3.5c.zip 
$ head -n1 dbNSFP3.5c_variant.chr1 > h
$ cat dbNSFP3.5c_variant.chr* | grep -v ^#chr | sort -k1,1 -k2,2n - | cat h - | bgzip -c > dbNSFP3.5c.gz
$ tabix -s 1 -b 2 -e 2 dbNSFP3.5c.gz
```

For some reason, it was necessary to name the processed file dbNSFP3.5c.gz instead of dbNSFP.gz as described in the plugin comments.

## Obtaining ClinVar expert reviewed variants

On the ClinGen front page https://clinicalgenome.org/ there is (on 28th June 2019) a counter of `Expert Reviewed Variants in ClinVar` which links to https://www.ncbi.nlm.nih.gov/clinvar?term=(%22reviewed%20by%20expert%20panel%22%5BReview%20status%5D)%20OR%20%22practice%20guideline%22%5BReview%20status%5D

On that page, there is a `Download`-button which allows to download the variants in a tab-delimited file. However, this is not directly in a VCF format.

An alternative, easier way is to obtain the file from `ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/vcf_GRCh38/` . The most recent file on 28th June 2019 was named `clinvar_20190624.vcf.gz` . 

The same filtering criteria as when using the link on ClinGen's front page seems to be attainable by requiring each variant to have `CLNREVSTAT` value as either `reviewed_by_expert_panel` or `practice_guideline`. 

## Annotating the VCF with VEP and dbNSFP

Running VEP:

```
$ mkdir $HOME/amiss_data
$ chmod a+rwx $HOME/amiss_data
$ docker run -t -i -v $HOME/vep_data:/opt/vep/.vep -v $HOME/dbnsfp_data:/opt/vep/dbnsfp_data -v $HOME/amiss_data:/data ensemblorg/ensembl-vep
```

```
$ ./vep --cache --offline --format vcf --vcf --force_overwrite --dir_cache /opt/vep/.vep/ --dir_plugins /opt/vep/.vep/Plugins/ --input_file /data/clinvar_20190624.vcf.gz --output_file /data/clinvar_20190624.vep.vcf --plugin dbNSFP,/opt/vep/dbnsfp_data/dbNSFP3.5c.gz,ALL --canonical
```
