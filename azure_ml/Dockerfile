# FROM mcr.microsoft.com/azureml/base-gpu:openmpi3.1.2-cuda10.1-cudnn7-ubuntu18.04
FROM mcr.microsoft.com/azureml/openmpi4.1.0-ubuntu20.04
# FROM mcr.microsoft.com/azureml/openmpi3.1.2-ubuntu16.04

RUN conda install -c r -y pip=20.1.1 conda=4.8.3 r-essentials openssl=1.1.1c && \
	conda clean -ay && \
	pip install --no-cache-dir azureml-defaults pyspark ruamel.yaml

ENV TAR="/bin/tar"
RUN R -e "options(repos='https://mran.microsoft.com/snapshot/2020-09-24');install.packages(c('remotes', 'reticulate', 'optparse', 'azuremlsdk', 'BiocManager', 'abind', 'ROCR', 'ggplot2', 'DMwR2'))"

## Install Dependencies
RUN wget https://cran.r-project.org/src/contrib/Archive/DMwR/DMwR_0.4.1.tar.gz
RUN R -e "options(repos='https://mran.microsoft.com/snapshot/2020-09-24');install.packages('DMwR_0.4.1.tar.gz', type = 'source')"

### pcamethods
RUN R -e "BiocManager::install('pcaMethods')"

## Install `amiss` from Github branch
ARG AMISS_BRANCH
RUN R -e "options(repos='https://mran.microsoft.com/snapshot/2019-07-19'); remotes::install_github('blueprint-genetics/amiss', ref = '"$AMISS_BRANCH"', dependencies = TRUE)"
#RUN R -e "remotes::install_github('blueprint-genetics/amiss', ref = '"$AMISS_BRANCH"', dependencies = TRUE)"

