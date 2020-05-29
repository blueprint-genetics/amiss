FROM rocker/r-ver:3.6.2
RUN apt-get update && \
	apt-get install -y zlib1g-dev && \
	apt-get install -y libcurl4-openssl-dev && \
	rm -rf /var/lib/apt/lists/*

COPY ./R/install_packages.R /amiss/R/install_packages.R
RUN Rscript /amiss/R/install_packages.R
COPY . /amiss
RUN chmod +x /amiss/run_tests.sh
RUN cd /amiss && /amiss/run_tests.sh

CMD ["bash", "/amiss/run.sh", "/amiss"]

