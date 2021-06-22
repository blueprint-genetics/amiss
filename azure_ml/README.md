# Scaling the `amiss` R framework with Azure Machine Learning

<h3 align="right">Quest Diagnostics, Blueprint Genetics, and BlueGranite</h3>

----------------------------

Since the `amiss` framework performs a grid search across a number of parameters, this process can be distributed across multiple nodes in a compute cluster. Using Azure Machine Learning, a cloud-based service to help build, scale, monitor, and deploy machine learning models, we can distribute this search with ease.

To learn more about Azure Machine Learning, click [here](https://docs.microsoft.com/en-us/azure/machine-learning/overview-what-is-azure-ml).


<img src="https://raw.githubusercontent.com/colbyford/amiss/master/azure_ml/img/scalability_approach.png">


## How it works - HyperDrive

As part of the Azure Machine Learning Python and R SDKs, there is a hyperparameter tuning package called [HyperDrive](https://docs.microsoft.com/en-us/python/api/azureml-train-core/azureml.train.hyperdrive?view=azure-ml-py).

HyperDrive allows for more efficient hyperparameter tuning by distributing combinations of parameters to separate compute contexts on a cluster. This helps to find the set of parameters that result in the best performance.

<img src="https://raw.githubusercontent.com/colbyford/amiss/master/azure_ml/img/hyperdrive_example.png">

## Running `amiss` in Azure ML

1. Provision an Azure Machine Learning workspace in your Azure tenant using the following instructions: https://docs.microsoft.com/en-us/azure/machine-learning/quickstart-create-resources#create-the-workspace

2. Once the Azure ML workspace is ready, create a Compute Instance as described [here](https://docs.microsoft.com/en-us/azure/machine-learning/quickstart-create-resources#instance).

3. Once the Compute Instance is up and running, click on the RStudio link to open an RStudio Server instance in your browser.

4. Upload the [aml_r_sdk_hyperdrive.rmd](aml_r_sdk_hyperdrive.rmd) notebook and the [amiss_test_script.R](amiss_test_script.R) script into RStudio.

5. Upload your data into RStudio under the `/data` directory. (You may have to create this directory first.)

6. Change the values in the the `aml_r_sdk_hyperdrive.rmd` notebook to fit your desired computational workload.

## Building the Custom Docker Image

From this directory, run the following commands to build the custom Docker image. This docker image is built with the required Azure ML dependencies as well as the packages to run the `amiss` R package.

```sh
docker build -t amiss_aml .

docker run --name amiss_aml --rm -p 8787:8787 amiss_aml

docker image tag amiss_aml <YOUR DOCKERHUB USERNAME>/amiss_aml:latest
docker push <YOUR DOCKERHUB USERNAME>/amiss_aml:latest

# docker image tag amiss_aml cford38/amiss_aml:latest
# docker push cford38/amiss_aml:latest
```

Notes:
 - This image is currently hosted under [`cford38/amiss_aml`](https://hub.docker.com/r/cford38/amiss_aml) if you don't want to rebuild it.
 - If you choose to rebuild the image for yourself, update the `env <- r_environment("amiss-env", custom_docker_image = "<YOUR DOCKERHUB USERNAME>/amiss_aml")` line in the `aml_r_sdk_hyperdrive.rmd` notebook.
 - Also, replace `<YOUR DOCKERHUB USERNAME>` with your username for [hub.docker.com](https://hub.docker.com/) below.
