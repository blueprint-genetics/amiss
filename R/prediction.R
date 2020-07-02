library(foreach)
library(doParallel)
library(doRNG)
library(here)

source(here("R", "imputation.R"))

prediction <- function(models, completions, positive_label = POSITIVE_LABEL, seed = 1) {
  
  predictions <- lapply(names(models), function(method) {
  
    pred_per_model <- lapply(models[[method]], function(model) {

      if (is.null(completions[[method]]) || length(completions[[method]]) < 1) {
        return(list(NA))
      }
      pred_per_completion <- foreach(completed_dataset = completions[[method]], .options.RNG = seed) %dorng% {
        if (!is.null(completed_dataset)) {
          tryCatch({
            flog.pid.info("Predicting using best model for %s", method)
            predict(model, completed_dataset, type = "prob")[,positive_label, drop = TRUE]
          }, error = function(e) {
            flog.pid.debug(e)
            return(NA)
          })
        }
        else  {
          return(NA)
        }
      }

      names(pred_per_completion) <- paste0("imp_", seq_along(pred_per_completion))
      pred_per_completion

    })

    names(pred_per_model) <- paste0("model_", seq_along(pred_per_model))
    pred_per_model

  })
  names(predictions) <- names(models)
  return(predictions)
}

