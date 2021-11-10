#' Predict using every classifier model in a model tree on every dataset in an imputed dataset tree
#'
#' @param models Classifier model tree
#' @param completions Imputed dataset tree
#' @param positive_label String representing the positive label to choose correct column from predict output
#' @param seed Seed value to set for loop
#'
#' @return Tree of vectors of predicted probabilities
#'
#' @importFrom magrittr %>%
#' @importFrom foreach %do%
#' @importFrom doRNG %dorng%

prediction <- function(models, completions, positive_label = POSITIVE_LABEL, seed = 1) {
  
  predictions <- lapply(names(models), function(method) {
  
    pred_per_model <- lapply(models[[method]], function(model) {

      if (is.null(completions[[method]]) || length(completions[[method]]) < 1) {
        return(list(NA))
      }
      pred_per_completion <- foreach::foreach(completed_dataset = completions[[method]], .options.RNG = seed) %dorng% {
        if (!is.null(completed_dataset)) {
          tryCatch({
            tr_cats <- model$trainingData %>% sapply(levels) %>% Filter(f = Negate(is.null))
            te_cats <- completed_dataset %>% sapply(levels) %>% Filter(f = Negate(is.null))
            tr_cats <- tr_cats[names(tr_cats) != ".outcome"]
            if (length(tr_cats) > 1) {
              for (cat in names(tr_cats)) {
                tr_levels <- tr_cats[[cat]]
                te_levels <- te_cats[[cat]]
                if (length(tr_levels) > length(te_levels)) {
                  levels(completed_dataset[, cat]) <- c(te_levels, setdiff(tr_levels, te_levels))
                } else if (length(tr_levels) < length(te_levels)) {
                  completed_dataset[completed_dataset[,cat] %in% setdiff(te_levels, tr_levels), cat] <- "MISSING"
                  completed_dataset[, cat] <- factor(completed_dataset[, cat])
                }
              }
            }
            flog.pid.info("Predicting using best model for %s", method)
            te_time <- system.time(
            predictions <- predict(model, completed_dataset, type = "prob")[,positive_label, drop = TRUE]
            )
            attr(predictions, "tr_time") <- model$times$final["elapsed"]
            attr(predictions, "te_time") <- te_time["elapsed"]
            return(predictions)
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

