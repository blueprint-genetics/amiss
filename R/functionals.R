#' Reimplementation of rapply that does not consider
#' data.frames to be lists and visit them.
#'
#' The input tree should be constructed using only simple
#' lists (i.e. `class` should be *exactly* `"list"`, not
#' e.g. `c("list", "data.frame")`).
#'
#' The function does not keep branches that lead to branches
#' of the wrong class.
#'
#' @param x List structure to recursively apply over.
#' @param fun Function to apply on each leaf of class `x_class`.
#' @param x_class Class of leaf that should be applied on.
#'
#' @return A tree, containing all branches of `x` that lead to leaves of type `x_class`, now with `fun` applied.
recursive_apply <- function(x, fun, x_class) {

  if (x_class %in% class(x)) {
    result <- fun(x)
  }
  else {
    if ("list" %in% class(x)) {
      result <- lapply(x, function(y) recursive_apply(y, fun, x_class))
      names(result) <- names(x)

      non_null_elements <- sapply(result, Negate(is.null))

      if (length(non_null_elements) > 0)
        result <- result[non_null_elements]
      else result <- NULL

    }
    else {
      result <- NULL
    }
  }

  return(result)
}

#' A recursive application of a function to lists of leaves
#' 
#' This function applies `fun` to the (unique) sibling lists of each leaf.
#' 
#' The function assumes that the input tree's nodes only have internal
#' nodes as children, or have only leaves as children.
#'
#' E.g.
#' Correct:
#' `list(list(1:5, 6:10), list(11:15))`
#' Incorrect:
#' `list(list(list(1:5), 6:10), list(11:15))`
#'
#' E.g.
#' `> a <- list(list(1:5, 6:10), list(11:15))`
#' `> leaf_apply(a, max, TRUE)`
#' `[[1]]
#' [1] 10
#' 
#' [[2]]
#' [1] 15
#'
#' @param x List structure over whose leaves to apply.
#' @param fun Function to apply.
#' @param docall Whether to apply `fun` directly or using `do.call`.
#'
#' @return A (list) tree with the lowest ancestors of leaves replaced by the result of `fun` over the leaves.
leaf_apply <- function(x, fun, docall) {
  
  if (length(x) < 1) return (NULL)
  
  if (class(x) == "list" && all(sapply(x, class) != "list")) {
    if (docall) {
      do.call(fun, x)
    }
    else {
      fun(x)
    }
  }
  else {
    lapply(x, function(y) leaf_apply(y, fun, docall))
  }
}

pair_up <- function(name, value) {
  list(name = name,  value = value)
}

#' Reimplementation of `enumerate` from Python
#'
#' @param x A named list-like
#'
#' @return A list of (name, value) pairs represented as lists.
enumerate <- function(x) {
  mapply(FUN = pair_up, names(x), x, SIMPLIFY = FALSE)
}