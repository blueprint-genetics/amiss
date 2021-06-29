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
#' @param pass_node_names Whether to pass the list of names
#' accumulated from ancestor nodes to `fun`
#' @param node_names List of names so far visited in this branch -
#' *not intended to be set by user*
#'
#' @return A tree, containing all branches of `x` that lead to
#' leaves of type `x_class`, now with `fun` applied.
recursive_apply <- function(x, fun, x_class, pass_node_names = FALSE, node_names = list()) {

  if (x_class %in% class(x)) {
    if (pass_node_names)
      result <- fun(x, node_names)
    else
      result <- fun(x)
  }
  else {
    if ("list" %in% class(x)) {
      if (pass_node_names) {
        result <- lapply(seq_along(x), function(y) recursive_apply(x = x[[y]],
                                                                   fun = fun,
                                                                   x_class = x_class,
                                                                   pass_node_names = pass_node_names,
                                                                   node_names = c(node_names, names(x)[[y]])))
      } else {
        result <- lapply(x, function(y) recursive_apply(y, fun, x_class, pass_node_names = FALSE, node_names = list()))
      }
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

# Common special cases
recursive_apply_numeric <- function(x, fun, pass_node_names = FALSE)
  recursive_apply(x = x, fun = fun, x_class = "numeric", pass_node_names = pass_node_names)


# Analogous to `expand.grid` for a tree
get_tree_names <- function(tree, x_class) {
  tree_names <- recursive_apply(tree, function(x, name_list) return(name_list), x_class = x_class, pass_node_names = TRUE)
  tree_names %<>% leaf_apply(. %>% paste0(collapse = ":"), docall = FALSE)
  tree_names %<>% unlist(use.names = FALSE)
  return(tree_names)
}
get_tree_names_as_df <- function(tree, x_class) {
  tree_names <- get_tree_names(tree, x_class)
  tree_names %<>% strsplit(":")
  tree_names <- do.call(rbind, tree_names)
  tree_names %<>% data.frame(stringsAsFactors = FALSE)
  return(tree_names)
}
tree_as_df <- function(tree, x_class) {
  tree_df <- get_tree_names_as_df(tree, x_class)
  vals <- apply(tree_df, MARGIN = 1, FUN = function(...) {
    a <- tree
    for(n in list(...)) {
      a <- a[[n]]
    }
    return(a)
  })
  tree_df$values <- vals
  return(tree_df)
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
#' `[[1]]`
#' `[1] 10`
#' 
#' `[[2]]`
#' `[1] 15`
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
  else if (class(x) == "list") {
    lapply(x, function(y) leaf_apply(y, fun, docall))
  } else {
    warning("leaf_apply reached a node of non-list type. This should never happen. " %>% paste0(
            "Check if the original input was a list, or if there were nodes with both list- and non-list children."))
    return(NULL)
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
