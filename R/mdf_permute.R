#' Permute the values in a gridded object
#'
#' The permutation of a set of cell values leads to a systematic (re)arrangement
#' of all the members of the set.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param type [character(1)][character]\cr the type of permutation that should
#'   be  applied. Either \code{"invert"}, \code{"revert"}, \code{"descending"},
#'   \code{"ascending"} or \code{"cycle"}.
#' @param by [integerish(1)][integer]\cr value by which to apply the
#'   permutation; recently only for \code{type = "cycle"}.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimensions as \code{obj}, in which all values
#'   have been permuted according to the chosen \code{type}.
#' @family operators to modify cell values
#' @examples
#' input <- gtGeoms$grid$continuous
#' patches <- mdf_binarise(obj = input, thresh = 30) %>%
#'   mdf_patches()
#'
#' # invert background/foreground
#' mdf_permute(obj = input) %>%
#'   visualise(input, .)
#'
#' # sort patch values ascending
#' mdf_permute(obj = patches, type = "ascending") %>%
#'   visualise(patches, .)
#'
#' # cycle values backwards by 30
#' mdf_permute(obj = input, type = "cycle", by = -30) %>%
#'   visualise(input, .)
#' @importFrom checkmate assertChoice assertIntegerish testCharacter testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#'   subNumNumCpp
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @export

mdf_permute <- function(obj, type = "invert", by = NULL, label = NULL){

  # check arguments ----
  assertChoice(x = type, choices = c("invert", "revert", "descending", "ascending", "cycle"))
  if(type == "cycle"){
    assertIntegerish(x = by)
  }

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "permuted"
  }

  # manage card ----
  if(is.null(obj)){

    obj <- new(Class = "card",
               operators = list(),
               modify = list(),
               metrics = list(),
               measure = list())
  }

  if(testClass(x = obj, classes = "card")){

    temp <- list(fun = "mdf_permute",
                 type = type,
                 by = by)

    obj@operators <- c(obj@operators, set_names(x = list(temp), nm = label))

    return(obj)
  }

  # pull data ----
  theExtent <- getExtent(x = obj)
  theFeatures <- getFeatures(x = obj)
  if(is.null(theFeatures)){
    stop("'obj' doesn't seem to contain any valid features.")
  }
  theRes <- getRes(x = obj)
  dims <- round(c((theExtent$x[2] - theExtent$x[1])/theRes[1],
                  (theExtent$y[2] - theExtent$y[1])/theRes[2]), 0)

  # make temporary objects ----
  mat <- matrix(data = theFeatures[[2]],
                ncol = dims[1],
                nrow = dims[2])
  uVals <- unique(theFeatures[[2]])

  # body ----
  if(type == "invert"){
    newVals <- max(uVals, na.rm = TRUE) - uVals
    action <- "inverted"
  } else if(type == "revert"){
    newVals <- rev(uVals)
    action <- "reverted"
  } else if(type == "descending"){
    newVals <- sort(uVals, decreasing = TRUE)
    action <- "descending sorting"
  } else if(type == "ascending"){
    newVals <- sort(uVals, decreasing = FALSE)
    action <- "ascending sorting"
  } else if(type == "cycle"){
    newVals <- uVals + by
    newVals[newVals > max(uVals, na.rm = TRUE)] <- newVals[newVals > max(uVals, na.rm = TRUE)] - max(uVals, na.rm = TRUE)
    newVals[newVals < min(uVals, na.rm = TRUE)] <- newVals[newVals < min(uVals, na.rm = TRUE)] + max(uVals, na.rm = TRUE)
    action <- paste0("cycled by ", by)
  }
  temp <- subNumNumCpp(mat = mat, replace = uVals, with = newVals)

  # build output ----
  history <- paste0("values have been ", action)
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = sort(unique(theFeatures$gid)))

  out <- new(Class = "geom",
             type = "grid",
             name = paste0(label, " [", action, "]"),
             point = rbind(theExtent[1,], dims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = obj),
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), list(history)))

  return(out)

}
