#' Replace values in a gridded object
#'
#' Replace a set of values with a new value.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param old [integerish(.)][integer]\cr values to be substituted.
#' @param new [integerish(1)][integer]\cr value to substitute with.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimensions as \code{obj}, in which \code{old}
#'   values have been replaced by the \code{new} value.
#'
#' @details also talk about nearblack Function to identify similar pixel based
#'   on some threshold and convert them to the "anchor-values". Is a special
#'   case of mdf_replace, because a set of "similar pixels needs to be
#'   identified and they are converted to the anchor-values. See gdals nearblack
#'   function.
#'
#' @family operators to modify cell values
#' @examples
#' input <- gtGeoms$grid$categorical
#'
#' # replace different 40ies with 40
#' mdf_replace(obj = input, old = c(41, 44, 47), new = 40) %>%
#'   visualise(input, .)
#' @importFrom checkmate assertNumeric testCharacter testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#'   subNumNumCpp
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @export

mdf_replace <- function(obj, old = NULL, new = NULL, label = NULL){

  # check arguments ----
  assertNumeric(x = old, any.missing = FALSE, min.len = 1)
  assertNumeric(x = new, any.missing = FALSE, min.len = 1)
  if(length(old) != length(new)){
    newValues <- rep(new, length.out = length(old))
  } else{
    newValues <- new
  }

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "substituted"
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

    temp <- list(fun = "mdf_replace",
                 old = old,
                 new = new)

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
  # assertSubset(x = old, choices = uVals)

  # body ----
  if(all(is.na(old))){
    temp <- theFeatures[[2]]
    temp[is.na(temp)] <- new
  } else {
    temp <- subNumNumCpp(mat = mat, replace = old, with = newValues)
  }

  # build output ----
  history <- paste0("value '", old, "' replaced with '", new, "'.")
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = unique(theFeatures$gid))

  out <- new(Class = "geom",
             type = "grid",
             name = label,
             point = rbind(theExtent[1,], dims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = obj),
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), list(history)))

  return(out)
}
