#' Offset the values in a gridded object
#'
#' Apply arithmetic operators to the values in a gridded object to offset the
#' values.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param fun [character(1)][character]\cr function used for offsetting, recently
#'   supported are all \link[=^]{arithmetic operators}.
#' @param value [integerish(1)][integer]\cr the value by which to offset all
#'   values in the raster (can also be negative).
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimension as \code{obj}, in which all
#'   values have been offset by \code{value}.
#' @family operators to modify cell values
#' @examples
#' binarised <- mdf_binarise(obj = gtGeoms$grid$continuous, thresh = 30)
#'
#' # offset by multiplying values by 2
#' mdf_offset(obj = binarised, fun = "*", value = 2) %>%
#'   visualise(binarised, .)
#' @importFrom checkmate assertChoice assertIntegerish testCharacter testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @export

mdf_offset <- function(obj, fun = "+", value = 1, label = NULL){

  # check arguments ----
  assertIntegerish(x = value, any.missing = FALSE, len = 1)
  assertChoice(x = fun, choices = c("+", "-", "*", "/", "%%", "%/%", "^", "**"))

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "offset"
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

    temp <- list(fun = "mdf_offset",
                 fun = fun,
                 value = value)

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
                ncol = dims[2],
                nrow = dims[1])

  # body ----
  temp <- do.call(what = fun, args = list(mat, value))
  # temp <- t(apply(mat, 1:2, function (x) eval(parse(text = paste(x, fun, value)))))

  # build output ----
  history <- paste0("the object values have been offset by ", value, ".")
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = unique(theFeatures$gid))

  out <- new(Class = "geom",
             type = "grid",
             name = paste0(label, " [", fun, "]"),
             point = rbind(theExtent[1,], dims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = obj),
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), list(history)))

  return(out)
}
