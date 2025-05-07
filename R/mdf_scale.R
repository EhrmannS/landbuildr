#' Change the scale of the values of a gridded object
#'
#' Change minimum and maximum value in a gridded object and scale all values to
#' that range.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param range [integerish(2)][integer]\cr vector of minimum and maximum value
#'   to which the values shall be scaled.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimension as \code{obj}, where the cell
#'   values have been rescaled.
#' @family operators to modify cell values
#' @examples
#' input <- gtGeoms$grid$continuous
#'
#' # scale values between 0 and 25
#' mdf_scale(obj = input, range = c(0, 25)) %>%
#'   visualise(input, .)
#' @importFrom checkmate assertIntegerish testCharacter testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @export

mdf_scale <- function(obj, range = NULL, label = NULL){

  # check arguments ----
  assertIntegerish(x = range, len = 2)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "values scaled"
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

    temp <- list(fun = "mdf_rescale",
                 range = range)

    obj@operators <- c(obj@operators, set_names(x = list(temp), nm = label))

    return(obj)
  }

  # pull data ----
  theExtent <- getExtent(x = obj)
  theFeatures <- getFeatures(x = obj)
  theRes <- getRes(x = obj)
  dims <- round(c((theExtent$x[2] - theExtent$x[1])/theRes[1],
                  (theExtent$y[2] - theExtent$y[1])/theRes[2]), 0)

  # make temporary objects ----
  mat <- matrix(data = theFeatures[[2]],
                ncol = dims[1],
                nrow = dims[2])
  uVals <- unique(theFeatures[[2]])

  # body ----
  temp <- (mat - min(mat, na.rm = TRUE)) * (range[2] - range[1]) / (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) + range[1]

  # build output ----
  history <- paste0("the values have been scaled between ", range[1], " and ", range[2])
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = unique(theFeatures$gid))

  out <- new(Class = "geom",
             type = "grid",
             name = paste0(label, " [", range[1], "-", range[2], "]"),
             point = rbind(theExtent[1,], dims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = obj),
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), list(history)))

  return(out)
}
