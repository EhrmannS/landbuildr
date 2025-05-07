#' Select cells based on a value range
#'
#' Transform a gridded object by setting all cells that are not comprised within
#' a lower and upper threshold to \code{background}.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param lower [numeric(1)][numeric]\cr minimum value above which the values will
#'   be selected.
#' @param upper [numeric(1)][numeric]\cr maximum value below which the values will
#'   be selected.
#' @param background [integerish(1)][integer]\cr the value any cell with value
#'   NA should have.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimension as \code{obj}, where the cells with
#'   values between \code{lower} and \code{upper} retain their value and all
#'   other cells are set to \code{background}.
#' @family operators to select a subset of cells
#' @examples
#' input <- gtGeoms$grid$continuous
#'
#' # get values between 40 and 80
#' mdf_range(obj = input, lower = 40, upper = 80) %>%
#'   visualise(input, .)
#' @importFrom checkmate assertNumeric assertIntegerish testCharacter testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @export

mdf_range <- function(obj = NULL, lower = NULL, upper = NULL, background = NA,
                      label = NULL){

  # check arguments ----
  assertNumeric(lower, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertNumeric(upper, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = background)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "range_selected"
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

    temp <- list(fun = "mdf_range",
                 lower = lower,
                 upper = upper,
                 background = background)

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
  temp <- mat
  if(!is.null(lower)){
    if(!min(uVals) < lower | !lower < max(uVals)){
      stop("please provide values for 'lower' within the range of the values of 'obj'.")
    }
    temp <- morphC(mat = temp, kernel = matrix(lower, 1, 1), value = uVals, blend = 4,
                   merge = 12, rotateKernel = FALSE, strictKernel = FALSE)
  } else {
    lower <- min(uVals)
  }
  if(!is.null(upper)){
    if(!min(uVals) < upper | !upper < max(uVals)){
      stop("please provide values for 'upper' within the range of the values of 'obj'.")
    }
    temp <- morphC(mat = temp, kernel = matrix(upper, 1, 1), value = uVals, blend = 3,
                   merge = 12, rotateKernel = FALSE, strictKernel = FALSE)
  } else {
    upper <- max(uVals)
  }
  temp[is.na(temp)] <- background

  history <- paste0("values greater than ", lower, " and less than ", upper, " have been selected")
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = unique(theFeatures$gid))

  out <- new(Class = "geom",
             type = "grid",
             name = paste0(label, " [", lower, "-", upper, "]"),
             point = rbind(theExtent[1,], dims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = obj),
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), list(history)))

  return(out)
}
