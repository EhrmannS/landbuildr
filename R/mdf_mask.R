#' Select cells based on a mask
#'
#' Transform a gridded object by setting all cells that are not covered by a
#' mask to \code{background}.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param by [gridded(1)][geom]\cr binarised gridded object of the same
#'   dimension as \code{obj} where the cells that should be retained have the
#'   value 1 and all other cells the value 0.
#' @param background [integerish(1)][integer]\cr the value any cell with value
#'   NA should have.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimensions as \code{obj}, in which all cells
#'   with value 0 in the mask have been set to \code{background} and all other
#'   values are retained.
#' @details If used in an algorithm (i.e., when \code{obj} is not defined),
#'   \code{by} can also contain the name of a sub-algorithm to use the final
#'   output thereof as mask. Moreover, \code{by = "input"} would select the
#'   original gridded object as mask.
#' @family operators to select a subset of cells
#' @examples
#' input <- gtGeoms$grid$continuous
#'
#' # define an mask with the correct dimension
#' mask <- matrix(nrow = getRows(patches),
#'                ncol = getCols(patches),
#'                data = 0)
#' mask[c(5:25), c(5:50)] <- 10
#'
#' mdf_mask(obj = input, by = mask) %>%
#'   visualise(input, mask, .)
#' @importFrom checkmate assertIntegerish assert testCharacter testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom mmand binary
#' @importFrom tibble tibble
#' @export

mdf_mask <- function(obj = NULL, by = NULL, background = NA, label = NULL){

  # check arguments ----
  assertIntegerish(x = background)
  assert(all(getRes(x = obj) == getRes(x = by)))

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "masked"
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

    temp <- list(fun = "mdf_mask",
                 by = by,
                 background = background)

    obj@operators <- c(obj@operators, set_names(x = list(temp), nm = label))

    return(obj)
  }

  # pull data ----
  theExtent <- getExtent(x = obj)
  mExtent <- getExtent(x = by)
  theFeatures <- getFeatures(x = obj)
  if(is.null(theFeatures)){
    stop("'obj' doesn't seem to contain any valid features.")
  }
  mFeatures <- getFeatures(x = by)
  if(is.null(mFeatures)){
    stop("'by' doesn't seem to contain any valid features.")
  }
  theRes <- getRes(x = obj)
  dims <- round(c((theExtent$x[2] - theExtent$x[1])/theRes[1],
                  (theExtent$y[2] - theExtent$y[1])/theRes[2]), 0)
  mDims <-  round(c((mExtent$x[2] - mExtent$x[1])/theRes[1],
                    (mExtent$y[2] - mExtent$y[1])/theRes[2]), 0)

  # make temporary objects ----
  mat <- matrix(data = theFeatures[[2]],
                ncol = dims[1],
                nrow = dims[2])
  by <- matrix(data = mFeatures[[2]],
               ncol = mDims[1],
               nrow = mDims[2])

  if(!all(dim(mat) == dim(by))){
    stop("'by' does not have the same dimension as 'obj'.")
  }

  # body ----
  if(!binary(by)){
    stop("'mask' is not binary, please run 'mdf_binarise()' first.")
  }
  temp <- mat
  temp[by == 0] <- background

  history <- paste0("the raster has been masked")
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = sort(unique(theFeatures$gid)))

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
