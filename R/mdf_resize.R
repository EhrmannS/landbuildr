#' Change the size of a gridded object
#'
#' Increase or decrease the size of a gridded object and takes values according to the nearest neighbour.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param factor [numeric(1)][numeric]\cr an integer larger than 1 (will be
#'   truncated if it's not an integer) for up-scaling and a fraction for
#'   down-scaling (see Details).
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom with a dimension that has been up or down-scaled from
#'   \code{obj}, where the new values have been determined by applying
#'   \code{kernelFunction}.
#' @details need to mention here that this is based on the nearest-neighbour
#'   interpolation, any other interpolation needs to be applied with
#'   mdf_interpolate
#'
#'   \code{factor} is a value by which the number of cells in x and y dimension
#'   will be multiplied. If \code{factor > 1}, the object will be up-scaled and
#'   if \code{factor < 1}, the raster will be down-scaled. Allowed values are
#'   integers or the inverse of an integer value (1/n) (denominator will be
#'   rounded to the next integer, such as 1/2 or 1/5).
#' @family operators to modify the overall object
#' @examples
#' input <- gtGeoms$grid$continuous
#' binarised <- mdf_binarise(obj = input, thresh = 30)
#'
#' # shrink the image size by a half
#' mdf_resize(obj = input, factor = 0.5) %>%
#'   visualise(input, .)
#'
#' increase the size (values are "the same", but based on 2^factor pixels)
#' mdf_resize(obj = input, factor = 2) %>%
#'   visualise(input, .)
#'
#' # up-scaling can be useful for follow-up on morphological operations
#' mdf_skeletonise(obj = binarised) %>%
#'   visualise(binarised, .)
#'
#' bin2 <- mdf_resize(obj = binarised, factor = 2)
#' mdf_skeletonise(obj = bin2) %>%
#'   visualise(rescaled, .)
#' @importFrom checkmate assertNumeric assertClass testCharacter testClass
#' @importFrom mmand boxKernel rescale
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom stats setNames
#' @importFrom tibble tibble
#' @export

mdf_resize <- function(obj = NULL, factor = NULL, label = NULL){

  # check arguments ----
  assertNumeric(x = factor, any.missing = FALSE, len = 1)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "resized"
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

    temp <- list(fun = "mdf_resize",
                 factor = factor)

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
                nrow = dims[2],
                byrow = TRUE)

  # body ----
  if(factor > 1){
    factor <- trunc(factor)
  } else{
    denominator <- round(1/factor)
    factor <- 1/denominator
  }
  temp <- rescale(x = mat, factor = factor, kernel = boxKernel())
  outDims <- round(dims * factor, 0)
  outWindow <- getWindow(x = obj) * factor

  # manage the bibliography entry ----
  bib <- "mmand"
  options(bibliography = unique(c(getOption("bibliography"), bib)))

  # build output ----
  history <- paste0("the object has been resized by the factor ", factor)

  rleVal <- rle(as.vector(t(temp)))
  theFeatures <- tibble(val = rleVal$values,
                        len = rleVal$lengths)
  theGroups <- tibble(gid = sort(unique(theFeatures$val)))

  out <- new(Class = "geom",
             type = "grid",
             name = paste0(label, " [", factor, "]"),
             point = rbind(theExtent[1,], outDims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = outWindow,
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), list(history)))

  return(out)
}
