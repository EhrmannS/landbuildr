#' Number of objects
#'
#' Count the number of objects in a gridded object.
#' @param obj [gridded(1)][geom]\cr The object to measure.
#' @param scale [character(1)][character]\cr scale at which the number of
#'   objects should be counted; possible values are \code{"patch"} and
#'   \code{"class"}.
#' @param label [character(1)][character]\cr the label by which this function
#'   shall be callable in a derived metric.
#' @return For \code{scale = "class"} the number of unique values (classes) in
#'   the gridded object. For \code{scale = "patch"} the number of objects per
#'   distinct value (i.e. the number of patches per class).
#' @family generic landscape metrics
#' @examples
#' cat <- gtGeoms$grid$categorical
#' bin <- mdf_binarise(gtGeoms$grid$continuous, thresh = 40)
#'
#' # the number ...
#' # ... per landcover type
#' msr_number(obj = cat, scale = "class")
#'
#' # ... of patches per landcover type
#' msr_number(obj = cat, scale = "patch")
#'
#' # ...  of certain values; from a binary raster, patches are
#' # automatically determined
#' mdf_binarise(obj = cat, match = c(41, 44, 47)) %>%
#'   msr_number(scale = "patch")
#'
#' msr_number(obj = bin, scale = "class")
#'
#' @importFrom checkmate assertChoice assertCharacter assertSubset
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @importFrom geomio getExtent getFeatures getRes
#' @importFrom mmand components shapeKernel binary
#' @export

msr_number <- function(obj = NULL, scale = "patch", label = NULL){

  assertChoice(x = scale, choices = c("patch", "class"))

  # manage card ----
  if(is.null(obj)){

    obj <- new(Class = "card",
               operators = list(),
               modify = list(),
               metrics = list(),
               measure = list())
  }

  if(testClass(x = obj, classes = "card")){
    assertCharacter(label, ignore.case = TRUE, any.missing = FALSE, len = 1)

    temp <- list(fun = "msr_number",
                 scale = scale)

    obj@metrics <- c(obj@metrics, set_names(x = list(temp), nm = label))

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
  uVals <- unique(theFeatures[[2]])

  # do calculation for the respective scale ----
  if(scale == "class"){

    vals <- unique(as.vector(mat[!is.na(mat)]))
    values <- tibble(landscape = 1, classes = length(vals))

  } else{

    # test which object we are dealing with
    if(binary(mat)){

      temp <- components(mat, shapeKernel(c(3, 3), type = "diamond"))
      vals <- unique(as.vector(temp[!is.na(temp)]))
      values <- tibble(landscape = 1, patches = length(vals))

    } else {

      vals <- sort(unique(as.vector(mat[!is.na(mat)])))

      patches <- NULL
      for(i in seq_along(vals)){
        temp_mat <- mat
        temp_mat[temp_mat != vals[i]] <- NA

        temp_cc <- components(temp_mat, shapeKernel(c(3, 3), type = "diamond"))
        temp_vals <- sort(unique(as.vector(temp_cc)))
        patches <- c(patches, length(temp_vals))
      }
      values <- tibble(class = vals, patches = patches)

    }

  }

  # manage the bibliography entry ----
  bib <- "mmand"
  options(bibliography = unique(c(getOption("bibliography"), bib)))

  return(values)
}
