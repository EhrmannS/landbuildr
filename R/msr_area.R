#' Area of objects
#'
#' Calculate the area of objects in a gridded object.
#' @param obj [gridded(1)][geom]\cr The object to measure.
#' @param scale [character(1)][character]\cr scale at which the area of objects
#'   should be calculated; possible values are \code{"patch"}, \code{"class"}
#'   and \code{"landscape"}.
#' @param unit [character(1)][character]\cr the unit the output should have.
#'   With \code{"map"} the result will be in the respective map unit and with
#'   \code{"cells"} (default) it will be the number of raster cells.
#' @param label [character(1)][character]\cr the label by which this function
#'   shall be callable in a derived metric.
#' @return For \code{scale = "landscape"} the area of the overall gridded
#'   object, for \code{scale = "class"} the total area of each unique value
#'   (class), for \code{scale = "patch"} the area of distinct objects per
#'   distinct values (i.e. the area of patches per class).
#' @family generic landscape metrics
#' @examples
#' cat <- gtGeoms$grid$categorical
#' bin <- mdf_binarise(gtGeoms$grid$continuous, thresh = 40)
#'
#' # the area ...
#' # ... per landcover type
#' msr_area(obj = cat, scale = "class")
#'
#' # ... of patches per landcover type
#' msr_area(obj = cat, scale = "patch")
#'
#' # ...  of certain values; from a binary raster, patches are
#' # automatically determined
#' mdf_binarise(obj = cat, match = c(41, 44, 47)) %>%
#'   msr_area(scale = "patch")
#'
#' msr_area(obj = bin, scale = "class")
#'
#' @importFrom checkmate assertChoice assertSubset assertCharacter
#' @importFrom geomio getExtent getFeatures getRes countCellValuesCpp
#' @importFrom tibble tibble
#' @importFrom mmand binary components shapeKernel
#' @export

msr_area <- function(obj = NULL, scale = "patch", unit = "cells", label = NULL){

  assertChoice(x = scale, choices = c("patch", "class", "landscape"))
  assertChoice(x = unit, choices = c("cells", "map"))

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

    temp <- list(fun = "msr_area",
                 scale = scale,
                 unit = unit)

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

  # do calculation for the respective scale
  if(scale == "landscape"){

    groups <- tibble(landscape = 1)
    values <- tibble(cells = length(mat[!is.na(mat)]))

  } else if(scale == "class"){

    temp <- mat

    # count...
    values <- countCellValuesCpp(mat = temp)
    groups <- tibble(class = values$value)

  } else {

    # test which object we are dealing with and adapt temp accordingly
    if(binary(mat)){
      temp <- components(mat, shapeKernel(c(3, 3), type = "diamond"))
    } else{
      temp <- mat
    }
    vals <- sort(unique(as.vector(temp[!is.na(temp)])))

    values <- NULL
    groups <- NULL
    for(i in seq_along(vals)){
      temp_mat <- mat
      temp_mat[temp_mat != vals[i]] <- NA
      temp_cc <- components(temp_mat, shapeKernel(c(3, 3), type = "diamond"))

      # count...
      temp_vals <- countCellValuesCpp(mat = temp_cc)
      temp_vals <- temp_vals[!is.na(temp_vals$value),]
      values <- rbind(values, temp_vals)
      groups <- c(groups, rep(vals[i], length(temp_vals$value)))
    }
    groups <- cbind(class = groups, patch = values$value)

  }

  # manage the bibliography entry ----
  bib <- "mmand"
  options(bibliography = unique(c(getOption("bibliography"), bib)))

  # put together the output ----
  if(unit == "map"){
    cells <- values$cells * theRes[1]*theRes[2]
    result <- tibble(groups, area = cells)
  } else{
    cells <- values$cells
    result <- tibble(groups, cells)
  }

  return(result)
}
