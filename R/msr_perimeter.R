#' Perimeter length
#'
#' Calculate the length of boundaries between objects in a gridded object.
#' @param obj [gridded(1)][geom]\cr The object to measure.
#' @param scale [character(1)][character]\cr scale at which the perimeter of objects
#'   should be calculated; possible values are \code{"patch"} and
#'   \code{"class"}.
#' @param unit [character(1)][character]\cr the unit the output should have. With
#'   \code{"map"} the result will be in the respective map unit and with
#'   \code{"cells"} (default) it will be the multiple of the cell
#'   dimension.
#' @param label [character(1)][character]\cr the label by which this function
#'   shall be callable in a derived metric.
#' @return For \code{scale = "class"} the edge length of each unique value
#'   (class) in the gridded object, for \code{scale = "patch"} the edge length of
#'   distinct objects per distinct values (i.e. the area of patches per class).
#' @family generic landscape metrics
#' @examples
#' cat <- gtGeoms$grid$categorical
#' bin <- mdf_binarise(gtGeoms$grid$continuous, thresh = 40)
#'
#' # the perimeter ...
#' # ... per landcover type
#' msr_perimeter(obj = cat, scale = "class")
#'
#' # ... of patches per landcover typ
#' msr_perimeter(obj = cat, scale = "patch")
#'
#' # ...  of certain values; from a binary raster, patches are
#' # automatically determined
#' mdf_binarise(obj = cat, match = c(41, 44, 47)) %>%
#'   msr_perimeter(scale = "patch")
#'
#' msr_perimeter(obj = bin, scale = "class")
#'
#' @importFrom checkmate assertChoice testClass assertCharacter
#' @importFrom geomio getNames getExtent getFeatures getRes countCellEdgesCpp
#' @importFrom tibble tibble
#' @importFrom rlang set_names
#' @importFrom mmand binary components shapeKernel
#' @export

msr_perimeter <- function(obj = NULL, scale = "patch", unit = "cells",
                          label = NULL){

  assertChoice(x = scale, choices = c("patch", "class"))
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

    temp <- list(fun = "msr_perimeter",
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

  # test which object we are dealing with
  if(scale == "class"){

    temp <- mat

    # count...
    values <- countCellEdgesCpp(mat = temp)
    groups <- tibble(class = values$value)

  } else{

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
      temp_cc[is.na(temp_cc)] <- 0

      # count...
      temp_vals <- count.edges(mat = temp_cc)
      temp_vals <- temp_vals[temp_vals$value != 0,]
      values <- rbind(values, temp_vals)
      groups <- c(groups, rep(vals[i], length(temp_vals$value)))
    }
    groups <- cbind(class = groups, patch = values$value)

  }

  # put together the output ----
  if(unit == "map"){
    edges <- values$edgesX * theRes[1] + values$edgesY * theRes[2]
    result <- tibble(groups, edgelength = edges)
  } else{
    edges <- values$edgesX + values$edgesY
    result <- tibble(groups, edges)
  }

  # manage the bibliography entry ----
  bib <- "mmand"
  options(bibliography = unique(c(getOption("bibliography"), bib)))

  return(result)
}
