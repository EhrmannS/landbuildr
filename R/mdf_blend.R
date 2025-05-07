#' Blend two gridded objects
#'
#' Transform a gridded object by combining it with another gridded object.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param overlay [gridded(1)][geom]\cr the gridded object that should be
#'   combined with the primary gridded object \code{obj}; has to have the same
#'   dimension as \code{obj}.
#' @param fun [character(1)][character]\cr function used for blending, recently
#'   supported are all \link[=^]{arithmetic operators}.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimension as \code{obj}, in which an overlay
#'   has been blended with the primary raster.
#' @details To aggregate more than two objects, see \code{\link{ls_reduce}}.
#' @family operators to modify the overall object
#' @examples
#' patches <- gtGeoms$grid$continuous %>%
#'   mdf_binarise(thresh = 30) %>%
#'   mdf_patches()
#'
#' # define an overlay with the correct dimension
#' mask <- matrix(nrow = getRows(patches),
#'                ncol = getCols(patches),
#'                data = 0)
#' mask[c(5:25), c(5:50)] <- 10
#'
#' # blend patches with the mask
#' mdf_blend(obj = patches, overlay = mask) %>%
#'   visualise(patches, mask, .)
#' @importFrom checkmate assert assertChoice testCharacter testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @export

mdf_blend <- function(obj = NULL, overlay, fun = "+", label = NULL){

  # check arguments ----
  assert(all(getRes(x = obj) == getRes(x = overlay)))
  assertChoice(x = fun, choices = c("+", "-", "*", "/", "%%", "%/%", "^", "**"))

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "blended"
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

    temp <- list(fun = "mdf_blend",
                 overlay = overlay,
                 fun = fun)

    obj@operators <- c(obj@operators, set_names(x = list(temp), nm = label))

    return(obj)
  }

  # pull data ----
  theExtent <- getExtent(x = obj)
  oExtent <- getExtent(x = overlay)
  theFeatures <- getFeatures(x = obj)
  oFeatures <- getFeatures(x = overlay)
  theRes <- getRes(x = obj)
  dims <- round(c((theExtent$x[2] - theExtent$x[1])/theRes[1],
                  (theExtent$y[2] - theExtent$y[1])/theRes[2]), 0)
  oDims <- round(c((oExtent$x[2] - oExtent$x[1])/theRes[1],
                   (oExtent$y[2] - oExtent$y[1])/theRes[2]), 0)

  # make temporary objects ----
  mat <- matrix(data = theFeatures[[2]],
                ncol = dims[2],
                nrow = dims[1])
  overlay <- matrix(data = oFeatures[[2]],
                    ncol = oDims[2],
                    nrow = oDims[1])

  # body ----
  # I keep this here because at some point I might implement
  # '#include <boost/...>' in reduceMatrixC
  # matList <- list(mat, overlay*weight)
  # temp <- reduceMatrixC(matList, f = sum)
  temp <- do.call(what = fun, args = list(mat, overlay))

  # build output ----
  history <- paste0("the object has been blended")
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = sort(unique(theFeatures$gid)))

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
