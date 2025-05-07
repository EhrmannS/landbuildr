#' Combine a stack of gridded objects
#'
#' Successively combine the layers of a raster stack with each other (similar to
#' \code{\link{Reduce}}).
#' @param obj [\code{any gridded(1)}]\cr the gridded object to modify.
#' @param by [\code{list(.)}]\cr the cell values by which the layers shall be
#'   aggregated; see Details.
#' @param fun [\code{function(1)}]\cr find the resulting value of the combined
#'   layers.
#' @param weights [\code{numeric(length(obj))}]\cr weight by which the values in
#'   each layer are multiplied before applying \code{fun}.
#' @param direction [\code{character(1)}]\cr the direction into which the
#'   \code{RasterLayer} in \code{obj} is supposed to be combine. Either
#'   \code{"right"} (default) or \code{"left"}; see Details.
#' @param label [character(1)][character]\cr the label by which this function
#'   shall be callable.
#' @details The argument \code{direction} takes the direction into which the
#'   layers should be combined. \code{"right"} means that layers are combined
#'   from left to right. \code{rReduce} is based on the functional
#'   \code{\link{Reduce}}, where this wording is handled differently.
#'
#'   The number of layers in the aggregated raster depends on the length of the
#'   list in \code{by}. If by is left empty, everything is written into one
#'   \code{RasterLayer} object. Values to be aggregated in a \code{RasterLayer}
#'   are in the same list element.
#' @return a gid geom of the same dimensions as \code{obj}, in which the layers
#'   of \code{obj} are aggregated into a smaller set of layers.
#' @family operators to modify the overall object
#' @examples
#' patches <- mdf_patches(mdf_binarise(gtGeoms$grid$continuous, thresh = 30))
#' myPatches <- mdf_segregate(patches)
#'
#' # revert the segregation
#' visualise(mdf_reduce(myPatches))
#'
#' # group patches
#' twoGroups <- list(c(1:14), c(15:28))
#' visualise(mdf_reduce(myPatches, by = twoGroups))
#'
#' # select a subset of patches
#' someLayers <- list(c(1, 3, 5, 7, 9))
#' visualise(mdf_reduce(myPatches, by = someLayers))
#' @importFrom checkmate assertClass assertList assertFunction assertIntegerish
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom stats setNames
#' @importFrom tibble tibble
#' @export

mdf_reduce <- function(obj = NULL, by = NULL, fun = function(x) sum(x, na.rm = TRUE),
                       weights = NULL, direction = "right", label = NULL){

  # check arguments ----

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "reduce"
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

    temp <- list(fun = "mdf_reduce",
                 by = by,
                 fun = fun,
                 weights = weights,
                 direction = direction)

    obj@operators <- c(obj@operators, set_names(x = list(temp), nm = label))

    return(obj)
  }

  # manage card ----
  if(is.null(obj) | testClass(x = obj, classes = "card")){
    out <- punch(card = obj,
                 operator = list(rReduce = list(by = by,
                                                fun = fun,
                                                weights = weights,
                                                direction = direction)))
    return(out)
  }

  # pull data ----
  theExtent <- getExtent(x = obj)
  theFeatures <- getFeatures(x = obj)
  theRes <- getRes(x = obj)
  dims <- round(c((theExtent$x[2] - theExtent$x[1])/theRes$x,
                  (theExtent$y[2] - theExtent$y[1])/theRes$y), 0)

  # make temporary objects ----
  mat <- matrix(data = theFeatures$values,
                ncol = dims[1],
                nrow = dims[2])
  uVals <- unique(theFeatures$values)

  # body ----
  temp <-

    # build output ----
  history <- paste0("")
  theName <- ""

  theFeatures <- setNames(list(tibble(values = as.vector(temp))), theName)
  theGroups <- setNames(list(tibble(gid = integer())), theName)

  out <- new(Class = "geom",
             type = "grid",
             name = label,
             point = rbind(theExtent[1,], dims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = obj),
             scale = "absolute",
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), list(history)))
  #
  # # check arguments
  # assertClass(obj, "RasterStackBrick")
  # assertList(by, types = "integerish", min.len = 1, null.ok = TRUE)
  # assertFunction(fun)
  # assertIntegerish(weights, null.ok = TRUE)
  # direction <- match.arg(direction, c("right", "left"))
  #
  # if(is.null(weights)){
  #   weights <- rep_len(1, dim(obj)[3])
  # } else{
  #   if(length(weights) != dim(obj)[3]){
  #     weights <- rep_len(weights, dim(obj)[3])
  #   }
  # }
  #
  # if(direction == "right"){
  #   right <- FALSE
  # } else{
  #   right <- TRUE
  # }
  #
  # if(!is.null(by)){
  #   out <- brick()
  #   for(i in seq_along(by)){
  #
  #     tempBy <- by[i]
  #     tempName <- names(tempBy)
  #     tempObj <- obj[[tempBy[[1]]]]
  #     tempColTab <- tempObj[[1]]@legend@colortable
  #     tempWeights <- weights[by[[i]]]
  #     theRasters <- lapply(X = 1:dim(tempObj)[3], function(x){
  #       tempObj[[x]] * tempWeights[x]
  #     })
  #     tempOut <- Reduce(f = function(x, y) overlay(x, y, fun = fun),
  #                       x = theRasters,
  #                       right = right)
  #     colortable(tempOut) <- tempColTab
  #     names(tempOut) <- tempName
  #     out <- addLayer(out, tempOut)
  #   }
  #   action <- paste0(length(by), " new layers")
  #
  # } else{
  #
  #   theRasters <- lapply(X = 1:dim(obj)[3], function(x){
  #     obj[[x]] * weights[x]
  #   })
  #   out <- Reduce(f = function(x, y) overlay(x, y, fun = fun),
  #                 x = theRasters,
  #                 right = right)
  #
  #   # I keep this here because at some point I might implement '#include <boost/...>' in reduceMatrixC
  #   # theMatrixes <- lapply(1:dim(obj)[3], function(x) as.matrix(obj[[x]]))
  #   # out <- reduceMatrixC(theMatrixes, f = fun)
  #
  #   action <- paste0("1 new layer")
  #   names(out) <- "reduced"
  # }
  # extent(out) <- extent(obj)
  # crs(out) <- crs(obj)
  #
  # # assign history
  # # if(length(obj@history)==0){
  # #   history <- list(paste0("the object was loaded from memory"))
  # # } else{
  # #   history <- obj@history
  # # }
  # # out@history <- c(history, list(paste0("layers have been reduced into ", action)))
  # out <- setHistory(x = out, history = paste0("layers have been reduced into ", action))
  #
  return(out)
}
