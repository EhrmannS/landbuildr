#' Segregate values of a gridded object into layers
#'
#' Distinct values in a gridded object will be assigned to layers in a raster
#' stack.
#' @param obj [gridded(1)][geom]\cr any gridded object (with patches) to modify.
#' @param by [gridded(1)][geom]\cr optional object by which \code{obj} should be
#'   segregated. If left empty, the distinct values of \code{obj} will be taken.
#' @param flatten [logical(1)][logical]\cr whether all values should be set 1 or
#'   the original value should be retained.
#' @param background [integerish(1)][integer]\cr the value any cell with value
#'   NA should have.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return a \code{RasterStack} of the same dimensions as \code{obj}, in which
#'   the elements specified in \code{by} or the distinct values of \code{obj}
#'   have each been assigned to a separate layer.
#' @family operators to modify the overall object
#' @examples
#' patches <- gtGeoms$grid$continuous %>%
#'   mdf_binarise(thresh = 30) %>%
#'   mdf_patches(background = 0)
#'
#' segPatches <- mdf_layerise(obj = patches)
#'
#' visualise(patches, segPatches[[2]], segPatches[[3]], segPatches[[13]])
#'
#' # when flattening, all values are set to 1
#' segPatches2 <- mdf_layerise(patches, flatten = TRUE)
#' visualise(patches, segPatches2[[2]], segPatches2[[3]], segPatches2[[13]])
#'
#' # cut out by 'patches'
#' patchValues <- mdf_layerise(obj = gtGeoms$grid$continuous, by = patches)
#' visualise(patches, patchValues[[2]], patchValues[[3]], patchValues[[13]])
#' @importFrom checkmate assertLogical assertIntegerish testClass testCharacter
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @export

mdf_layerise <- function(obj = NULL, by = NULL, flatten = FALSE, background = NA,
                          label = NULL){

  # check arguments ----
  assertLogical(x = flatten, len = 1)
  assertIntegerish(x = background)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "segregated"
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

    temp <- list(fun = "mdf_layerise",
                 by = by,
                 flatten = flatten,
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
                  (theExtent$y[2] - theExtent$y[1])/theRes[1]), 0)

  # make temporary objects ----
  mat <- matrix(data = theFeatures[[2]],
                ncol = dims[2],
                nrow = dims[1])

  # if 'by' is given, create subMat (for subsetting into groups) from that
  if(!is.null(by)){
    bExtent <- getExtent(x = by)
    bFeatures <- getFeatures(x = by)
    bRes <- getRes(x = by)
    bDims <- c((bExtent$x[2] - bExtent$x[1])/bRes[1], (bExtent$y[2] - bExtent$y[1])/bRes[2])
    subMat <- matrix(data = bFeatures[[2]],
                     ncol = bDims[2],
                     nrow = bDims[1])
    uVals <- sort(unique(bFeatures[[2]]))
  } else {
    subMat <- mat
    uVals <- sort(unique(theFeatures[[2]]))
  }

  # body ----
  out <- list()
  flattened <- NULL
  for(i in seq_along(uVals)){
    temp <- mat
    temp[subMat != uVals[i] | is.na(subMat)] <- background
    if(flatten){
      temp[subMat == uVals[i]] <- 1
      flattened <- " and flattened"
    }

    # build output ----
    history <- paste0("the object has been segregated by the value '", uVals[i], "'", flattened)

    rleVal <- rle(as.vector(temp))
    theFeatures <- tibble(val = rleVal$values,
                          len = rleVal$lengths)
    theGroups <- tibble(gid = sort(unique(theFeatures$val)))

    temp <- new(Class = "geom",
               type = "grid",
               name = paste0(label, " [", uVals[i], "]"),
               point = rbind(theExtent[1,], dims, theRes),
               feature = theFeatures,
               group = theGroups,
               window = getWindow(x = obj),
               crs = getCRS(x = obj),
               history = c(getProvenance(obj), list(history)))

    out <- c(out, set_names(list(temp), paste0(label, " [", uVals[i], "]")))
  }

  # manage the bibliography entry ----
  bib <- "mmand"
  options(bibliography = unique(c(getOption("bibliography"), bib)))

  return(out)
}
