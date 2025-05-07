#' Morphologically erode a gridded object
#'
#' The morphological operation 'erode' decreases the value of cells that match a
#' structuring element.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param struct [struct(1)][struct]\cr the structuring element based on which to select
#'   cells for erosion; see \code{\link{setStruct}} for details.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimension as \code{obj}, where an erosion has
#'   been performed.
#' @family operators to morphologically modify a raster
#' @examples
#' input <- gtGeoms$grid$continuous
#' binarised <- mdf_binarise(obj = input, thresh = 30)
#'
#' # erode with the default structuring element
#' mdf_erode(obj = binarised) %>%
#'   visualise(binarised, .)
#'
#' # use another structuring element
#' box <- setStruct(custom = matrix(1, 3, 3))
#' mdf_erode(obj = binarised, struct = box) %>%
#'   visualise(binarised, .)
#'
#' # dilate also non-binarised gridded objects
#' mdf_erode(obj = input) %>%
#'   visualise(input, .)
#' @importFrom checkmate assertClass testCharacter testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom mmand binary
#' @importFrom tibble tibble
#' @export

mdf_erode <- function(obj = NULL, struct = NULL, label = NULL){

  # check arguments ----
  assertClass(x = struct, classes = "struct", null.ok = TRUE)
  if(is.null(struct)) struct <- setStruct(type = "disc", width = 3, height = 3)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "eroded"
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

    temp <- list(fun = "mdf_erode",
                 struct = struct)

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
                ncol = dims[2],
                nrow = dims[1])
  uVals <- unique(theFeatures[[2]])

  # body ----
  blend <- 1 # morphC::blendIdentity
  if(!binary(mat)){
    values <- uVals[uVals != 0]
    if(!binary(struct@pattern)){
      blend <- 6 # morphC::blendMinus
    }
  } else{
    values <- c(1)
  }
  struct@pattern[struct@pattern==0] <- NA
  # use either morph, or dilate from mmand
  # temp <- morphC(mat = mat, kernel = struct@pattern,
  #                value = values, blend = blend,
  #                merge = 1, # morphC::mergeMin
  #                rotateKernel = FALSE, strictKernel = FALSE)

  # build output ----
  history <- paste0("the object has been morphologically eroded")
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = unique(theFeatures$gid))

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
