#' Determine the skeleton of foreground patches
#'
#' The morphological skeleton of a patch is a binary skeletal remnant that
#' preserves, when using the hit-or-miss algorithm (default), the extent and
#' connectivity (i.e. the topology) of the patch.
#' @param obj [gridded(1)][geom]\cr any binarised gridded object (with binary
#'   values) to modify.
#' @param method [character(1)][character]\cr the method to determine the
#'   skeleton. Either \code{"hitormiss"} (default), \code{"lantuejoul"} or
#'   \code{"beucher"}.
#' @param struct [struct(1)][struct]\cr the structuring element based on which
#'   to select cells; see \code{\link{setStruct}} for details.
#' @param background [integerish(1)][integer]\cr the value any cell with value
#'   NA should have.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return a grid geom of the same dimensions as \code{obj}, in which foreground
#'   patches have been transformed into their morphological skeletons.
#' @family operators to determine objects
#' @examples
#' binarised <- mdf_binarise(obj = gtGeoms$grid$continuous, thresh = 30)
#'
#' # identify a topology preserving skeleton
#' mdf_skeletonise(obj = binarised) %>%
#'   visualise(binarised, .)
#' @importFrom checkmate assertChoice assertClass assertIntegerish testCharacter
#'   testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getProvenance
#' @importFrom rlang set_names
#' @importFrom mmand skeletonise binary
#' @importFrom tibble tibble
#' @export

mdf_skeletonise <- function(obj = NULL, method = "hitormiss", struct = NULL,
                            background = NA, label = NULL){

  # check arguments ----
  assertChoice(x = method, choices = c("lantuejoul", "beucher", "hitormiss"))
  assertClass(x = struct, classes = "struct", null.ok = TRUE)
  if(is.null(struct)) struct <- setStruct(type = "disc", width = 3, height = 3)
  assertIntegerish(x = background)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "skeletonised"
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

    temp <- list(fun = "mdf_skeletonise",
                 method = method,
                 struct = struct,
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
                ncol = dims[2],
                nrow = dims[1])

  # body ----
  if(!binary(mat)){
    stop("'obj' is not binary, please run 'mdf_binarise()' first.")
  }
  temp <- skeletonise(x = mat, kernel = struct, method = method)
  temp[temp == 0] <- background

  # manage the bibliography entry ----
  bib <- "mmand"
  options(bibliography = unique(c(getOption("bibliography"), bib)))

  # build output ----
  history <- paste0("the morphological skeleton has been determined")
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = unique(theFeatures$gid))

  out <- new(Class = "geom",
             type = "grid",
             name = paste0(label, " [", method, "]"),
             point = rbind(theExtent[1,], dims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = obj),
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), list(history)))

  return(out)
}
