#' Morphologically modify a gridded object
#'
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param struct [struct(1)][struct]\cr the structuring element based on which
#'   to select cells; see \code{\link{setStruct}} for details.
#' @param blend [character(1)][character]\cr \code{identity}, \code{equal},
#'   \code{lower}, \code{greater}, \code{plus}, \code{minus}, \code{product};
#'   see Details.
#' @param merge [character(1)][character]\cr \code{min}, \code{max}, \code{all},
#'   \code{any}, \code{sum}, \code{mean}, \code{median}, \code{sd}, \code{cv},
#'   \code{one}, \code{zero}, \code{na}; see Details.
#' @param strict [logical(1)][logical]\cr whether ...
#' @param rotate [logical(1)][logical]\cr whether the kernel should be rotated
#'   for all possible rotations or should be used as is.
#' @param background [integerish(1)][integer]\cr the value any cell with value
#'   NA should have.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @details The \code{morphC} function (internal) is the basis of many modify
#'   operations in \code{lomm} and \code{rMorph} exposes a fully functional
#'   interface of this C++ function. The morphC function iteratively goes
#'   through each pixel of a gridded object and compares a socalled
#'   \link[=struct]{structuring element} with the gridded object at the location
#'   of that pixel. The result of this comparison depends on the arguments
#'   \code{blend} and \code{merge}: \itemize{ \item First, all values that are
#'   covered by the structuring element are "cut out" and summarised pairwise
#'   with the structuring element (i.e. they are \emph{blended}) by the function
#'   defined in \code{blend} (e.g. "plus"), leaving as many values as cells in
#'   \code{struct}. \item Then these values are summarised into a single value
#'   (i.e. they are \emph{merged}) by the function defined in \code{merge} (e.g.
#'   "mean") and the resulting value is assigned in the current location in the
#'   raster.}
#'
#'   The following functions are defined for \code{blend}: \enumerate{ \item
#'   identity: the value of \code{obj} where \code{kernel} is not NA. \item
#'   equal: the value 1 where \code{obj} and \code{kernel} are equal, otherwise
#'   the value 0. \item lower: the values of \code{obj} that are lower than
#'   \code{kernel}, otherwise 0. \item greater: the values of \code{obj} that
#'   are greater than \code{kernel}, otherwise 0. \item plus: the values of
#'   \code{obj} added to the values of \code{kernel}. \item minus: the values of
#'   \code{kernel} subtracted from the values of \code{obj}. \item product: the
#'   product of the values of \code{obj} and \code{kernel}.} The following
#'   functions are defined for \code{merge}: \enumerate{ \item min: the minimum
#'   value. \item max: the maximum value. \item all: the value 1 if all non-NA
#'   values are not 0, otherwise 0. \item any: the value 1 if any of the non-NA
#'   values are not 0, otherwise 0. \item sum: the sum of all non-NA values.
#'   \item mean: the mean of all non-NA values. \item median: the median of all
#'   non-NA values. \item sd: the standard deviation of all non-NA values. \item
#'   cv: the coefficient of variation of all non-NA values. \item sumNa: if the
#'   sum of all values is greater than 0 than this sum, otherwise NA.}
#' @family operators to morphologically modify a raster
#' @references Credit for the original idea and the original C++ code of this
#'   function is due to Jon Clayden
#'   (\href{https://github.com/jonclayden/mmand}{R::mmand}). The functionality
#'   has been slightly extended here.
#' @importFrom checkmate assertClass assertSubset assertLogical assertIntegerish
#'   testCharacter testClass
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @export

mdf_morph <- function(obj = NULL, struct = NULL, blend = NULL, merge = NULL,
                      rotate = TRUE, strict = TRUE, background = NA,
                      label = NULL){

  # check arguments ----
  assertClass(x = struct, classes = "struct", null.ok = TRUE)
  if(is.null(struct)) struct <- setStruct(type = "disc", width = 3, height = 3)
  assertSubset(blend, choices = c("identity", "equal", "lower", "greater", "plus", "minus", "product"), empty.ok = FALSE)
  blendID <- which(c("identity", "equal", "lower", "greater", "plus", "minus", "product") %in% blend)
  assertSubset(merge, choices = c("min", "max", "all", "any", "!all", "!any", "sum", "mean", "median", "sd", "cv", "sumNa"), empty.ok = FALSE)
  mergeID <- which(c("min", "max", "all", "any", "!all", "!any", "sum", "mean", "median", "sd", "cv", "sumNa") %in% merge)
  assertLogical(x = rotate, any.missing = FALSE)
  assertLogical(x = strict, any.missing = FALSE)
  assertIntegerish(x = background)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "morphed"
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

    temp <- list(fun = "mdf_morph",
                 struct = struct,
                 blend = blend,
                 merge = merge,
                 rotate = rotate,
                 strict = strict,
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
                ncol = dims[1],
                nrow = dims[2],
                byrow = TRUE)
  uVals <- unique(theFeatures[[2]])

  # body ----
  temp <- morphC(mat = mat,
                 kernel = struct@pattern,
                 value = uVals,
                 blend = blendID,
                 merge = mergeID,
                 rotateKernel = rotate,
                 strictKernel = strict)
  temp[is.na(temp)] <- background

  # build output ----
  history <- list(paste0("the object has been morphologically modified (blend:", blend, ",merge:", merge, ")"),
                  paste0("cells have been matched with a ", dim(struct@pattern)[1], " x ", dim(struct@pattern)[2], " structuring element with values ", paste0(as.vector(struct@pattern), collapse = " ")))

  rleVal <- rle(as.vector(t(temp)))
  theFeatures <- tibble(val = rleVal$values,
                        len = rleVal$lengths)
  theGroups <- tibble(gid = sort(unique(theFeatures$val)))

  out <- new(Class = "geom",
             type = "grid",
             name = paste0(label, " [blend:", blend, ",merge:", merge, "]"),
             point = rbind(theExtent[1,], dims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = obj),
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), history))

  return(out)
}
