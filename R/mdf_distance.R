#' Calculate the distance map for a gridded object
#'
#' The distance map of a binarised gridded object contains the distance of each
#' background cell to the nearest foreground cell.
#' @param obj [gridded(1)][geom]\cr any binarised gridded object to modify.
#' @param method [character(1)][character]\cr the distance measure to be
#'   calculated. Either \code{"euclidean"} (default), \code{"manhatten"} or
#'   \code{"chessboard"} distance.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimension as \code{obj}, where the value of
#'   the background cells has been replaced with the distance to the nearest
#'   foreground cell.
#' @details In contrast to \code{\link[raster]{distance}}, the distance values
#'   here do not warp around the boundaries of the map.
#'
#' @references Meijster, A., Roerdink, J.B.T.M., Hesselink, W.H., 2000. A
#'   general algorithm for computing distance transforms in linear time, in:
#'   Goutsias, J., Vincent, L., Bloomberg, D.S. (Eds.), Mathematical Morphology
#'   and Its Applications to Image and Signal Processing. Springer, pp. 331–340.
#' @family operators to modify cell values
#' @examples
#' input <- gtGeoms$grid$continuous
#'
#' # the different distance metrics
#' binarised <- mdf_binarise(obj = input, thresh = 40)
#' disEuc <- mdf_distance(obj = binarised)
#' disMan <- mdf_distance(obj = binarised, method = "manhattan")
#' disChb <- mdf_distance(obj = binarised, method = "chessboard")
#'
#' visualise(binarised, euclidean = disEuc,
#'           manhattan = disMan, chessboard = disChb)
#'
#' # calculate distance from edge to patch interior
#' mdf_permute(obj = binarised) %>%
#'   mdf_distance() %>%
#'   visualise(binarised, .)
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS
#'   getProvenance matDistanceCpp
#' @importFrom rlang set_names
#' @importFrom mmand binary
#' @importFrom tibble tibble
#' @export

mdf_distance <- function(obj, method = "euclidean", label = NULL){

  # check arguments ----
  assertChoice(x = method, choices = c("euclidean", "manhattan", "chessboard"))

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "distance"
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

    temp <- list(fun = "mdf_distance",
                 method = method)

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

  temp <- matDistanceCpp(mat, method = method)

  if(method == "euclidean"){
    temp <- sqrt(temp)
  }

  # manage the bibliography entry ----
  bib = bibentry(bibtype = "incollection",
                 title = "A general algorithm for computing distance transforms in linear time",
                 volume = "18",
                 isbn = "978-0-306-47025-7",
                 booktitle = "Mathematical Morphology and its Applications to Image and Signal Processing",
                 publisher = "Springer",
                 author = c(
                   person("A", "Meijster"),
                   person(c("J", "B", "T", "M"), "Roerdink"),
                   person(c("W", "H"), "Hesselink")
                 ),
                 editor = c(
                   person("John", "Goutsias"),
                   person("Luc", "Vincent"),
                   person(c("Dan", "S"), "Bloomberg")
                 ),
                 year = "2000",
                 pages = "331--340")
  options(bibliography = unique(c(getOption("bibliography"), bib)))

  # build output ----
  history <- paste0("distance values have been calculated according to the '", method, "'-distance")
  theFeatures <- tibble(gid = as.vector(temp))
  theGroups <- tibble(gid = sort(unique(theFeatures$gid)))

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
