#' Determine the centroid of patches
#'
#' The centroid is the average location of all cells of a foreground patch.
#' @param obj [gridded(1)][geom]\cr any gridded object (with patches) to modify.
#' @param gridded [logical(1)][logical]\cr whether the output is returned as a
#'   gridded object or by default as a point geom.
#' @param background [integerish(1)][integer]\cr the value any cell with value
#'   NA should have.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return Depending on \code{gridded}, either \enumerate{ \item a grid geom of
#'   the same dimension as \code{obj}, in which the centroid of each foreground
#'   patch has the value of the patch-number identified with
#'   \code{\link{mdf_patches}} and where the remaining cells of each foreground
#'   patch have the value NA or \item a point geom of the centroids.}
#' @family operators to determine objects
#' @examples
#' patches <- gtGeoms$grid$continuous %>%
#'   mdf_binarise(thresh = 30) %>%
#'   mdf_patches()
#'
#' # identify patch centroids
#' mdf_centroid(obj = patches) %>%
#'   visualise(patches, .)
#' @importFrom checkmate assertLogical assertIntegerish testCharacter testClass
#' @importFrom geomio getExtent getPoints getFeatures getRes getWindow getCRS
#'   getProvenance
#' @importFrom rlang set_names
#' @importFrom stats aggregate
#' @importFrom tibble as_tibble tibble
#' @export

mdf_centroid <- function(obj = NULL, gridded = TRUE, background = NA,
                         label = NULL){

  # check arguments ----
  assertLogical(x = gridded, len = 1)
  assertIntegerish(x = background)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "centered"
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

    temp <- list(fun = "mdf_centroid",
                 gridded = gridded,
                 background = background)

    obj@operators <- c(obj@operators, set_names(x = list(temp), nm = label))

    return(obj)
  }

  # pull data ----
  theExtent <- getExtent(x = obj)
  thePoints <- getPoints(x = obj)
  theFeatures <- getFeatures(x = obj)
  theRes <- getRes(x = obj)
  dims <- round(c((theExtent$x[2] - theExtent$x[1])/theRes[1],
                  (theExtent$y[2] - theExtent$y[1])/theRes[2]), 0)

  # make temporary objects ----

  # body ----
  dat <- merge(x = theFeatures, thePoints, by = "fid")
  dat <- dat[-which(colnames(dat) %in% c("fid"))]
  datNNA <- dat[!is.na(dat[[1]]),]

  # determine centroids by averaging all cell coordinates per patch
  theMeans <- aggregate(x = datNNA, by = list(datNNA[[1]]), FUN = mean)
  theMeans <- theMeans[, c(2, which(colnames(theMeans) %in% c("x", "y")))]

  # build output ----
  history <- paste0("the centroids of patches have been determined")

  if(gridded){
    # round to next 0.5 by steps of 1
    theMeans[c("x", "y")] <- round(theMeans[c("x", "y")]-0.5)+0.5
    theMeans <- merge(dat[-1], theMeans, by = c("y", "x"), all = TRUE)

    # set na
    theMeans[[3]][is.na(theMeans[[3]])] <- background

    theFeatures <- tibble(gid = as.vector(theMeans[[3]]))
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

  } else{

    theMeans$y <- dims[2] - theMeans$y
    thePoints <- tibble(x = theMeans$x, y = theMeans$y, fid = theMeans[[1]])
    theFeatures <- tibble(fid = theMeans[[1]], gid = theMeans[[1]])
    theGroups <- tibble(gid = theMeans[[1]])
    out <- new(Class = "geom",
               type = "point",
               name = label,
               point = thePoints,
               feature = theFeatures,
               group = theGroups,
               window = getWindow(x = obj),
               crs = getCRS(x = obj),
               history = c(getProvenance(obj), list(history)))

  }

  return(out)
}
