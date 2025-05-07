#' Determine patches in a gridded object
#'
#' Patches are sets of cells that are connected.
#' @param obj [gridded(1)][geom]\cr any binarised gridded object to modify.
#' @param struct [struct(1)][struct]\cr the structuring element based on which
#'   to select cells; see \code{\link{setStruct}} for details.
#' @param background [integerish(1)][integer]\cr the value any cell with value
#'   NA should have.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimension as \code{obj}, in which
#'   neighboring cells of the foreground have been assigned the same value,
#'   forming patches.
#' @family operators to determine objects
#' @examples
#' binarised <- gtGeoms$grid$continuous %>%
#'   mdf_binarise(thresh = 30)
#'
#' patches <- mdf_componentise(obj = binarised)
#'
#' visualise(binarised, patches)
#' @importFrom checkmate assertClass assertIntegerish testClass testCharacter
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#' @importFrom mmand binary components
#' @export

mdf_componentise <- function(obj = NULL, struct = NULL, background = NA, label = NULL){

  # check arguments ----
  assertClass(x = struct, classes = "struct", null.ok = TRUE)
  if(is.null(struct)) struct <- setStruct(type = "disc", width = 3, height = 3)
  assertIntegerish(x = background)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "patches"
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

    temp <- list(fun = "mdf_componentise",
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
                ncol = dims[1],
                nrow = dims[2],
                byrow = TRUE)

  # body ----
  if(!binary(mat)){
    stop("'obj' is not binary, please run 'mdf_binarise()' first.")
  }
  temp <- components(mat, struct@pattern)

  # set na
  temp[is.na(temp)] <- background

  # manage the bibliography entry ----
  bib <- "mmand"
  options(bibliography = unique(c(getOption("bibliography"), bib)))

  # build output ----
  history <- paste0("patches have been determined")

  rleVal <- rle(as.vector(t(temp)))
  theFeatures <- tibble(val = rleVal$values,
                        len = rleVal$lengths)
  theGroups <- tibble(gid = sort(unique(theFeatures$val)))

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
