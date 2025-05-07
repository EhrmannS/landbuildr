#' Adjacency of cells
#'
#' Calculate the cell-adjacency matrix for a gridded object.
#' @param obj [gridded(1)][geom]\cr The object to measure.
#' @param type [character(1)][character]\cr which type of adjacencies to calculate;
#'   either \code{"like"} adjacencies between cells of the same value,
#'   adjacencies between \code{"paired"} values or the rowsum of the paired
#'   values (\code{"pairedSum"}).
#' @param count [character(1)][character]\cr the cells that should be counted;
#'   possible values are \code{"single"} to count only cells to the right and
#'   bottom of the focal cell and \code{"double"} to count additionally cells to
#'   the left and top of the focal cell.
#' @param label [character(1)][character]\cr the label by which this function
#'   shall be callable in a derived metric.
#' @details In case \code{type = "like"}, only the diagonal of the adjacency
#'   matrix is returned, in case \code{type = "paired"}, the complete adjacency
#'   matrix is returned.
#' @return a \code{data.frame} of the frequency of adjacencies of the values in
#'   \code{obj}.
#' @family generic landscape metrics
#' @examples
#' cat <- gtGeoms$grid$categorical
#' bin <- mdf_binarise(gtGeoms$grid$continuous, thresh = 40)
#'
#' # double count like adjacencies
#' msr_adjacency(obj = cat)
#'
#' # paired adjacencies
#' msr_adjacency(obj = cat, type = "paired")
#'
#' # adjacencies with single count
#' msr_adjacency(obj = bin, count = "single")
#'
#' @importFrom checkmate assertCharacter assertChoice assertSubset
#' @importFrom geomio getExtent getFeatures getRes countCellAdjacenciesCpp
#' @importFrom tibble tibble as_tibble
#' @export

msr_adjacency <- function(obj = NULL, type = "like", count = "double",
                          label = NULL){

  assertChoice(x = type, choices = c("like", "paired", "pairedSum"))
  assertChoice(x = count, choices = c("single", "double"))

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

    temp <- list(fun = "msr_adjacency",
                 type = type,
                 count = count)

    obj@metrics <- c(obj@metrics, set_names(x = list(temp), nm = label))

    return(obj)
  }

  if(count == "single"){
    countDouble <- FALSE
  } else{
    countDouble <- TRUE
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

  values <- countCellAdjacenciesCpp(mat = mat, countDouble = countDouble)
  rownames(values) <- uVals
  colnames(values) <- uVals

  if(type == "like"){

    result <- tibble(class = uVals, likeAdj = diag(values))

  } else if(type == "paired"){

    values <- as_tibble(values)
    result <- cbind(class = uVals, values)

  } else{

    result <- tibble(class = uVals, pairedSum = rowSums(values))

  }

  return(result)
}
