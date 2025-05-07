#' Categorise a gridded object
#'
#' Transform the values of a gridded object to a (smaller) set of categories.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param breaks [integerish(1)][integer]\cr the break points, where categories
#'   should be delimited.
#' @param n [integerish(1)][integer]\cr number of categories.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimension as \code{obj}, in which the cells
#'   have the category value into which their values fall.
#' @details Using \code{n} will determine \code{breaks} based on the value-range
#'   of \code{obj} so that the values are assigned to n categories.
#'
#'   Assigning \code{breaks} is mostly useful when values are to be non-linear,
#'   such as \code{log(min:max)*max/log(max)}, but could also be \code{seq(min,
#'   max, length.out = 21)}, which corresponds to \code{n = 20}.
#' @family operators to modify cell values
#' @examples
#' input <- gtGeoms$grid$continuous
#'
#' # divide into 5 equal categories
#' mdf_categorise(obj = input, n = 5) %>%
#'   visualise(input, .)
#'
#' # use a particular set of breaks
#' mdf_categorise(obj = input, breaks = log(1:5)*5/log(5)*20) %>%
#'   visualise(input, .)
#' @importFrom checkmate assertNumeric assertNumber assertIntegerish
#'   testCharacter
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom rlang set_names
#' @importFrom stats setNames
#' @importFrom tibble tibble
#' @export

mdf_categorise <- function(obj, breaks = NULL, n = NULL, label = NULL){

  # check arguments ----
  assertNumeric(x = breaks, null.ok = TRUE)
  assertIntegerish(x = n, null.ok = TRUE)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "categories"
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

    temp <- list(fun = "mdf_categorise",
                 breaks = breaks,
                 n = n)

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

  # body ----
  theRange <- range(theFeatures[[2]], na.rm = TRUE)
  if(is.null(breaks)){
    assertIntegerish(x = n)
    breaks <- seq(theRange[1], theRange[2], length.out = n+1)
  }

  # manage the tails
  if(!any(breaks == theRange[1])){
    breaks <- c(theRange[1], breaks)
  }
  if(!any(breaks == theRange[2])){
    breaks <- c(breaks, theRange[2])
  }
  temp <- findInterval(theFeatures[[2]], breaks, rightmost.closed = TRUE)

  # build output ----
  outLabel <- paste0("[", paste0(round(breaks, 1), collapse = "/"), "]")
  history <- paste0("categories based on the breaks ", outLabel, " have been defined")

  rleVal <- rle(as.vector(temp))
  theFeatures <- tibble(val = rleVal$values,
                        len = rleVal$lengths)

  groupsLabel <- paste0(round(breaks[seq_along(unique(theFeatures$val))], 4), "-",
                        round(breaks[seq_along(unique(theFeatures$val))+1], 4))
  theGroups <- tibble(gid = sort(unique(theFeatures$val)),
                      bins = groupsLabel)

  out <- new(Class = "geom",
             type = "grid",
             name = paste0(label, " ", outLabel),
             point = rbind(theExtent[1,], dims, theRes),
             feature = theFeatures,
             group = theGroups,
             window = getWindow(x = obj),
             crs = getCRS(x = obj),
             history = c(getProvenance(obj), list(history)))

  return(out)
}
