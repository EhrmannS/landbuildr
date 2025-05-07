#' Binarise a gridded object
#'
#' Transform a gridded object so that it has the values 0 and 1.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param thresh [integerish(1)][integer]\cr value above which the cell will be
#'   set to 1, below which it will be set to 0.
#' @param match [integerish(1)][integer]\cr one or more values which will be set
#'   to 1, while the remaining values will be set to 0.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom object of the same dimension as \code{obj}.
#' @family operators to modify cell values
#' @examples
#' input <- gtGeoms$grid$continuous
#'
#' # values larger than 30 are set to 1
#' mdf_binarise(obj = input, thresh = 30) %>%
#'   visualise(input, .)
#'
#' # all values except 1 are set to 0
#' mdf_binarise(obj = input, match = 1) %>%
#'   visualise(input, .)
#' @importFrom checkmate assertNumber assertNumeric testClass testCharacter
#' @importFrom geomio getExtent getFeatures getRes getWindow getCRS getProvenance
#' @importFrom mmand binarise
#' @importFrom tibble tibble
#' @importFrom rlang set_names `:=`
#' @export

mdf_binarise <- function(obj = NULL, thresh = NULL, match = NULL, label = NULL){

  # check arguments ----
  assertNumber(x = thresh, null.ok = TRUE)
  assertNumeric(x = match, null.ok = TRUE)

  if(!testCharacter(x = label, ignore.case = TRUE, any.missing = FALSE, len = 1)){
    label <- "binarised"
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

    temp <- list(fun = "mdf_binarise",
                 thresh = thresh,
                 match = match)

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
  uVals <- unique(theFeatures[[2]])

  # body ----
  if(is.null(thresh)){
    thresh <- max(uVals)
  } else{
    assertNumeric(x = thresh, lower = min(uVals, na.rm = TRUE), upper = max(uVals, na.rm = TRUE))
    if(!min(uVals, na.rm = TRUE) <= thresh | !thresh <= max(uVals, na.rm = TRUE)){
      stop("please provide a value for 'thresh' within the range of the values of 'obj'.")
    }
  }

  # in case 'match' is defined, remove these values from 'values'
  if(!is.null(match)){
    values <- uVals[!uVals %in% match]
    outLabel <- paste0("[. = (", paste0(match, collapse = ","), ")]")
    groupsLabel <- c(paste0(". != (", paste0(match, collapse = ","), ")"), paste0(". = (", paste0(match, collapse = ","), ")"))
  } else{
    values <- uVals
    outLabel <- paste0("[. > ", thresh, "]")
    groupsLabel <- c(paste0(". < ", thresh), paste0(". > ", thresh))
  }

  # select only values that are above 'thresh'
  temp <- binarise(x = mat)

  # build output ----
  history <- paste0("values have been binarised")

  rleVal <- rle(as.vector(temp))
  theFeatures <- tibble(val = rleVal$values,
                        len = rleVal$lengths)
  theGroups <- tibble(gid = sort(unique(theFeatures$val)),
                      !!outLabel := groupsLabel)

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
