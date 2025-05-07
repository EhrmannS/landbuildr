#' Cell value distribution
#'
#' Calculate distribution parameters of cell values of a raster
#' @param obj [gridded(1)][geom]\cr The object to measure.
#' @param param [character(.)][character]\cr parameter(s) to calculate; possible
#'   parameters are \code{"mean"}, \code{"sum"}, \code{"number"}, \code{"sd"},
#'   \code{"cv"}, \code{"iqr"}, \code{"min"}, \code{"median"}, \code{"max"},
#'   \code{"quantile"}, \code{"weighted.mean"} or \code{"all"}.
#' @param label [character(1)][character]\cr the label by which this function
#'   shall be callable in a derived metric.
#' @param groupBy [character(1)][character]\cr layer based on the unique values
#'   of which to calculate distribution parameters (by default the second
#'   layer).
#' @return A list with elements resulting from the stratification with the value
#'   of each specified parameter per object in \code{groupBby}.
#' @family generic landscape metrics
#' @examples
#' con <- gtGeoms$grid$continuous
#' patches <- mdf_binarise(con, 30) %>%
#'   mdf_patches(background = 0)
#'
#' # the average and standard deviation of all values
#' msr_values(obj = con, param = c("mean", "sd"))
#'
#' # destribution parameters per patch ...
#' msr_values(obj = raster::stack(con, patches),
#'            param = c("weighted.mean", "min", "max"), groupBy = "patches")
#'
#' # ... or per class
#' msr_values(obj = raster::stack(con, gtGeoms$grid$categorical),
#'            param = c("mean", "sd"), groupBy = "categorical")
#'
#' @importFrom checkmate assertSubset assertCharacter testClass
#' @importFrom geomio getNames getExtent getFeatures getRes
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom rlang set_names
#' @importFrom stats weighted.mean quantile
#' @export

msr_values <- function(obj = NULL, param = NULL, label = NULL, groupBy = NULL){

  allParam <- c("mean", "sum", "number", "sd", "cv", "iqr", "min", "median", "max", "quantile", "weighted.mean")
  assertSubset(x = param, choices = c(allParam, "all"))
  if(any(param == "all")){
    param <- allParam
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
    assertCharacter(label, ignore.case = TRUE, any.missing = FALSE, len = 1)

    temp <- list(fun = "msr_values",
                 param = param,
                 groupBy = groupBy)

    obj@metrics <- c(obj@metrics, set_names(x = list(temp), nm = label))

    return(obj)
  }

  assertCharacter(groupBy, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(is.null(groupBy)){
    groupBy <- getNames(obj)[1]
  } else{
    assertSubset(groupBy, choices = getNames(obj))
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

  # adapt parameter names to have the respective function name
  param <- case_when(
    param == "iqr" ~ "IQR",
    param == "number" ~ "length",
    .default = param
  )

  # make list of values and levels to process ----
  if(!is.null(groupBy)){
    grouped <- as.matrix(eval(parse(text = paste0("obj$", groupBy))))
    vals <- sort(base::unique(as.vector(grouped[!is.na(grouped)])))
    values <- lapply(seq_along(vals), function(x){
      temp <- mat[grouped == vals[x]]
      temp[!is.na(temp)]
    })
    # values <- setNames(values, vals)
  } else{
    values <- values(obj)
    values <- list(values[!is.na(values)])
    vals <- 1
  }

  out <- NULL
  # handle 'weighted.mean' and 'quantile', because they can't be retrieved
  # with the below do.call snippet
  if(any(param == "weighted.mean")){
    out_wm <- sapply(seq_along(vals), function(x){
      tempVals <- table(values[[x]])
      dimnames(tempVals) <- NULL
      tempWeights <- rep(tempVals, tempVals)
      stats::weighted.mean(x = sort(values[[x]]),
                           w = tempWeights)
    })
    param <- param[-which(param == "weighted.mean")]
  } else{
    out_wm <- NULL
  }

  if(any(param=="quantile")){
    out_q <- sapply(seq_along(values), function(x){
      quantile(values[[x]])
    })
    out_q <- t(out_q)
    colnames(out_q) <- paste0("q", substr(colnames(out_q), 0, nchar(colnames(out_q))-1))
    param <- param[-which(param =="quantile")]
  } else{
    out_q <- NULL
  }

  # handle all other functions ----
  for(i in seq_along(param)){

    temp <- unlist(lapply(
      seq_along(values), function(j){
        do.call(what = param[i], args = list(values[[j]]))
      }
    ))
    out <- cbind(out, temp)
    colnames(out)[i] <- param[i]
  }

  if(!is.null(out_wm)) out <- cbind(out, wgh.mean = out_wm)
  if(!is.null(out_q)) out <- cbind(out, out_q)

  result <- tibble(value = vals, out)

  return(result)
}
