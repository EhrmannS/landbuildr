#' Measure gridded objects
#'
#' Landscape metrics typically quantify spatial patterns of 2D lattices, such as
#' maps of landscapes, artificial (spatial) patterns or even photographs.
#' @param obj [gridded(1)][geom]\cr the gridded object to measure
#' @param ... [card][card(.)]\cr any number of derived landscape metrics by
#'   which the object in \code{x} shall be measured.
#' @param background [integerish(1)][integer]\cr the value any cell with value
#'   NA should have.
#' @param simplify [logical(1)][logical]\cr should a "nice looking" output be
#'   created, where the resulting values are associated to the correct ids
#'   (\code{TRUE}, default), or should the raw values be returned
#'   (\code{FALSE})?
#' @family generic landscape metrics
#' @return depending on the employed metric, but typically a \code{data.frame}.
#' @details A landscape metric can be generic or derived. In the first case the
#'   metric is a list that includes the \code{operator} name and its arguments.
#'   In the latter case these generic metrics are considered as \emph{terms},
#'   while the derived metric is given as its mathematical equation, where the
#'   terms are related to each other. You can find the equations for landscape
#'   metrics in the vignette with \code{vignette("landscape_metrics", package =
#'   "rasterTools")}
#'
#'   The following operators for generic metrics are defined: \itemize{ \item
#'   \code{\link{msr_adjacency}}: Determine the adjacency matrix of a gridded object. \item
#'   \code{\link{msr_area}}: Calculate the area of objects in a gridded object. \item
#'   \code{\link{msr_number}}: Count the number of objects in a gridded object. \item
#'   \code{\link{msr_perimeter}}: Calculate the length of the boundary of objects
#'   in a gridded object. \item \code{\link{msr_values}}: Summarise the values of objects
#'   in a gridded object. }
#' @examples
#' cat <- gtGeoms$grid$categorical
#'
#' # create some derived landscape metrics
#' myMtrcs <- msr_area(scale = "patch", label = "ap") %>%
#'   msr_area(scale = "class", label = "ac") %>%
#'   msr_area(scale = "landscape", label = "al") %>%
#'   punch(equation = "ac / al * 100", label = "class proportional area",
#'         note = "this metric approaches 0 (100) when the respective class becomes increasingly rare (frequent)") %>%
#'   punch(equation = "max(ap) / al * 100", label = "largest patch index",
#'         note = "this is a measure of dominance that approaches 0 (100) when the largest patch is increasingly small (large).")
#'
#' # and take measure with it
#' measure(obj = cat, myMtrcs)
#' @importFrom checkmate assertIntegerish
#' @importFrom geomio getExtent getFeatures getRes
#' @importFrom rlang enquos
#' @export

measure <- function(obj, ..., background = NA){

  # check other landscape metrics
  #
  # Patton DR. A Diversity Index for Quantifying Habitat “Edge.” Wildlife Society Bulletin. 1975;3:171–3. http://www.jstor.org/stable/3781151
  # Fragstat
  # https://r-spatialecology.github.io/landscapemetrics/index.html



  assertIntegerish(x = background)

  metrics <- enquos(...)

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

  # first identify all unique generic metrics
  for(i in seq_along(metrics)){


  }





  # assertList(with, types = c("list", "character"), min.len = 1, any.missing = FALSE)
  # assertLogical(simplify)
  #
  # if(!isList){
  #   input <- list(input)
  #   objNames <- "thisObject"
  # } else{
  #   objNames <- lapply(seq_along(input), function(x){
  #     names(input[[x]])
  #   })
  # }
  # out <- input
  #
  # # determine which elements are terms and which elements are equations
  # isTerms <- unlist(lapply(
  #   seq_along(with), function(x){
  #     testList(with[[x]])
  #   }
  # ))
  #
  # if(!any(isTerms)){
  #   stop("please specify at least one generic metric.")
  # }
  # terms <- with[isTerms]
  # isEquations <- unlist(lapply(
  #   seq_along(with), function(x){
  #     testCharacter(with[[x]])
  #   }
  # ))
  # if(!any(isEquations)){
  #   simplify <- FALSE
  # }
  # equations <- with[isEquations]
  # metricNames <- names(equations)
  #
  # # go through input elements
  # for(i in seq_along(input)){
  #
  #   value_list <- list()
  #   result_list <- list()
  #   # determine the value of each generic metric (term)
  #   for(j in seq_along(terms)){
  #
  #     tempTerm <- terms[[j]]
  #     termName <- names(terms[j])
  #
  #     # assign 'layer' in case this is missing
  #     if(!any(names(tempTerm) == "layer")){
  #       tempTerm <- c(tempTerm, list(layer = names(input[[i]])[1]))
  #     }
  #
  #     # call the function and assign names
  #     values <- do.call(what = tempTerm$operator,
  #                       args = c(tempTerm[-1], obj =  input[[1]]))
  #     colnames(values)[!names(values) %in% c("landscape", "class", "patch", "value")] <- "result"
  #     values <- values[c(!values[,names(values) != "result"] %in% background),]
  #
  #     value_list <- c(value_list, setNames(list(values), termName))
  #   }
  #
  #   # make a list of IDs
  #   id_list <- lapply(seq_along(value_list), function(x){
  #     ids <- value_list[[x]]
  #     ids[!names(ids) %in% "result"]
  #   })
  #
  #   # grab the resulting values from 'value_list'
  #   theValues <- lapply(seq_along(terms), function(x){
  #     value_list[[x]][,which(colnames(value_list[[x]]) == "result")]
  #   })
  #   names(theValues) <- names(terms)
  #
  #   # compute the result
  #   for(k in seq_along(equations)){
  #     theResult <- round(eval(parse(text = equations[[k]]), envir = theValues), 2)
  #     result_list <- c(result_list, setNames(list(theResult), metricNames[k]))
  #   }
  #
  #   if(simplify){
  #     # get the number of rows per generic metric ...
  #     elemInValues <- lapply(seq_along(value_list), function(x){
  #       dim(value_list[[x]])[1]
  #     })
  #
  #     # ... and try to match it with the results. If a generic metric has the same
  #     # length, the ids are what we want
  #     idInResults <- NULL
  #     tempOut <- lapply(seq_along(result_list), function(x){
  #       nElements <- length(result_list[[x]])
  #       idPos <- which(elemInValues == nElements)[[1]]
  #       idInResults <- c(idInResults, idPos)
  #
  #       id_list[[idInResults]] <- data.frame(id_list[[idInResults]], result_list[[x]], fix.empty.names = FALSE)
  #       names(id_list[[idInResults]])[dim(id_list[[idInResults]])[2]] <- "result"
  #
  #       id_list[[unique(idInResults)]]
  #     })
  #     names(tempOut) <- metricNames
  #     out[[i]] <- tempOut
  #   } else{
  #     out[[i]] <- c(value_list, result_list)
  #   }
  # }
  # names(out) <- objNames
  #
  # if(length(out) == 1){
  #   out <- out[[1]]
  # }
  #
  # return(out)

}
