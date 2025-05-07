#' Modify gridded objects
#'
#' Typical GIS operations modify gridded objects according to a given process.
#' This can serve to identify certain objects or to prepare the quantitative
#' assessment of the spatial object in question.
#' @param input a raster object or a named list containing raster objects, which
#'   should be modified. Typically retrieved via \code{\link{obtain}}, but can
#'   also be assembled "by hand".
#' @param by list of \code{operators}, in which the raster modification
#'   functions are specified. Each \code{operator} is a list iteself and
#'   includes the operator name and its arguments as sub-elements; see
#'   Examples.
#' @param sequential logical; should the defined operators be carried out based
#'   on the output of the previous operator (TRUE), or separately based on the
#'   original input (FALSE, default); see Details.
#' @param merge logical; should the resulting object be merged to a raster stack
#'   (\code{TRUE}), or should it remain a list (\code{FALSE}, default).
#' @param keepInput logical; should \code{input} be retained (\code{TRUE}) or
#'   should it be discarded (\code{FALSE}, default)?
#' @param envir the environment to which the output of this function should be
#'   saved.
#' @details Operators can be called several successive times with modified
#'   arguments. The following operators are recently defined...
#'
#'   ... to select a subset of cells: \itemize{
#'     \item \code{\link{mdf_layerise}}: Segregate values of a gridded object into layers.
#'     \item \code{\link{mdf_range}}: Select cells based on a value range.
#'     \item \code{\link{rMask}}: Select cells of a raster based on a mask.
#'     \item \code{\link{rMatch}}: Match cells of a raster with a kernel.
#'   }
#'
#'   ... to modify cell values: \itemize{
#'     \item \code{\link{rBinarise}}: Binarise the values in a raster.
#'     \item \code{\link{mdf_categorise}}: Assign categories to the values in a raster.
#'     \item \code{\link{rDistance}}: Calculate the distance map for a raster.
#'     \item \code{\link{rFillNA}}: Fill NA values in a raster.
#'     \item \code{\link{mdf_permute}}: Assign a permutation to the cell values of a raster.
#'     \item \code{\link{mdf_perturb}}: Slightly modify (randomise) the values in a gridded object.
#'     \item \code{\link{mdf_replace}}: Replace values in a gridded object.
#'     \item \code{\link{rRange}}: Change the scale of the values in a raster.
#'   }
#'
#'   ... to determine objects: \itemize{
#'     \item \code{\link{rCentroid}}: Determine the centroid of foreground patches in a raster.
#'     \item \code{\link{mdf_componentise}}: Determine components (patches) in a gridded object.
#'     \item \code{\link{rSkeletonise}}: Determine the skeleton of foreground patches in a raster.
#'   }
#'
#'   ... to morphologically modify a raster: \itemize{
#'     \item \code{\link{rDilate}}: Morphologically dilate foreground patches in a raster.
#'     \item \code{\link{rErode}}: Morphologically erode foreground patches in a raster.
#'   }
#'
#'   ... to modify the overall raster: \itemize{
#'     \item \code{\link{rAggregate}}: Combine a raster stack after segregation.
#'     \item \code{\link{rBlend}}: Blend two rasters with each other.
#'     \item \code{\link{rRescale}}: Rescale a raster.
#'   }
#'
#'   Moreover, you can create your own operator or check this package's
#'   \href{https://github.com/EhrmannS/lomm}{github} page to suggest new
#'   algorithms or make a pull-request.
#' @return A list of rasters or a raster stack of modified objects according to
#'   the number of chosen datasets and (combinations of) operators.
#' @examples
#' obj <- lommData$continuous
#'
#' # employ modification with merely one operator
#' objBin <- rBinarise(thresh = 40, obj = obj)
#' visualise(objBin)
#'
#' # employ several operators combined to an algorithm, 'obj' does not need to
#' # be specified per operator, as 'modify' assigns it.
#' getPatches <- list(list(operator = "rBinarise", thresh = 40),
#'                    list(operator = "rPatches"))
#' patches <- modify(input = obj, by = getPatches, sequential = TRUE)
#' visualise(patches)
#'
#' # To run separated sub-algorithms, use names for each operator to specify
#' # which elements should be computed sequentially.
#' getPatchNCats <- list(get_patches = list(operator = "rBinarise", thresh = 40),
#'                       get_patches = list(operator = "rPatches"),
#'                       get_categories = list(operator = "rCategorise", n = 5))
#' patchNCats <- modify(input = obj, by = getPatchNCats, merge = TRUE)
#' visualise(patchNCats)
#'
#' # Create objects that are usable later in the algorithm
#' getMedialAxis <- list(skeleton = list(operator = "rSkeletonise", background = 0),
#'                       medAxis = list(operator = "rPermute"),
#'                       medAxis = list(operator = "rDistance"),
#'                       medAxis = list(operator = "rMask", mask = "skeleton"))
#' MAT <- modify(input = objBin, by = getMedialAxis, merge = TRUE)
#' visualise(MAT)
#' @importFrom raster stack
#' @export

modify <- function(input, by, sequential = FALSE, merge = FALSE,
                   keepInput = FALSE, envir = NULL){

  if(missing(by)){
    stop("please specify an algorithm by which to modify the spatial object.")
  }

  # check which input we are dealing with and adapt if needs be
  if(is.list(input)){
    objs <- unlist(input)
  } else {
    objs <- setNames(list(input), "thisObject")
  }

  # if the algos don't have names, assign generic ones and separate it into subalgos
  if(is.null(names(by))){
    if(sequential){
      names(by) <- rep("algorithm", length(by))
    } else{
      names(by) <- paste0("algorithm", seq(length(by)))
    }
  }
  subAlgoNames <- base::unique(names(by))
  if(keepInput){
    out <- setNames(list(input), "input")
  } else{
    out <- list()
  }

  for(j in seq_along(subAlgoNames)){

    tempObjs <- objs
    tempAlgorithm <- by[which(names(by) == subAlgoNames[j])]

    for(k in seq_along(tempAlgorithm)){

      # set the correct mask raster
      if(tempAlgorithm[[k]]$operator == "rMask"){
        if(is.character(tempAlgorithm[[k]]$mask)){
          if(tempAlgorithm[[k]]$mask == "input"){
            theMask <- input
          } else{
            theMask <- out[[which(names(out) == tempAlgorithm[[k]]$mask)]]
          }
          # warning when the string doesn't match and object of the algorithm
        }
      } else{
        theMask <- NULL
      }

      # # set the correct overlay raster
      # if(tempAlgorithm[[k]]$operator == "rBlend"){
      #
      # }

      # set the correct groups raster
      if(tempAlgorithm[[k]]$operator == "rSegregate"){
        if(is.character(tempAlgorithm[[k]]$by)){
          theGroups <- out[[which(names(out) == tempAlgorithm[[k]]$by)]]
          tempAlgorithm[[k]]$by <- theGroups
        }
      }

      # set a switch to aggregate layers
      if(tempAlgorithm[[k]]$operator == "rAggregate"){
        aggregate <- TRUE
      } else{
        aggregate <- FALSE
      }

      for(i in seq_along(tempObjs)){
        thisObj <- tempObjs[[i]]

        # if the object has more than one layer and aggregate != TRUE, go
        # through each layer separately; if aggregate == TRUE, treat the
        # multiple layer raster as one, because rAggregate expects several
        # layers to combine them.
        if(dim(thisObj)[3] > 1 & !aggregate){

          for(l in 1:dim(thisObj)[3]){

            # in case a mask has to be set and the mask contains several layers
            # (i.e. after segregating of the mask), assign the respective mask.
            if(!is.null(theMask)){
              if(dim(theMask)[3] == dim(thisObj)[3]){
                tempAlgorithm[[k]]$mask <- theMask[[l]]
              } else{
                tempAlgorithm[[k]]$mask <- theMask[[1]]
              }
            }

            modifiedObj <- do.call(what = tempAlgorithm[[k]]$operator,
                                   args = c(obj = list(thisObj[[l]]), tempAlgorithm[[k]][-1]))
            thisObj[[l]] <- modifiedObj

          }
          newHistory <- paste0("in layers: ", modifiedObj@history[[length(modifiedObj@history)]])
          thisObj@history <- c(thisObj@history, list(newHistory))
          tempObjs[[i]] <- thisObj

        } else{

          if(!is.null(theMask)){
            if(dim(theMask)[3] > 1){
              tempAlgorithm[[k]]$mask <- theMask[[1]]
              warning("only the first element of 'mask' was utilized.")
            } else{
              tempAlgorithm[[k]]$mask <- theMask
            }
          }

          modifiedObj <- do.call(what = tempAlgorithm[[k]]$operator,
                                 args = c(obj = list(thisObj), tempAlgorithm[[k]][-1]))
          tempObjs[[i]] <- modifiedObj

        }
      }

    }
    out <- c(out, setNames(object = tempObjs, nm = subAlgoNames[j]))

  }

  if(length(out) == 1){
    out <- out[[1]]
  }

  if(merge){
    out <- stack(out)
  }

  toEnvironment(object = out, name = "new_obj_mod", envir = envir)
}
