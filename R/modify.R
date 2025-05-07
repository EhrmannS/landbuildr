#' Modify gridded objects
#'
#' Geoprocessing workflows that modify an input can be broken down into a
#' sequence of smallest functional building blocks, socalled geo-operators.
#' \code{modify} is the core function that modifies gridded input objects based
#' on a sequence (an algorithm) of those geo-operators.
#' @param input [gridded(1)][geom]\cr any gridded object to modify.
#' @param by [\code{list(.)}]\cr algorithm in which the operators to modify
#'   \code{input} are specified; see Examples.
#' @param sequential [\code{logical(1)}]\cr should the algorithm in \code{by} be
#'   carried out based on the output of the previous operator (\code{TRUE}), or
#'   separately based on the original input (\code{FALSE}, default); see
#'   Details.
#' @param merge [\code{logical(1)}]\cr should the resulting object be merged to
#'   a raster stack (\code{TRUE}), or should it remain a list (\code{FALSE},
#'   default).
#' @param keepInput [\code{logical(1)}]\cr should \code{input} be retained
#'   (\code{TRUE}) or should it be discarded (\code{FALSE}, default)?
#' @details The following geo-operators are recently defined...
#'
#'   ... to select a subset of cells: \itemize{
#'   \item \code{\link{ls_select}}: Select cells that are comprised in a value range.
#'   \item \code{\link{ls_mask}}: Select cells based on a mask.
#'   \item \code{\link{ls_ match}}: Select cells based on a stencil.
#'   }
#'
#'   ... to modify cell values: \itemize{
#'   \item \code{\link{ls_binarise}}: Binarise a gridded object.
#'   \item \code{\link{ls_categorise}}: Categorise a gridded object.
#'   \item \code{\link{ls_distance}}: Calculate the distance map for a gridded object.
#'   \item \code{\link{ls_fillNA}}: Fill NA values in a gridded object.
#'   \item \code{\link{ls_offset}}: Offset the values in a gridded object.
#'   \item \code{\link{ls_permute}}: Permute the cell values of a gridded object.
#'   \item \code{\link{ls_rescale}}: Rescale the values of a gridded object.
#'   \item \code{\link{ls_substitute}}: Substitute values in a gridded object.
#'   }
#'
#'   ... to determine objects: \itemize{ \item \code{\link{ls_centroid}}:
#'   Determine the centroid of foreground patches. \item
#'   \code{\link{ls_patches}}: Determine foreground patches. \item
#'   \code{\link{ls_skeletonise}}: Determine the skeleton of foreground patches.
#'   }
#'
#'   ... to morphologically modify a gridded object: \itemize{ \item
#'   \code{\link{ls_ morph}}: Morphologically modify a gridded object. \item
#'   \code{\link{ls_ dilate}}: Morphologically dilate a gridded object. \item
#'   \code{\link{ls_ erode}}: Morphologically erode a gridded object. }
#'
#'   ... to modify the overall gridded object: \itemize{ \item
#'   \code{\link{ls_blend}}: Blend two rasters with each other. \item
#'   \code{\link{ls_ reduce}}: Combine a stack of gridded objects. \item
#'   \code{\link{ls_ resample}}: Change the resolution of a raster. \item
#'   \code{\link{ls_ rescale}}: Rescale the values of a gridded object. \item
#'   \code{\link{ls_ segregate}}: Segregate values in a raster into layers. }
#'
#'   Moreover, you can create your own operator or suggest new algorithms at github.
#' @return A list of \code{RasterLayer}s or a \code{RasterStack} of modified
#'   objects according to the number of chosen datasets and (combinations of)
#'   operators.
#' @examples
#' input <- gtGeoms$grid$continuous
#'
#' # employ modification with merely one operator
#' binarised <- mdf_binarise(input, thresh = 40)
#' visualise(binarised)
#'
#' # combine several operators in an algorithm, optionally with arguments.
#' getPatches <- mdf_binarise(thresh = 40) %>%
#'   mdf_patches() %>%
#'   punch(name = "get_patches")
#'
#' patches <- modify(input, by = getPatches, sequential = TRUE)
#' visualise(patches)
#'
#' # To run separated sub-algorithms, use names for each operator to specify
#' # which elements should be computed sequentially.
#' getPatchNCats <- mdf_binarise(thresh = 40) %>%
#'   mdf_patches() %>%
#'   punch(name = "get_patches") %>%
#'   mdf_categorise(n = 5) %>%
#'   punch(name = "get_categories")
#' patchNCats <- modify(input, by = getPatchNCats, merge = TRUE)
#' visualise(patchNCats)
#'
#' # Create objects that are available later in the algorithm
#' getMedialAxis <- mdf_skeletonise(background = 0) %>%
#'   punch(name = "skeleton") %>%
#'   mdf_permute() %>%
#'   mdf_distance() %>%
#'   mdf_mask(mask = "skeleton") %>%
#'   punch(name = "medAxis")
#'
#' MAT <- modify(binarised, by = getMedialAxis, merge = TRUE)
#' visualise(MAT, trace = TRUE)

modify <- function(input = NULL, by = NULL, sequential = FALSE, merge = FALSE,
                   keepInput = FALSE){






  # # check arguments
  # isRaster <- testClass(input, "Raster")
  # isStackBrick <- testClass(input, "RasterStack")
  # isList <- testClass(input, "list")
  # if(!isRaster & !isStackBrick & !isList){
  #   stop("please provide a valid 'input'.")
  # }
  # if(isList){
  #   assertList(input, "RasterLayer")
  # }
  # assertList(by, types = "list", min.len = 1, any.missing = FALSE)
  # assertNames(names(by[[1]]), must.include = "operator")
  # assertLogical(sequential)
  # assertLogical(merge)
  # assertLogical(keepInput)
  #
  # # check which input we are dealing with and adapt if needs be
  # if(isList){
  #   objs <- unlist(input)
  #   objNames <- lapply(seq_along(input), function(x){
  #     names(input[[x]])
  #   })
  # } else if(isRaster){
  #   objNames <- "thisObject"
  #   objs <- setNames(list(input), objNames)
  # }
  #
  # # if the algos don't have names, assign generic ones and separate it into subalgos
  # if(is.null(names(by))){
  #   if(sequential){
  #     names(by) <- rep("algorithm", length(by))
  #   } else{
  #     names(by) <- paste0("algorithm", seq(length(by)))
  #   }
  # }
  # subAlgoNames <- unique(names(by))
  # if(keepInput){
  #   out <- setNames(list(input), "input")
  # } else{
  #   out <- list()
  # }
  #
  # for(j in seq_along(subAlgoNames)){
  #
  #   tempObjs <- objs
  #   tempAlgorithm <- by[which(names(by) == subAlgoNames[j])]
  #
  #   for(k in seq_along(tempAlgorithm)){
  #
  #     # set the correct mask raster
  #     if(tempAlgorithm[[k]]$operator == "rMask"){
  #       if(is.character(tempAlgorithm[[k]]$mask)){
  #         if(tempAlgorithm[[k]]$mask == "input"){
  #           theMask <- input
  #         } else if(any(names(out) == tempAlgorithm[[k]]$mask)){
  #           theMask <- out[[which(names(out) == tempAlgorithm[[k]]$mask)]]
  #         } else{
  #           theMask <- get(tempAlgorithm[[k]]$mask, envir = .GlobalEnv)
  #           if(!testClass(theMask, "geom")){
  #             if(dim(theMask)[3] > 1){
  #               theMask <- theMask[[1]]
  #             }
  #           }
  #         }
  #         tempAlgorithm[[k]]$mask <- theMask
  #       } else if(testClass(tempAlgorithm[[k]]$mask, "RasterLayer")){
  #         theMask <- tempAlgorithm[[k]]$mask
  #       } else{
  #         stop(paste0("please provide either the name of a layer in 'input' or a RasterLayer as 'mask' in operator ", k, " (", tempAlgorithm[[k]]$operator, ")."))
  #       }
  #     } else{
  #       theMask <- NULL
  #     }
  #
  #     # # set the correct overlay raster
  #     if(tempAlgorithm[[k]]$operator == "rBlend"){
  #       if(is.character(tempAlgorithm[[k]]$overlay)){
  #         if(any(names(out) == tempAlgorithm[[k]]$overlay)){
  #           theOverlay <- out[[which(names(out) == tempAlgorithm[[k]]$overlay)]]
  #         } else{
  #           theOverlay <- get(tempAlgorithm[[k]]$overlay, envir = .GlobalEnv)
  #           if(dim(theOverlay)[3] > 1){
  #             theOverlay <- theOverlay[[1]]
  #           }
  #         }
  #         tempAlgorithm[[k]]$overlay <- theOverlay
  #       } else if(testClass(tempAlgorithm[[k]]$overlay, "RasterLayer")){
  #         theOverlay <- tempAlgorithm[[k]]$overlay
  #       } else{
  #         stop(paste0("please provide either the name of a layer in 'input' or a RasterLayer as 'overlay' in operator ", k, " (", tempAlgorithm[[k]]$operator, ")."))
  #       }
  #     } else{
  #       theOverlay <- NULL
  #     }
  #
  #     # set the correct groups raster
  #     if(tempAlgorithm[[k]]$operator == "rSegregate"){
  #       if(is.character(tempAlgorithm[[k]]$by)){
  #         if(any(names(out) == tempAlgorithm[[k]]$by)){
  #           theGroups <- out[[which(names(out) == tempAlgorithm[[k]]$by)]]
  #         } else{
  #           theGroups <- get(tempAlgorithm[[k]]$by, envir = .GlobalEnv)
  #           if(dim(theGroups)[3] > 1){
  #             theGroups <- theGroups[[1]]
  #           }
  #         }
  #         tempAlgorithm[[k]]$by <- theGroups
  #       }
  #     } else{
  #       theGroups <- NULL
  #     }
  #
  #     # set a switch to reduce layers
  #     if(tempAlgorithm[[k]]$operator == "rReduce"){
  #       reduce <- TRUE
  #     } else{
  #       reduce <- FALSE
  #     }
  #
  #     for(i in seq_along(tempObjs)){
  #       thisObj <- tempObjs[[i]]
  #
  #       # if the object has more than one layer and reduce != TRUE, go
  #       # through each layer separately; if reduce == TRUE, treat the
  #       # multiple layer raster as one, because rReduce expects several
  #       # layers to combine them.
  #       if(dim(thisObj)[3] > 1 & !reduce){
  #
  #         for(l in 1:dim(thisObj)[3]){
  #
  #           # in case a mask has to be set and the mask contains several layers
  #           # (i.e. after segregating of the mask), assign the respective mask.
  #           if(!is.null(theMask)){
  #             if(testClass(theMask, "geom")){
  #               tempAlgorithm[[k]]$mask <- theMask
  #             } else{
  #               if(dim(theMask)[3] == dim(thisObj)[3]){
  #                 tempAlgorithm[[k]]$mask <- theMask[[l]]
  #               } else{
  #                 tempAlgorithm[[k]]$mask <- theMask[[1]]
  #               }
  #             }
  #           }
  #
  #           modifiedObj <- do.call(what = tempAlgorithm[[k]]$operator,
  #                                  args = c(obj = list(thisObj[[l]]), tempAlgorithm[[k]][-1]))
  #           thisObj[[l]] <- modifiedObj
  #
  #         }
  #         # newHistory <- paste0("in layers: ", modifiedObj@history[[length(modifiedObj@history)]])
  #         # thisObj@history <- c(thisObj@history, list(newHistory))
  #         tempObjs[[i]] <- thisObj
  #
  #       } else{
  #
  #         modifiedObj <- do.call(what = tempAlgorithm[[k]]$operator,
  #                                args = c(obj = list(thisObj), tempAlgorithm[[k]][-1]))
  #         tempObjs[[i]] <- modifiedObj
  #
  #       }
  #       tempObjs <- setNames(tempObjs, nm = objNames)
  #     }
  #
  #   }
  #   if(length(tempObjs) == 1){
  #     tempObjs <- tempObjs[[1]]
  #   }
  #   out <- c(out, setNames(object = list(tempObjs), nm = subAlgoNames[j]))
  #
  # }
  #
  # if(length(out) == 1){
  #   out <- out[[1]]
  # }
  #
  # if(merge & testList(out, min.len = 2)){
  #   out <- stack(out)
  # }
  #
  # return(out)
}

