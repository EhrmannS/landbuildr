#' Generate a (neutral) gradient pattern
#'
#' This is largely still work in progress.
#'
#' A gradient can be understood in terms of the distance to an origin. With a
#' straight line as origin, one might end up with a edge gradient, or, in case
#' the line does not cross the the plot window, a planar gradient. Beware that
#' in case you provide an origin or a strict set of parameters (\code{...}), the
#' spatial pattern may not be neutral anymore!
#' @param mat [\code{matrix}]\cr object to which the spatial pattern is added.
#' @param origin [\code{RasterLayer} | \code{matrix}]\cr a binary object that
#'   serves as origin of the gradient; overrides the other arguments, if given.
#' @param type [\code{character(1)}]\cr the type of the gradient model. Either
#'   \code{"planar"} (default), \code{"point"}, \code{"line"}, \code{"polygon"},
#'   special cases thereof or \code{"random"}, to select one by chance.
#' @param ... parameters to create the origin.
#' @details In case \code{origin} is not given, \code{nlmGradient} constructs
#'   internally a binary origin from what is specified in \code{type} and
#'   \code{params} and then provides a distance matrix that has been scaled
#'   between 0 and 1 as gradient. Each geometry requires at least the number of
#'   vertices and their coordinates. In case the required arguments are not
#'   specified in \code{params = list(...)}, they will be set randomly.
#' @examples
#' # create a point gradient based on an origin
#' mat <- matrix(nrow = 100, ncol = 100, data = 0)
#' origin <- mat; origin[5000] <- 1
#' myPointGradient <- spmGradient(mat = mat, origin = origin)
#' visualise(raster = myPointGradient)
#'
#' # create a geometry object
#' #coords <- data.frame(x = c(0.4, 0.45, 0.7, 0.5),
#' #                     y = c(0.4, 0.4, 0.6, 0.7),
#' #                     id = 1)
#' #window <- data.frame(x = c(0, 1),
#' #                     y = c(0, 1))
#' #aGeom <- geomPolygon(anchor = coords, window = window, show = TRUE)
#'
#' # create gradient from the parameters of a geom
#' #NLMPolyGrad <- spmGradient(mat = mat, type = "polygon")
#'
#' # create a gradient from a random point pattern
#' #NLMPointGrad <- spmGradient(mat = mat, type = "point")
#'
#' # create a completely random gradient
#' #RandGrad <- spmGradient(mat = mat, type = "random")
#'
#' #visualise(raster = raster::stack(NLMPolyGrad, NLMPointGrad, RandGrad))

gnrt_gradient <- function(mat, origin = NULL, type = "planar", ...){

  # assertMatrix(mat)
  # assertCharacter(type, any.missing = FALSE, len = 1)
  # existsOrigin <- !is.null(origin)
  # if(existsOrigin){
  #   isRaster <- testClass(origin, "RasterLayer")
  #   isMatrix <- testClass(origin, "matrix")
  # }
  # types <- c("planar", "point", "line", "rectangle", "square", "polygon", "spline", "ellipse", "circle", "triangle", "hexagon")
  # if(type == "random"){
  #   type <- sample(types, 1)
  # } else{
  #   assertSubset(type, choices = types)
  # }
  #
  # # theArgs <- listArgs()
  # # newArgs <- theArgs[!names(theArgs) %in% c("mat", "origin", "type")]
  # # if(length(newArgs) == 0){
  # #   makeRandom <- TRUE
  # # } else{
  # #   assertNames(newArgs, subset.of = c("anchor", "vertices"))
  # #   makeRandom <- FALSE
  # # }
  #
  # if(!existsOrigin){
  #
  #   if(makeRandom){
  #     # theGeom <- gs_random(type = type, template = mat)
  #     # theMask <- as.matrix(gt_as_raster(theGeom))
  #   } else{
  #
  #     if(type == "planar"){
  #       # planar gradient is a gradient where a linear line through the plot window is layed and moved orthogonally until it touches the plot boundary in only one value
  #
  #
  #     } else{
  #       # put together the call from 'type' and 'theArgs' and 'newArgs'
  #
  #
  #     }
  #
  #   }
  # } else{
  #   if(isRaster){
  #     origin <- as.matrix(origin)
  #   }
  #   if(!isBinaryC(origin)){
  #     stop("please provide a binary raster or matrix in 'origin'.")
  #   } else{
  #     theMask <- origin
  #   }
  # }
  #
  # # create the distance object, thus the gradient
  # temp <- sqrt(meijsterDistanceC(theMask, method = "euclidean"))
  #
  # # scale between 0 and 1
  # # temp <- scaleMat(temp, c(0, 1))
  #
  # out <- raster(temp, xmn = 0, xmx = ncol(temp), ymn = 0, ymx = nrow(temp))
  #
  # # manage the bibliography entry
  # bib <- bibentry(bibtype = "incollection",
  #                 title = "A general algorithm for computing distance transforms in linear time",
  #                 volume = "18",
  #                 isbn = "978-0-306-47025-7",
  #                 booktitle = "Mathematical Morphology and its Applications to Image and Signal Processing",
  #                 publisher = "Springer",
  #                 author = c(
  #                   person("A", "Meijster"),
  #                   person(c("J", "B", "T", "M"), "Roerdink"),
  #                   person(c("W", "H"), "Hesselink")
  #                 ),
  #                 editor = c(
  #                   person("John", "Goutsias"),
  #                   person("Luc", "Vincent"),
  #                   person(c("Dan", "S"), "Bloomberg")
  #                 ),
  #                 year = "2000",
  #                 pages = "331--340"
  # )
  #
  # if(is.null(getOption("bibliography"))){
  #   options(bibliography = bib)
  # } else{
  #   currentBib <- getOption("bibliography")
  #   if(!bib%in%currentBib){
  #     options(bibliography = c(currentBib, bib))
  #   }
  # }
  #
  # names(out) <- paste0(type, "_gradient_nlm")
  # return(out)
}
