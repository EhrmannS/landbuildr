#' Generate a neutral random pattern
#'
#' This is largely still work in progress.
#'
#' Random values can be applied to different patterns within the landscape
#' model.
#' @param mat [\code{matrix}]\cr object to which the spatial pattern is added.
#' @param pattern the pattern to which random values are assigned, either
#'   \code{"cell"}, \code{"rectangle"}, \code{"circle"}, \code{"voronoi"} or
#'   \code{"cluster"}; see Details.
#' @param spectrum in case the random values shall follow a particular
#'   distribution, such as white noise, pink noise, brownian noise, etc.
#' @param seed specify a seed for the random creation of numbers.
#' @details \itemize{ \item For \code{pattern = "cell"}, each cell of the
#'   resulting model is assigned a random value. \item For \code{pattern =
#'   "rectangle"}, \item For \code{pattern = "circle"}, \item For \code{pattern
#'   = "voronoi"}, Gaucherel (2008) \item For \code{pattern = "cluster"}, Saura
#'   & Martínez-Millán (2000) }
#' @references Gaucherel, C. (2008) Neutral models for polygonal landscapes with
#'   linear networks. Ecological Modelling, 219, 39 - 48. Saura, S.,
#'   Martínez-Millán, J. (2000) Landscape patterns simulation with a modified
#'   random clusters method. Landscape Ecology 15, 661–678.
#' @examples
#' mat <- matrix(nrow = 100, ncol = 100, data = 0)
#' myRandomPattern <- spmRandom(mat = mat)
#' visualise(raster = myRandomPattern)
#'

gnrt_random <- function(mat, pattern = "cell", spectrum = "white", seed = NULL){
  #
  #   # https://en.wikipedia.org/wiki/Centroidal_Voronoi_tessellation
  #
  #
  #   patterns <- c("cell", "rectangle", "circle", "voronoi", "cluster")
  #   assertSubset(pattern, choices = patterns)
  #
  #   if(!is.null(seed)){
  #     set.seed(seed)
  #   }
  #
  #   if(pattern == "cell"){
  #     # values <- runif(length(mat), min = 0, max = 1)
  #     mat[] <- values
  #   } else if(pattern == "rectangle"){
  #
  #   } else if(pattern == "circle"){
  #
  #   } else if(pattern == "voronoi"){
  #
  #   }
  #
  #   out <- raster(mat, xmn = 0, xmx = ncol(mat), ymn = 0, ymx = nrow(mat))
  #
  #   # # manage the bibliography entry (the different algos)
  #   # bib <- bibentry(bibtype = "",
  #   #                 title = "",
  #   #                 author = person(""),
  #   #                 year = ,
  #   # )
  #   #
  #   # if(is.null(getOption("bibliography"))){
  #   #   options(bibliography = bib)
  #   # } else{
  #   #   currentBib <- getOption("bibliography")
  #   #   if(!bib%in%currentBib){
  #   #     options(bibliography = c(currentBib, bib))
  #   #   }
  #   # }
  #
  #   return(out)

}
