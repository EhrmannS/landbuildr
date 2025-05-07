#' Generate a heightmap
#'
#' This is largely still work in progress.
#'
#' A heightmap is the two dimensional representation of a three dimensional
#' surface, where the value of the cells represents the height in the three
#' dimensional surface (a simulated digitial elevation model).
#' @param mat [\code{matrix}]\cr object to which the spatial pattern is added.
#' @param hurst [\code{numeric(1)}]\cr the Hurst exponent (fBm) or its
#'   equivalent, the roughness factor (DSa). Bounded between 0 and 1.
#' @param type [\code{character(1)}]\cr the model according to which the pattern
#'   is generated, either \code{"diamondSquare"} (default), \code{"brownian"} or
#'   \code{"gaussian"}.
#' @param startDev bla (for now)
#' @param seed bla
#' @details \describe{ \item{Diamond Square algorithm}{The algorithm was
#'   originally proposed by Fournier et al. (1982), but has been enhanced since
#'   then. It assigns values to an empty array (one or two dimensional).
#'   \emph{"The algorithm recursively subdivides the interval and generates a
#'   scalar value at the midpoint which is proportional to the current standard
#'   deviation times the scale or "roughness" factor (h). [...] h is equivalent
#'   to the Hurst exponent [see fractional brownian motion] and can take values
#'   between 0 and 1."} (Fournier et al, 1982). \cr\cr The implementation here
#'   computes values that are at the boundary of the two dimensional array as
#'   average from its three only neighbours and not from the one dimensional
#'   version of the algorithm (Fournier et al., 1982).}
#'   \item{Fractional brownian motion}{} \item{Gaussian random field}{} }
#' @references Fournier A, Fussell D, Carpenter L. Computer rendering of
#'   stochastic models. Communications of the ACM. 1982;25:371–384
#'
#'   Palmer MW. The coexistence of species in fractal landscapes. The American
#'   Naturalist. 1992;139:375–397
#'
#'   Travis JMJ, Dytham C. A method for simulating patterns of habitat
#'   availability at static and dynamic range margins. Oikos. 2004;104:410–416
#'
#' @examples
#' mat <- matrix(nrow = 100, ncol = 100, data = 0)
#' myHeightmap <- spmHeightmap(mat = mat, hurst = 0.4, seed = 13531)
#' visualise(myHeightmap)
#'

gnrt_heightmap <- function(mat, type = "diamondSquare", hurst = NULL, startDev = 1,
                         seed = NULL){

  # # https://en.wikipedia.org/wiki/Brownian_surface
  # # https://en.wikipedia.org/wiki/Random_walk
  #
  # assertMatrix(mat)
  # assertCharacter(type, any.missing = FALSE, len = 1)
  # assertNumeric(hurst, any.missing = FALSE, len = 1, lower = 0, upper = 1)
  # assertIntegerish(seed, any.missing = FALSE, len = 1, null.ok = TRUE)
  #
  # if(type == "diamondSquare"){
  #   # manage the seed here.
  #   if(!is.null(seed)){
  #     if(length(seed) > 1){
  #       cornerSeed <- rep_len(seed, 4)
  #     } else{
  #       set.seed(seed)
  #       # cornerSeed <- runif(4)
  #     }
  #   } else{
  #     # cornerSeed <- runif(4)
  #   }
  #
  #   # manage the initial array
  #   mat_dim <- max(dim(mat))
  #   level <- ceiling(log(mat_dim)/log(2))
  #   ext <- 2**level + 1
  #   targetMat <- matrix(data = 0, nrow = ext, ncol = ext)
  #   stepSize <- 2**(level:1)
  #
  #   targetMat[1, 1] <- cornerSeed[1]
  #   targetMat[1, ext] <- cornerSeed[2]
  #   targetMat[ext, 1] <- cornerSeed[3]
  #   targetMat[ext, ext] <- cornerSeed[4]
  #
  #   # this algorithm still needs a proper handling of the standard deviation.
  #   # The way it is now is not how it's supposed to be. (see link in cpp file)
  #   mat_out <- diamondSquareC(mat = targetMat, stepSize = stepSize, roughness = hurst, startDev = startDev)
  #
  #   bib <- bibentry(bibtype = "Article",
  #                   author = c(person(given = "A", family = "Fournier"),
  #                              person(given = "D", family = "Fussell"),
  #                              person(given = "L", family = "Carpenter")),
  #                   title = "Computer rendering of stochastic models",
  #                   pages = "371-384",
  #                   year = 1982,
  #                   journal = "Communications of the ACM",
  #                   volume = 25,
  #                   issue = 6
  #   )
  #
  # } else if(type == "brownian"){
  #   stop("type = 'brownian' is not yet supported.")
  #   #
  # #   bib <- bibentry(bibtype = "Article",
  # #                   author = c(person(given = "J M J", family = "Travis"),
  # #                              person(given = "C", family = "Dytham")),
  # #                   title = "A method for simulating patterns of habitat availability at static and dynamic range margins",
  # #                   pages = "410-416",
  # #                   year = 2004,
  # #                   journal = "Oikos",
  # #                   volume = 104,
  # #                   issue = 2
  # #                   )
  # #
  # } else if(type == "gaussian"){
  #   stop("type = 'gaussian' is not yet supported.")
  # }
  #
  # # mat_out <- scaleMat(mat_out, c(0, 1))
  # obj <- raster(mat_out, xmn = 0, xmx = ext, ymn = 0, ymx = ext)
  #
  # # # manage the bibliography entry (diamong-square algo or other)
  # if(is.null(getOption("bibliography"))){
  #   options(bibliography = bib)
  # } else{
  #   currentBib <- getOption("bibliography")
  #   if(!bib%in%currentBib){
  #     options(bibliography = c(currentBib, bib))
  #   }
  # }
  #
  # names(obj) <- paste0("heightmap_", type)
  # return(obj)

}
