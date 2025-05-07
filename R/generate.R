#' Generate (neutral) spatial patterns
#'
#' This function is still largely work in process and very experimental!
#'
#' Spatial pattern models (SPMs), such as neutral landscape models (NLMs), are
#' useful to study an ecological response to a set of simulated spatial
#' patterns. Ideally, the resulting spatial patterns are strictly controlable
#' and in many situations they ought to be neutral (i.e. not influenced by any
#' process or bias). Here the term SPM is used because not all functions result
#' in neutral patterns.
#' @param model [\code{list(.)}]\cr algorithm in which the operators to generate
#'   spatial pattern models are specified. Each \code{operator} is a list
#'   iteself and includes the operator name and its arguments as sub-elements;
#'   see Examples.
#' @param dimensions [\code{integerish(2)}]\cr number of columns and rows the
#'   landscape model ought to have.
#' @details Many spatial pattern models have been suggested and NLMs probably
#'   constitute the most common use-case. However, often they are merely treated
#'   as "habitat models", where only the presence or abundance of habitat is
#'   simulated. Yet, "artificial patterns or processes" introduced by human
#'   disturbance and the influence of geological, atmospheric and ecological
#'   dynamics are also of interest. We try to cover these cases with ...
#'
#'   ... spatial patterns: \itemize{ \item \code{\link{spmGradient}}:
#'   Generate a (neutral) gradient pattern. \item \code{\link{spmRandom}}:
#'   Generate a random pattern. \item \code{\link{spmHeightmap}}: Generate a
#'   heightmap. \item \code{spmNoise}: Generate patterns based on different
#'   kinds of noise.}
#'
#'   ... ecological processes: \itemize{ \item \code{epmSucces}: Let
#'   objects in the spatial model success. \item \code{epmDiversify}: Let
#'   objects in the spatial model diversify. \item \code{epmPerforate}: Create
#'   gaps (e.g. clearcuts) into objects of the spatial model. \item
#'   \code{epmFragment}: Create fragmentation in the spatial model. \item
#'   \code{epmConnect}: Create connecting elements in the spatial model. }
#'
#'   Additionally you can use functions in other packages as operators, if they
#'   produce a 2D-lattice of spatial patterns; see for instance
#'   \href{https://github.com/ropensci/NLMR}{NLMR}.
#'
#' @examples
#' \dontrun{
#'
#' ## neutral landscape models
#' nlm <- list(list(operator = "spmRandom", seed = 12769),
#'             list(operator = "spmHeightmap", hurst = 0.4,
#'                  seed = 12769))
#'
#' myLandscape <- generate(model = nlm, dimensions = c(300, 300), to_env = TRUE)
#' }

generate <- function(model, dimensions){

  # check other generate operators
  #
  # 1. Gardner RH, O'Neill R V, Turner MG, Dale VH. 1989. Quantifying scale-dependent effects of animal movement with simple percolation models. Landscape Ecology 3:217 - 227.
  # 2. Gustafson, E.J. & Parker, G.R. (1992) Relationships between landcover proportion and indices of landscape spatial pattern. Landscape Ecology , 7, 101 - 110.
  # 3. Scherer, Cédric, et al. "Merging trait-based and individual-based modelling: An animal functional type approach to explore the responses of birds to climatic and land use changes in semi-arid African savannas." Ecological Modelling 326 (2016): 75-89.
  # 4. Saura, S. & Martínez-Millán, J. (2000) Landscape patterns simulation with a modified random clusters method. Landscape Ecology, 15, 661 – 678.
  # 5. Gaucherel, C. (2008) Neutral models for polygonal landscapes with linear networks. Ecological Modelling, 219, 39 - 48.
  # 6. Schwab, Dimitri, Martin Schlather, and Jürgen Potthoff. "A general class of mosaic random fields." arXiv preprint arXiv:1709.01441 (2017).
  # 7. Baddeley, Adrian, Ege Rubak, and Rolf Turner. Spatial point patterns: methodology and applications with R. CRC Press, 2015.
  # 8. Kéry & Royle (2016) Applied Hierarchical Modeling in Ecology Chapter 20
  # 9. Travis, J.M.J. & Dytham, C. (2004). A method for simulating patterns of habitat availability at static and dynamic range margins. Oikos , 104, 410–416.
  # 10. Martin Schlather, Alexander Malinowski, Peter J. Menck, Marco Oesting, Kirstin Strokorb (2015). nlm_fBm. Journal of Statistical Software, 63(8), 1-25. URL http://www.jstatsoft.org/v63/i08/.
  # 11. Keitt TH. 2000. Spectral representation of neutral landscapes. Landscape Ecology 15:479-493.





  # if(missing(dimensions)){
  #   stop("please specify the dimensions of the landscape model you would like to generate")
  # }
  # out <- model
  #
  # mat <- matrix(nrow = dimensions[1], ncol = dimensions[2], data = 0)
  #
  #
  #
  # return(out)
}
