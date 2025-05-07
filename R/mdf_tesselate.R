#' Determine Voronoi polygons of patches in a raster
#'
#' The voronoi polygon outlines the "area of influence" of a patch. The boundary
#' of the voronoi polygon follows the middle line between the focal patch and
#' its neughbouring patches.
#' @param use.centroid logical
#' @param method the method, either euclidean, manhattan or ...
#' @param obj the spatial object
#' @export

mdf_tesselate <- function(use.centroid, method, obj){

  # REVISE

  # maybe call this instead rTesselate and define the argument 'shape' with
  # values "triangle", "square", "hexagon" "octagon" and "circle". The latter
  # case would be a voronoi transform.

  # For patches this should probably not be called voronoi, but something with
  # "buffer", as it would be based on where the boundaries of the buffers of
  # patches meet.
}
