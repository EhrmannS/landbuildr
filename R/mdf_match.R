#' Select cells based on a structuring element
#'
#' Transform a gridded object by setting all cells that do not match a
#' structuring element to \code{background}.
#' @param obj [gridded(1)][geom]\cr any gridded object to modify.
#' @param struct [struct(1)][struct]\cr the pattern based on which to select
#'   cells; see \code{\link{setStruct}} for details.
#' @param rotate [logical(1)][logical]\cr whether the kernel should be rotated
#'   for all possible rotations or should be used as is.
#' @param background [integerish(1)][integer]\cr the value any cell with value
#'   NA should have.
#' @param label [character(1)][character]\cr the label by which this operator
#'   shall be callable in an algorithm.
#' @return A grid geom of the same dimension as \code{obj} in which all cells
#'   that match with the structuring element(s) have the structuring element
#'   value and all other cells have the value \code{background}.
#' @details This is also known as the 'hit-or-miss'-transform.
#' @family operators to select a subset of cells
#' @examples
#' binarised <- mdf_binarise(obj = gtGeoms$grid$continuous, thresh = 30)
#'
#' # create structuring element
#' dot <- setStruct(custom = matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3))
#'
#' # identify singular pixels (that are also not connected on the corner)
#' mdf_match(obj = binarised, struct = dot) %>%
#'   visualise(binarised, .)
#' @export

mdf_match <- function(obj = NULL, struct = NULL, rotate = TRUE, background = NA,
                      label = NULL){

  out <- mdf_morph(obj = obj,
                   struct = struct,
                   blend = "equal",
                   merge = "sumNa",
                   rotate = rotate,
                   background = background)

  return(out)
}
