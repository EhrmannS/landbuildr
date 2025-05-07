#' The \code{struct} class (S4) and its methods
#'
#' A structuring element is any 2D array with an odd number of cells in each
#' dimension and a focal cell in the middle. Each cell is compared against a
#' gridded object to probe the local pattern of the gridded object. Typically a
#' conclusion is then drawn based on how the structuring element matches the
#' local pattern and this conclusion is stored in the value of the focal cell.
#'
#' The cells of a structuring element can have four values: NA (indifferent), 0,
#' 1, and any value > 1. Gridded objects are either binary or non-binary
#' (greyscale). \itemize{ \item For binary objects a structuring element must
#' only contain values 0, 1 and NA. Cells with value 0 and 1 must match
#' accurately in the target and cells with value NA will be ignored, i.e. "it
#' does not matter what value there is". \item For a non-binary object the value
#' 0 does not have any meaning and every 0 is turned into an NA....}
#' @slot pattern [matrix(.)][matrix]\cr a pattern that is the backbone of a
#'   structuring element.
#' @slot rotate [logical(1)][logical]\cr whether or not the pattern shall be
#'   applied in all of its' rotations or whether it should only compared in its
#'   current rotation.
#' @slot background [numeric(1)][numeric]\cr the value of background cells, i.e.,
#'   those that shall be ignored.

struct <- setClass(Class = "struct",
                   slots = c(pattern = "matrix",
                             rotate = "logical",
                             background = "numeric"
                   )
)

#' Print the structuring element
#'
#' @param object [struct][struct]\cr the structuring element to print.
#' @importFrom crayon yellow

setMethod(f = "show",
          signature = "struct",
          definition = function(object){

            ptrn <- object@pattern
            bg <- object@background
            rot <- object@rotate
            dims <- dim(ptrn)

            df <- data.frame(id = c(0, 1, bg), val = c("\u25ef", "\u25c9", "\u25cc"), stringsAsFactors = FALSE)

            cat(paste0("background: ", bg, "; rotate: ", ifelse(rot, "yes", "no"), "\n\n"))

            for(i in 1:dims[1]){
              temp <- as.vector(ptrn[i,])
              temp <- df$val[match(x = temp, df$id)]
              cat("   ", temp, "\n")
            }

          }
)

#' Make a structuring element
#'
#' @param type [character(1)][character]\cr the shape of the structuring element,
#'   either 'disc', 'rectangle', 'cross' or ...
#' @param width [integerish(1)][integer]\cr the width of the structuring element
#'   in number of cells.
#' @param height [integerish(1)][integer]\cr the height of the structuring element
#'   in number of cells.
#' @param rotate [logical(1)][logical]\cr
#' @param background [integerish(1)][integer]\cr the value any cell with value NA
#'   should have.
#' @param custom [matrix(.)][matrix]\cr if none of the above \code{type} shall be
#'   used, give a matrix of the respective integer values here.
#' @export

setStruct <- function(type = "disc", width = 3, height = 3, rotate = FALSE,
                      background = NA, custom = NULL){

  if(is.na(background)) background <- NA_integer_

  if(is.null(custom)){
    temp <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), nrow = height, ncol = width)
  } else {
    temp <- custom
  }

  out <- new(Class = "struct",
             pattern = temp,
             rotate = rotate,
             background = background)

  return(out)

}

