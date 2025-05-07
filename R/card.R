#' The \code{card} class (S4) and its methods
#'
#' A \code{card} stores a sequence of geo-operators.
#' @slot operators [named list(.)][list]\cr geo-operators that modify an input.
#' @slot group [named list(.)][list]\cr temporary geo-operators that are
#'   supposed to be grouped when calling \code{\link{punch}}.

card <- setClass(Class = "card",
                 slots = c(operators = "list",
                           modify = "list",
                           metrics = "list",
                           measure = "list"
                 )
)

#' Print the \code{card}
#'
#' @param object [card][card]\cr the card to print.
#' @importFrom crayon yellow

setMethod(f = "show",
          signature = "card",
          definition = function(object){
            operators <- object@operators
            group <- object@group

            modNames <- names(operators)
            nAlgo <- sapply(seq_along(operators), function(x){
              nchar(modNames[x])
            })
            nOperators <- sum(lengths(operators))

            for(i in seq_along(modNames)){
              group <- operators[[i]]
              tempNames <- names(group)
              for(j in seq_along(tempNames)){
                if(j == 1){
                  cat(paste0(yellow(modNames[i]),
                             paste0(rep(" ", max(nAlgo) - nAlgo[i] + 3), collapse = ""),
                             tempNames[j],
                             "\n"))
                } else {
                  cat(paste0(paste0(rep(" ", max(nAlgo) + 3), collapse = ""),
                             paste0(" -> ", tempNames[j]),
                             "\n"))
                }
              }

            }

          }
)
