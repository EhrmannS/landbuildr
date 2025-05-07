#' Make algorithms of geo-operators
#'
#' This function records sequences of geo-operators in objects of class
#' \code{card}.
#' @param card [card(1)][card]\cr a punched card of a sequence of geo-operators
#' @param label [character(1)][character]\cr name of the algorithm the new
#'   tentative operators should be part of.
#' @param equation [character(1)][character]\cr an equation that is based on the
#'   metrics defined in \code{card}, by calling their labels together with
#'   statistical functions and mathematical operators.
#' @param note [character(1)][character]\cr
#' @details The function \code{punch} is used in a pipeline of geo-operators
#'   that shall be used for particular operations. All operators that appear
#'   before punching are grouped and stored in an algorithm with the label
#'   provided to \code{punch}. This is used for both, weave commands, modify
#'   algorithms, and metrics.
#'
#'   When using the geo-operators of \code{loom} (e.g. any \emph{wv_},
#'   \emph{mdf_} and \emph{msr_} function) without providing an object to
#'   process (\code{obj = ...}), they return an invisible object of type
#'   \code{card} that records the arguments specified of the recent and all
#'   preceding geo-operators of the recent pipeline. When using this object in
#'   \code{punch}, the tentative operators are stored as an algorithm with the
#'   provided label. This allows permanent storage of algorithms that can be
#'   applied to any object of a suitable input type.
#'
#'   The argument \code{operator} is relevant only within geo-operators, as it
#'   manages appending a new geo-operator to an internal tentative list.
#' @examples
#' # create an algorithm for weaving a new pattern
#' # todo
#'
#' # create a card that determines patches
#' getPatches <- mdf_binarise(thresh = 40) %>%
#'   mdf_patches() %>%
#'   punch(label = "get_patches")
#'
#' # create a derived landscape metric
#' CPA <- msr_area(scale = "class", label = "ac") %>%
#'   msr_area(scale = "landscape", label = "al") %>%
#'   punch(equation = "ac / al * 100", label = "class proportional area",
#'         note = "this metric approaches 0 (100) when the respective class becomes increasingly rare (frequent)")
#'
#' # create a card with two algorithms where on builds on the other. Here,
#' # mdf_mask() takes the output of the first algorithm 'get_skeleton'
#' getMedialAxis <- mdf_skeletonise(background = 0) %>%
#'   punch(label = "get_skeleton") %>%
#'   mdf_permute() %>%
#'   mdf_distance() %>%
#'   mdf_mask(by = "get_skeleton") %>%
#'   punch(label = "get_medial-axis")
#'
#' # combine modify with measure cards
#' # some example where an input needs to be modified to measure one thing and
#' # needs to be unmodified to measure another thing for a ratio of the two
#' # things as landscape metric
#'
#' @importFrom checkmate assertClass assertList assertCharacter testClass
#' @importFrom rlang set_names
#' @importFrom methods new
#' @export

punch <- function(card = NULL, label = NULL, equation = NULL, note = NULL){

  assertClass(x = card, classes = "card")
  assertCharacter(x = label, len = 1)
  assertCharacter(x = equation, len = 1, null.ok = TRUE)
  assertCharacter(x = note, len = 1, null.ok = TRUE)

  theOperators <- card@operators
  algo <- card@modify
  theMetrics <- card@metrics
  derivedMetrics <- card@measure

  if(is.null(note)) note <- ""

  # if an equation is provided, write the metric into the card, if no equation is provided, write all operators into the card
  if(!is.null(equation)){

    temp <- list(equation = equation, note = note)
    derivedMetrics <- c(derivedMetrics, set_names(x = list(temp), nm = label))

  } else {

    temp <- c(theOperators, note = note)
    algo <- c(algo, set_names(x = list(temp), nm = label))
    theOperators <- list()

  }

  out <- new(Class = "card",
             operators = theOperators,
             modify = algo,
             metrics = theMetrics,
             measure = derivedMetrics)

  return(out)
}
