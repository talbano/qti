#' Medical Biology Item Bank
#'
#' A list containing 611 medical biology items, each stored as an object of
#' class \link{qti_item}.
#'
#' Items include the following information, accessible via list elements:
#' \itemize{
#'  \item id: unique identifier.
#'  \item title: descriptive item title, in this case the learning outcome
#'  that the item was written to assess.
#'  \item type: item type. All MACRO items are multiple-choice.
#'  \item prompt: text for the item prompt or stem.
#'  \item options: vector of multiple-choice option text, one element per
#'  option.
#'  \item key: vector of scores per option.
#'  \item href: xml file name, generated automatically if not supplied.
#'  \item xml: item content in xml format.
#'}
#'
#' @format A list
#' @source \url{http://proola.org/}
"BIO"
