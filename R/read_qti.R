#' Read Assessment Items in QTI XML
#'
#' These functions read QTI XML from character strings or files.
#' Only the single and multiple choice item types are currently supported.
#'
#' @param x String or connection to QTI XML file.
#' @param \dots further arguments passed to or from other functions.
#' @return \code{read_qti} returns an object of class
#' \dQuote{\code{qti_item}}, which contains the following elements,
#' if present in x:
#' \item{id}{item id, extracted from the identifier attribute}
#' \item{title}{item title, extracted from the title attribute}
#' \item{type}{item type, currently \dQuote{\code{single}} for single choice}
#' \item{prompt}{text for the item stem}
#' \item{options}{list of text for options}
#' \item{key}{vector of correct/incorrect as 0/1 for options}
#' \item{xml}{raw XML contents, stored as an object of class
#' \dQuote{xml_document}}
#'
#' @keywords methods
#' @examples
#'
#' # Read from character string
#' item <- read_qti(
#'   "<assessmentItem identifier='1584'
#'     title='Excellent Example Item'>
#'     <responseDeclaration identifier='RESPONSE' cardinality='single'
#'       baseType='identifier'>
#'       <correctResponse>
#'         <value>ChoiceB</value>
#'       </correctResponse>
#'     </responseDeclaration>
#'     <outcomeDeclaration identifier='SCORE' cardinality='single'
#'       baseType='float'>
#'       <defaultValue><value>0</value></defaultValue>
#'     </outcomeDeclaration>
#'     <itemBody>
#'       <prompt>
#'         <p>Is this a really good question?</p>
#'       </prompt>
#'       <choiceInteraction responseIdentifier='RESPONSE' shuffle='false'
#'         maxChoices='1'>
#'         <simpleChoice identifier='ChoiceA'>Yes</simpleChoice>
#'         <simpleChoice identifier='ChoiceB'>No</simpleChoice>
#'         <simpleChoice identifier='ChoiceC'>Maybe</simpleChoice>
#'       </choiceInteraction>
#'     </itemBody>
#'   </assessmentItem>")
#' item
#'
#' # Read from file
#'
#' @export
read_qti <- function(x, ...) {
  xml <- xml2::read_xml(x, ...)
  xml2::xml_ns_strip(xml)
  out <- qti_item(id = qti_item_id(xml),
    title = qti_item_title(xml),
    type = qti_item_type(xml),
    prompt = qti_prompt_text(xml),
    options = qti_choice_text(xml),
    key = qti_key(xml),
    xml = xml2::read_xml(x, ...))
  return(out)
}

qti_prompt_text <- function(x) {
  # Extract prompt text, removing choice interactions
  # Copy the itemBody node (or full object for now), remove
  # choiceInteraction and children, pull remaining text
  y <- xml2::read_xml(as.character(x)) # What's the best way to copy?
  xml2::xml_remove(xml2::xml_find_all(y, "//choiceInteraction"), free = TRUE)
  as.character(xml2::xml_contents(xml2::xml_find_all(y, "//itemBody")))
}

qti_choice_text <- function(x) {
  # Extract all choice text
  # This is breaking all html into separate elements, like new options
  # Need to loop through elements of choiceInteraction and collapse
  # contents of all children
  temp_choices <- xml2::xml_find_all(x, "//simpleChoice")
  out <- character(length = length(temp_choices))
  for (i in seq_along(out)) {
    out[i] <- paste(xml2::xml_contents(temp_choices[[i]]), collapse = "")
  }
  return(out)
}

qti_key <- function(x, xpath = "//correctResponse") {
  # Returns vector of 0/1 for keyed choices listed as values
  # under the correctResponse node
  key_id <- xml2::xml_text(xml2::xml_find_all(x, "//correctResponse//value"))
  choice_id <- unlist(lapply(xml2::xml_attrs(xml2::xml_find_all(x,
    "//simpleChoice")), "[[", "identifier"))
  return(ifelse(choice_id %in% key_id, 1, 0))
}

qti_item_id <- function(x) {
  xml2::xml_attr(xml2::xml_find_first(x, "//assessmentItem"), "identifier")
}

qti_item_title <- function(x) {
  xml2::xml_attr(xml2::xml_find_first(x, "//assessmentItem"), "title")
}

qti_item_type <- function(x) {
  item_type <- xml2::xml_attr(xml2::xml_find_first(x, "//outcomeDeclaration"),
    "cardinality")
  if (item_type %in% c("single", "multiple"))
    return("choice")
  else
    return(NULL)
}
