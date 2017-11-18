#' Read Assessment Items in QTI XML
#'
#' These functions read QTI XML from strings or files. Only the single and
#' multiple choice item types are currently supported.
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
#' @author Tony Albano \email{tony.d.albano@@gmail.com}
#'
#' @keywords methods
#' @examples
#'
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
#' @export
read_qti <- function(x, ...) {
  xml <- xml2::read_xml(x, ...)
  xml2::xml_ns_strip(xml)
  qti_clean_nodes(xml)
  out <- qti_item(id = qti_item_id(xml),
    title = qti_item_title(xml),
    type = qti_item_type(xml),
    prompt = qti_clean_text(qti_prompt_text(xml)),
    options = qti_clean_text(qti_choice_text(xml)),
    key = qti_key(xml),
    xml = xml2::read_xml(x, ...))
  return(out)
}

qti_item <- function(id = NULL, title = NULL, type = NULL, prompt = NULL,
  options = NULL, key = NULL, xml = NULL) {
  structure(list(id = id, title = title, type = type, prompt = prompt,
    options = options, key = key, xml = xml), class = "qti_item")
}

#' @export
print.qti_item <- function(x, ...) {
  cat("\nqti_item\n")
  cat(x$title, "\n")
  cat(x$id, "\n\n")
  cat(x$prompt, "\n\n")
  cat(paste(x$options, collapse = "\n\n"), "\n\n")
}

qti_clean_nodes <- function(x, xpaths = c("//image", "//img",
  "//math", "//pre", "//table"), xtext = gsub("(//)(.+)", " [\\2] ", xpaths)) {
  # Replace any text contents of image, img, math, pre, and table with
  # placeholders
  # Remove all children, which also removes closing tag
  # Remove all attributes
  # Set text to be placeholder in xtext, which adds closing tag
  # Modifying by reference
  for (i in seq_along(xpaths)) {
    temp_nodes <- xml2::xml_find_all(x, xpaths[i])
    if (length(temp_nodes)) {
      xml2::xml_remove(xml2::xml_children(temp_nodes))
      xml2::xml_attrs(temp_nodes) <- NULL
      xml2::xml_text(temp_nodes) <- xtext[i]
    }
    rm(temp_nodes) # Does gc do this for us?
  }
}

qti_prompt_text <- function(x, xpath = "//itemBody",
  rmpath = "//choiceInteraction") {
  # Extract prompt text, removing choice interactions
  # Copy the itemBody node (or full object for now), remove
  # choiceInteraction and children, pull remaining text
  y <- xml2::read_xml(as.character(x)) # What's the best way to copy?
  xml2::xml_remove(xml2::xml_find_all(y, rmpath), free = TRUE)
  xml2::xml_text(xml2::xml_find_all(y, xpath))
}

qti_choice_text <- function(x) {
  # Extract all choice text
  xml2::xml_text(xml2::xml_find_all(x, "//simpleChoice"))
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

qti_clean_text <- function(x) {
  # Remove newlines, leading option letters, and leading and trailing space
  rm <- "^\\s*\\w{1}\\.{1}"
  out <- sapply(x, function(y) gsub(rm, "", y))
  out <- sapply(out, function(y) gsub("\\s+", " ", y))
  out <- sapply(out, function(y) gsub("^[[:space:]]+|[[:space:]]+$", "", y))
  names(out) <- NULL
  return(out)
}