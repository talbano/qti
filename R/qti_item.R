#' The qti_item class
#'
#' These functions build and manipulate qti_item objects, for analysis and
#' for reading and writing to QTI XML.
#'
#' @param id Item ID as an integer or string.
#' @param title Item title, as a string, with max 140 characters.
#' @param type Item type, only "choice" is currently supported.
#' @param prompt Stem text, as a single string.
#' @param options Option text, as vector of strings, one per option.
#' @param key Numeric vector of 0/1 response scores, one per option.
#' @param xml Optional XML representation of item, as character. If not
#' supplied, this is created automatically from the remaining arguments.
#' @return \code{qti_item} returns an object of class
#' \dQuote{\code{qti_item}}, which is a list containing named elements for
#' the item \code{id}, \code{title}, and \code{type}, as supplied via
#' arguments, the \code{prompt} and \code{options} text with formatting and
#' resources stripped, and the QTI XML representation of the item.
#'
#' @keywords methods
#' @examples
#'
#' item <- qti_item(
#'   id = 999,
#'   title = "Example Item",
#'   type = "choice",
#'   prompt = "What does this image tell you? <img src='life.png' />",
#'   options = c("Everything", "Something",
#'     "Nothing, but look at this code:<br/><pre>lm(life ~ R)</pre>"),
#'   key = c(1, 1, 0)
#' )
#' item
#'
#' @export
qti_item <- function(id = NULL, title = NULL, type = c("choice"),
  prompt = NULL, options = NULL, key = NULL, xml = NULL) {
  out <- list(id = id,
    title = substr(title, 1, 140),
    type = match.arg(type),
    prompt = prompt,
    options = options,
    key = key)
  if (is.null(xml))
    out$xml <- qti_item_xml(out)
  else
    out$xml <- xml
  out$prompt <- prep_item_text(paste(prompt, collapse = ""))
  out$options <- prep_item_text(options)
  class(out) <- "qti_item"
  return(out)
}

#' @describeIn qti_item Print method
#' @export
print.qti_item <- function(x, ...) {
  cat("\nqti_item\n")
  cat(x$title, "\n")
  cat(x$id, "\n\n")
  cat(x$prompt, "\n\n")
  cat(paste(x$options, collapse = "\n\n"), "\n\n")
}

clean_text <- function(x) {
  # Remove newlines, leading option letters, and leading and trailing space
  # Subs image, img, math, pre, and table tags with placeholders
  # Strip names
  out <- sapply(x, function(y) gsub("^\\s*\\w{1}\\.{1}", "", y))
  out <- sapply(out, function(y) gsub("\\s+", " ", y))
  out <- sapply(out, function(y) gsub("^[[:space:]]+|[[:space:]]+$", "", y))
  names(out) <- NULL
  return(out)
}

prep_item_text <- function(x, xpaths = c("//image", "//img",
  "//math", "//pre", "//table"), xtext = gsub("(//)(.+)", " [\\2] ", xpaths)) {
  # Replace any text contents of image, img, math, pre, and table with
  # placeholders
  # Remove all children, which also removes closing tag
  # Remove all attributes
  # Set text to be placeholder in xtext, which adds closing tag
  # All HTML is about to be stripped. Replace </br> with space to separate
  # sentences
  out <- character(length = length(x))
  for (i in seq_along(out)) {
    x[i] <- gsub("(<br/>)+", " ", x[i])
    temp_item <- xml2::read_html(sprintf("<p>%s</p>", x[i]))
    for (j in seq_along(xpaths)) {
      temp_nodes <- xml2::xml_find_all(temp_item, xpaths[j])
      xml2::xml_remove(xml2::xml_children(temp_nodes))
      xml2::xml_attrs(temp_nodes) <- NULL
      xml2::xml_text(temp_nodes) <- xtext[j]
    }
    out[i] <- clean_text(xml2::xml_text(temp_item))
  }
  return(out)
}

qti_item_xml <- function(x, template) {
  if (x$type == "choice") {
    if (missing(template))
      template <- system.file("templates", "choice.xml", package = "qti")
    choice_template <- xml2::read_xml(template)
    n_ops <- length(unlist(x$options))
    choice_labels <- paste0("Choice", LETTERS[1:n_ops])
    xml2::xml_attr(choice_template, "identifier") <- x$id
    xml2::xml_attr(choice_template, "title") <- x$title
    # Add prompt
    # Canvas QTI requires HTML formatting, at least with <p>
    temp_prompt_node <- xml2::xml_find_first(choice_template, "//d1:prompt")
    new_prompt_node <- xml2::read_xml(paste0("<prompt>", x$prompt,
      "</prompt>"))
    xml2::xml_replace(temp_prompt_node, new_prompt_node)
    # Add simpleChoice children under choiceInteraction
    # Add value children under correctResponse for each key
    temp_interaction_node <- xml2::xml_find_first(choice_template,
      "//d1:choiceInteraction")
    temp_correct_node <- xml2::xml_find_first(choice_template,
      "//d1:correctResponse")
    for (o in 1:n_ops) {
      temp_choice_node <- xml2::read_xml(paste0("<simpleChoice>",
        x$options[o], "</simpleChoice>"))
      xml2::xml_attr(temp_choice_node, "identifier") <- choice_labels[o]
      xml2::xml_add_child(temp_interaction_node, temp_choice_node)
      if (x$key[o] == 1) {
        temp_value_node <- xml2::read_xml(paste0("<value>", choice_labels[o],
          "</value>"))
        xml2::xml_add_child(temp_correct_node, temp_value_node)
        rm(temp_value_node)
      }
    }
    rm(temp_choice_node, temp_interaction_node, temp_prompt_node)
    return(choice_template)
  }
  else
    return(NULL)
}
