# qti_item class

qti_item <- function(id = NULL, title = NULL, type = NULL, prompt = NULL,
  options = NULL, key = NULL, xml = NULL) {
  structure(list(id = id, title = title, type = type, prompt = prompt,
    options = options, key = key, xml = xml), class = "qti_item")
}

#' @describeIn read_qti Print method
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

qti_build <- function(x, template) {
  if (x$type == "choice") {
    if (missing(template))
      template <- system.file("templates", "choice.xml", package = "qti")
    #choice_template <- xml2::read_xml(template)
    choice_template <- xml2::read_xml("/users/talbano/documents/code/qti/inst/templates/choice.xml")
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
