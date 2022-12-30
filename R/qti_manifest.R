#' The qti_manifest class
#'
#' These functions build and manipulate qti_manifest objects, for analysis
#' and for reading and writing to QTI XML.
#'
#' @param x One or more items as \dQuote{\code{qti_item}} objects.
#' @param id Optional identifier.
#' @param title Assessment title, as a string, with max 140 characters.
#' @param xml Optional XML representation of manifest, as character. If not
#' supplied, this is created automatically from the remaining arguments.
#' @return \code{qti_manifest} returns an object of class
#' \dQuote{\code{qti_manifest}}, which is a list containing named elements
#' for the assessment \code{id} and \code{title}, as supplied via
#' arguments, the items in \code{x} as a list of \dQuote{\code{qti_item}},
#' and the QTI XML representation of the manifest.
#'
#' @keywords methods
#' @examples
#'
#' item <- qti_item(
#'   id = "item-1",
#'   title = "Example Item",
#'   type = "choice",
#'   prompt = "What does this image tell you? <img src='life.png' />",
#'   options = c("Everything", "Something",
#'     "Nothing, but look at this code:<br/><pre>lm(life ~ R)</pre>"),
#'   key = c(1, 1, 0)
#' )
#' man <- qti_manifest(
#'   x = item,
#'   id = "exam-1",
#'   title = "Example Exam"
#' )
#'
#' @export
qti_manifest <- function(x, id = NULL, title = NULL, xml = NULL) {
  out <- list(id = id,
    title = substr(title, 1, 140))
  if (is.null(xml))
    out$xml <- qti_manifest_xml(x)
  else
    out$xml <- xml
  out$items <- x
  class(out) <- "qti_manifest"
  return(out)
}

#' @export
print.qti_manifest <- function(x, ...) {
  cat("\nqti_manifest\n")
  cat("Title:", x$title, "\n")
  cat("ID:", x$id, "\n\n")
  cat("Item href:\n")
  print(unlist(lapply(x$items, "[[", "href")))
}

img_src <- function(x) {
  xml2::xml_attr(xml2::xml_find_all(x$xml, "//img"), "src")
}

#qti_item_filename <- function(x)

resource_node <- function(x, identifier = x$id,
  type = "imsqti_item_xmlv2p2") {
  # Build resource node string for a given item, to be inserted in
  # manifest file
  # x qti_item object
  # Get all the images at once, across prompt and options
  node <- sprintf("<resource identifier='%s' type='%s' href='%s'>",
    identifier, type, x$href)
  node <- paste0(node, sprintf("<file href='%s'/>", x$href))
  for (i in img_src(x))
    node <- paste0(node, sprintf("<file href='%s'/>", i))
  node <- paste0(node, "</resource>")
  return(node)
}

qti_manifest_xml <- function(x, template) {
  # Build QTI XML manifest as xml_document object
  # x qti_item or list of qti_item objects
  if (missing(template))
    template <- system.file("templates", "imsmanifest.xml", package = "qti")
  man_template <- xml2::read_xml(template)
  temp_resources_node <- xml2::xml_find_first(man_template,
    "//d1:resources")
  # Add resource as children under resources, one per item and image
  if (class(x) == "qti_item") x <- list(x)
  for (i in x) {
    temp_item_node <- xml2::read_xml(resource_node(i))
    xml2::xml_add_child(temp_resources_node, temp_item_node)
  }
  rm(temp_resources_node)
  return(man_template)
}
