#' Write Assessment Items in QTI XML
#'
#' These functions write QTI XML to files. Only the single and
#' multiple choice item types are currently supported.
#'
#' @param x Object of class \dQuote{\code{qti_item}} or
#' \dQuote{\code{xml_document}} to be written.
#' @param file Path to file or connection to write to.
#' @param \dots Further arguments passed to \code{xml_write}.
#'
#' @keywords methods
#'
#' @export
write_qti <- function(x, file) {
  if (class(x) == "xml_document")
    xml2::write_xml(x, file)
  else if (class(x) == "qti_item") {
    if (is.null(x$xml))
      x$xml <- qti_build(x)
    xml2::write_xml(x$xml, file)
  } else
    stop("'x' must be class 'qti_item' or 'xml_document'")
}
