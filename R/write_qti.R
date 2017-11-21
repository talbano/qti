#' Write Assessment Items in QTI XML
#'
#' These functions write QTI XML to files. Only the single and
#' multiple choice item types are currently supported.
#'
#' @param x Object of class \dQuote{\code{qti_item}} to be written.
#' @param file Path to file or connection to write to.
#' @param \dots Further arguments passed to \code{xml_write}.
#' @return When \code{file} is not provided, x is written to the console
#' with \code{as.character}.
#' @keywords methods
#'
#' @export
write_qti <- function(x, file, ...) {
  if (class(x) == "xml_document")
    out <- x
  else if (class(x) == "qti_item") {
    if (is.null(x$xml))
      out <- qti_item_xml(x)
    else
      out <- x$xml
  }
  xml2::write_xml(out, file)
}
