#' Write QTI XML Manifest and Assessment Package
#'
#' These functions write QTI XML manifests to files, and build assessment
#' packages, with items as input.
#'
#' @param x \code{qti_manifest} object to be written, or combined with item
#' objects in an assessment package.
#' @param file Path to file or connection to write to, with default
#' \code{"imsmanifest.xml"}.
#' @param \dots Further arguments passed to \code{xml_write}.
#' @return When \code{file} is not provided, the manifest in x is written to the
#' console with \code{as.character}. When building an assessment package, files are
#' written to the current working directory.
#' @keywords methods
#'
#' @export
write_qti_manifest <- function(x, file = "imsmanifest.xml", ...) {
  if (class(x) == "xml_document")
    out <- x
  else if (class(x) == "qti_manifest") {
    if (is.null(x$xml))
      out <- qti_manifest_xml(x)
    else
      out <- x$xml
  }
  xml2::write_xml(out, file)
}

#' @rdname write_qti_manifest
#' @export
write_qti_package <- function(x, file) {
  if (class(x) != "qti_manifest")
    stop("'x' must be a qti_manifest object")
  cat("\nGenerating QTI manifest\n\n")
  write_qti_manifest(x, file)
  for (i in x$items) {
    cat("\nGenerating QTI for item ", i$id, "\n\n")
    write_qti(i, file = i$href) # Use function for getting item names?
  }
}

get_manifest_images <- function(x, pattern = "\\.png|\\.jpg|\\.jpeg",
  from, to, overwrite = FALSE) {
  # Find images linked in manifest x and copy them from from to to
  # x path to manifest file
  # pattern regular expression for desired image extension(s)
  # from is current directory file path, excluding image names, ending with /
  # to is new directory, excluding image names, ending with /
  man <- xml2::read_xml(x)
  temp_href <- xml2::xml_attr(xml2::xml_find_all(man, "//file"), "href")
  image_names <- basename(grep(pattern, temp_href, value = TRUE))
  if (!all(image_names %in% dir(from)))
    stop("Some images in manifest 'x' not found in 'from' directory")
  file_count <- file.copy(paste0(from, image_names), paste0(to, image_names),
    overwrite = overwrite)
  cat("\nTotal images copied: ", sum(file_count), "\n\n")
}
