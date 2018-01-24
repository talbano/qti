#' Write QTI XML Manifest and Assessment Package
#'
#' These functions write QTI XML manifests to files, and build assessment
#' packages, with items as input.
#'
#' @param x \code{qti_manifest} object to be written, or a list of
#' \code{qti_item} objects to be combined in an assessment package.
#' @param file Path to file or connection to write to.
#' @param \dots Further arguments passed to \code{xml_write}.
#' @return When \code{file} is not provided, x is written to the console
#' with \code{as.character}.
#' @keywords methods
#'
#' @export
write_qti_manifest <- function(x, file, ...) {
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

write_qti_package <- function(x, file) {
  # Write QTI XML files for items and manifest
  man <- qti_manifest(x) # Guarantees filenames will match
  write_qti_manifest(man, file)
  if (class(x) == "qti_item") x <- list(x)
  for (i in x) {
    cat("\nGenerating QTI for item ", i$id, "\n\n")
    write_qti(i, file = ) # Use function for getting item names?
  }
}

# Function for getting item names?
get_manifest_items <- function(x) {
  # Extract the href for resources with type imsqti
  nodes <- xml2::xml_find_all(x$xml, "//resource")
  has_items <- grepl("imsqti", xml2::xml_attrs(nodes))
  if (any(has_items)) {
    out <- xml2::xml_attr(nodes[has_items], "href")
  }
  else
    stop("Manifest 'x' does not contain resources with type imsqti")

  rm(nodes)
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
  image_names <- basename(grep(pattern, temp_href, value = T))
  if (!all(image_names %in% dir(from)))
    stop("Some images in manifest 'x' not found in 'from' directory")
  file_count <- file.copy(paste0(from, image_names), paste0(to, image_names),
    overwrite = overwrite)
  cat("\nTotal images copied: ", sum(file_count), "\n\n")
}
