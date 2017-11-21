write_qti_package <- function(x) {
  # Write QTI XML files for items and manifest
  write_qti_manifest(x)
  if (class(x) == "qti_item") x <- list(x)
  for (i in x) {
    cat("\nGenerating QTI for item ", i$id, "\n\n")
    write_qti(i)
  }
}

get_manifest_images <- function(x, pattern = "\\.image",
  from, to, overwrite = FALSE) {
  # Find images linked in manifest x and copy them from from to to
  # x path to manifest file
  # pattern regular expression for desired image extension(s)
  # from is current directory file path, excluding image names, ending with /
  # to is new directory, excluding image names, ending with /
  man <- xml2::read_xml(x)
  temp_resources_node <- xml2::xml_find_first(man, "//d1:resources")
  temp_href <- xml2::xml_find_all(temp_resources_node, "//@href")
  temp_paths <- gsub("([^'\"]*['|\"])([^'\"]+)(['|\"])", "\\2",
    as.character(temp_href))
  image_names <- basename(grep(pattern, temp_paths, value = T))
  if (!all(image_names %in% dir(from)))
    stop("Some images in manifest 'x' not found in 'from' directory")
  file_count <- file.copy(paste0(from, image_names), paste0(to, image_names),
    overwrite = overwrite)
  cat("\nTotal images copied: ", sum(file_count), "\n\n")
}
