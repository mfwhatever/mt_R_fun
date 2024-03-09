# add a datestamp to a filename string

# Version: 20240226_0911

add_datestamp <- function (
    # a filename (with or without an extension)
    filename,
    path = "",
    separator = "_",
    date_format = "%Y%m%d_%H%M"
) {
  
  file_extension <- tools::file_ext(filename)
  filename_sans <- tools::file_path_sans_ext(filename)
  datestamp <- paste0(
   ifelse(path != "",
     paste0(
       gsub("/$|\\\\$","",path),
       "/"
     ),
     ""),
   filename_sans,
   separator,
   format(Sys.time(), format = date_format),
   ifelse(file_extension != "", ".", ""),
   file_extension
  )
  
  return(datestamp)
}