# This function is copied from the 'tools' R package
file_path_no_ext <- function (x, compression = FALSE) 
{
  if (compression) 
    x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}
