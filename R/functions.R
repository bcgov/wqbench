unzip_folder_correction <- function(unzip_location_sub, file_name, file_path) {
  # ensure folders unzip into the expected folder structure 
  # some files behaved differently during the unzip process
  dirs <- list.dirs(unzip_location_sub, full.names = FALSE, recursive = FALSE)
  
  if (any(dirs == file_name)) {
    files_from <- list.files(
      file.path(unzip_location_sub, file_name), 
      recursive = TRUE,
      full.names = TRUE
    )
    files_to_move <- file.path(
      file_path, 
      list.files(unzip_location_sub, recursive = TRUE)
    )
    move_location <- file.path(file_path, file_name)
    dir.create(
      file.path(move_location, "validation"), 
      recursive = TRUE
    )
    file.copy(
      to = files_to_move,
      from = files_from
    )
  } else {
    files_from <- list.files(
      file.path(unzip_location_sub), 
      recursive = TRUE,
      full.names = TRUE
    )
    move_location <- file.path(file_path, file_name)
    files_to_move <- file.path(
      move_location, 
      list.files(unzip_location_sub, recursive = TRUE)
    )
    dir.create(
      file.path(move_location, "validation"),
      recursive = TRUE
    )
    file.copy(
      to = files_to_move,
      from = files_from
    )
  }
  
  move_location
}
