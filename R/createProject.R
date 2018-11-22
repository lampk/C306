#' Create project's folder based on pre-defined folder structure
#'
#' @description A generic function to create a project's folder based on pre-defined folder structure.
#'
#' @param dir a character specifies where to create the project's folder.
#' @param structure a character specifies the pre-defined folder structure to be used.
#'
#' @return A message to confirm that the project's folder is created at the designed place.
#' @export
createProject <- function(dir, structure = c("OUCRU_RCT_wInterim")){
  ## to create project directory
  ## initial version: 23 July 2015

  if (length(structure) > 1) {structure <- "OUCRU_RCT_wInterim"}

  ## create destination folder
  dir.create(dir)

  ## copy folder structure into destination folder
  file.copy(from = file.path(system.file(package = "C306"), "structure", structure, "."),
            to = dir, recursive = TRUE)
  file.rename(from = file.path(dir, "OUCRU_RCT_wInterim.Rproj"), to = file.path(dir, paste0(basename(dir), ".Rproj")))

  # inform
  cat(paste("Project", basename(dir), "was created in", dirname(dir), "\n", sep = " "))
}

