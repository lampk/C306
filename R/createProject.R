################################################################################
# to create project directory
# by: Lam PK
# version: 23 July 2015
################################################################################

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

