.onLoad <- function(libname, pkgname) {
  init_matter_dump_files_global_counter()
  setMatterDumpDir()
  file.create(get_matter_dump_logfile())
  init_matter_dataset_creation_global_counter()
}
