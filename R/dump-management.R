### =========================================================================
### matter dump management
### -------------------------------------------------------------------------
###

#' @include writeMatterArray.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NOTE: This just a temporary hack such that a new file is created every time
###       it is called
###

#' @export
getMatterDumpFile <- function(for.use = FALSE) {
  if (!S4Vectors::isTRUEorFALSE(for.use)) {
    stop("'for.use' must be TRUE or FALSE")
  }
  file <- try(.get_dump_specfile(), silent = TRUE)
  if (is(file, "try-error")) {
    file <- .get_dump_autofile(increment = for.use)
  }
  file
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 2 global internal counters: one for the dump files, one for the dump
### names
###
### The 2 counters are safe to use in the context of parallel execution e.g.
###
###   library(BiocParallel)
###   bplapply(1:5, function(i) .get_dump_files_global_counter(increment=TRUE))
###   bplapply(1:5, function(i) .get_dump_names_global_counter(increment=TRUE))
###

.get_dump_files_global_counter_filepath <- function() {
  file.path(tempdir(), "matterArray_dump_files_global_counter")
}

# NOTE: Called by .onLoad() hook (see zzz.R file).
init_matter_dump_files_global_counter <- function() {
  filepath <- .get_dump_files_global_counter_filepath()
  init_global_counter(filepath)
}

.get_dump_files_global_counter <- function(increment = FALSE) {
  filepath <- .get_dump_files_global_counter_filepath()
  get_global_counter(filepath, increment = increment)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Normalization (with basic checking) of a matter file path or dataset name
###

# Return the *absolute path* to the dump file.
# Has the side effect of creating the file as an empty matter file if it does
# not exist yet.
#' @importFrom S4Vectors isSingleString wmsg
normalize_dump_paths <- function(paths) {
  if (!is.character(paths) || paths == "")
    stop(wmsg("'paths' must be a non-empty string specifying the paths ",
              "to a new or existing matter file(s)"))
  if (!file.exists(paths)) {
    ok <- file.create(paths)
    if (!all(ok)) {
      stop(wmsg("failed to create file(s): '", paste0(paths[!ok],
                                                      collapse = "', '"),
                "'"), call. = FALSE)
    }
  }
  vapply(paths, tools::file_path_as_absolute, character(1L), USE.NAMES = FALSE)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Very low-level stuff used in this file only
###

.dump_settings_envir <- new.env(parent = emptyenv())

# Create directory 'dir' if it doesn't exist yet.
.set_dump_dir <- function(dir) {
  # NOTE: Even though file_path_as_absolute() will trim the trailing slashes,
  #       we need to do this early. Otherwise, checking for the existence of a
  #       file of the same name as the to-be-created directory will fail.
  if (nchar(dir) > 1L) {
    dir <- trim_trailing_slashes(dir)
  }
  if (!dir.exists(dir)) {
    if (file.exists(dir)) {
      stop(wmsg("\"", dir, "\" already exists and is a file, ",
                "not a directory"))
    }
    if (!suppressWarnings(dir.create(dir))) {
      stop("cannot create directory \"", dir, "\"")
    }
  }
  dir <- tools::file_path_as_absolute(dir)
  assign("dir", dir, envir = .dump_settings_envir)
}

.set_dump_autofiles_mode <- function() {
  suppressWarnings(rm(list = "specfile", envir = .dump_settings_envir))
}

# Create file as an empty HDF5 file if it doesn't exist yet.
.set_dump_specfile <- function(file) {
  file <- normalize_dump_paths(file)
  assign("specfile", file, envir = .dump_settings_envir)
}

# Return the user-specified file of the dump or an error if the user didn't
# specify a file.
.get_dump_specfile <- function() {
  get("specfile", envir = .dump_settings_envir)
}

# TODO: Is this needed?
# Return the user-specified name of the dump or an error if the user didn't
# specify a name.
.get_dump_specname <- function() {
  get("specname", envir = .dump_settings_envir)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### get/setMatterDumpDir()
###

#' @export
getMatterDumpDir <- function() {
  get("dir", envir = .dump_settings_envir)
}

# Create auto file as an empty matter file if it doesn't exist yet.
.get_dump_autofile <- function(increment = FALSE) {
  counter <- .get_dump_files_global_counter(increment = increment)
  file <- file.path(getMatterDumpDir(), sprintf("auto%05d.bin", counter))
  if (!file.exists(file)) {
    ok <- file.create(file)
    if (!ok) {
      stop(wmsg("failed to create file '", file, "'"), call. = FALSE)
    }
  }
  file
}

# NOTE: Called by .onLoad() hook (see zzz.R file).
#' @export
setMatterDumpDir <- function(dir) {
  if (missing(dir)) {
    dir <- file.path(tempdir(), "matterArray_dump")
  } else if (!isSingleString(dir) || dir == "") {
    stop(wmsg("'dir' must be a non-empty string specifying the path ",
              "to a new or existing directory"))
  }
  dir <- .set_dump_dir(dir)
  .set_dump_autofiles_mode()
  .get_dump_autofile()
  invisible(dir)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### set/getMatterDumpFile()
###

# Set the current matter dump file. Create it as an empty matter file if it
# doesn't exist yet.
#' @export
setMatterDumpFile <- function(file) {
  if (missing(file)) {
    .set_dump_autofiles_mode()
    file <- .get_dump_autofile()
  } else {
    if (!isSingleString(file) || file == "")
      stop("'file' must be a non-empty string")
    if (has_trailing_slash(file)) {
      setMatterDumpDir(file)
      file <- .get_dump_autofile()
    } else {
      file <- .set_dump_specfile(file)
    }
  }
}

# Return the *absolute path* to the dump file.
#' @export
getMatterDumpFile <- function(for.use = FALSE) {
  if (!S4Vectors::isTRUEorFALSE(for.use)) {
    stop("'for.use' must be TRUE or FALSE")
  }
  file <- try(.get_dump_specfile(), silent = TRUE)
  if (is(file, "try-error")) {
    file <- .get_dump_autofile(increment = for.use)
  }
  file
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Dump log
###

### Called by .onLoad() hook (see zzz.R file).
get_matter_dump_logfile <- function() {
  file.path(tempdir(), "matterArray_dump_log")
}

.get_dataset_creation_global_counter_filepath <- function() {
  file.path(tempdir(), "matterArray_dataset_creation_global_counter")
}

### Called by .onLoad() hook (see zzz.R file).
init_matter_dataset_creation_global_counter <- function() {
  filepath <- .get_dataset_creation_global_counter_filepath()
  init_global_counter(filepath)
}

.get_dataset_creation_global_counter <- function(increment = FALSE) {
  filepath <- .get_dataset_creation_global_counter_filepath()
  get_global_counter(filepath, increment = increment)
}

# NOTE: Use a lock mechanism so is safe to use in the context of parallel
#       execution.
#' @export
appendDatasetCreationToMatterDumpLog <- function(paths, dim, datamode) {
  logfile <- get_matter_dump_logfile()
  locked_path <- lock_file(logfile)
  on.exit(unlock_file(logfile))
  counter <- .get_dataset_creation_global_counter(increment = TRUE)
  dims <- paste0(dim, collapse = "x")
  for (path in paths) {
    cat(as.character(Sys.time()), counter,
        path, dims, as.character(datamode),
        sep = "\t", file = locked_path, append = TRUE)
    cat("\n", file = locked_path, append = TRUE)
  }
}

#' @export
showMatterDumpLog <- function() {
  COLNAMES <- c("time", "counter",
                "file", "dims", "datamode")
  # NOTE: The number of lines in the log file is the current value of the
  #       dataset creation counter minus one.
  counter <- .get_dataset_creation_global_counter()
  if (counter == 1L) {
    dump_log <- data.frame(time = character(0),
                           counter = integer(0),
                           file = character(0),
                           dims = character(0),
                           datamode = character(0),
                           stringsAsFactors = FALSE)
  } else {
    logfile <- get_matter_dump_logfile()
    locked_path <- lock_file(logfile)
    on.exit(unlock_file(logfile))
    dump_log <- utils::read.table(locked_path,
                                  sep = "\t", stringsAsFactors = FALSE)
    colnames(dump_log) <- COLNAMES
    fmt <- "[%s] #%d In file '%s': creation of dataset '%s' (%s:%s, chunk_dims=%s, level=%d)"
    message(paste0(sprintf(fmt,
                           dump_log$time, dump_log$counter,
                           dump_log$file, dump_log$dims,
                           dump_log$datamde),
                   "\n"),
            appendLF = FALSE)
  }
  invisible(dump_log)
}
