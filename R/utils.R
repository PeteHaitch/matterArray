### =========================================================================
### Some low-level utilities
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
###

#' @include writeMatterArray.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Wrappers around matter constructor/writer and reader functions
###

matterRead <- function(matter, index) {
  # NOTE: Need special case for when all elements of index are NULL because
  #       of how subsetting is implemented for matter objects
  missing_idx <- S4Vectors:::sapply_isNULL(index)
  if (all(missing_idx)) {
    return(matter[])
  }
  DelayedArray:::subset_by_Nindex(matter, index)
}

matterWrite <- function(data, sink) {
  if (is.matrix(data)) {
    if (is.null(sink@offset) & is.null(extent(sink))) {
      matter::matter_mat(data = data,
                         datamode = as.character(datamode(sink)),
                         paths = paths(sink),
                         filemode = filemode(sink),
                         nrow = nrow(sink),
                         ncol = ncol(sink),
                         rowMaj = rowMaj(sink),
                         dimnames = dimnames(sink),
                         chunksize = chunksize(sink))
    } else {
      # NOTE: Advanced usage with non-default extent and offset
      matter::matter_mat(data = data,
                         datamode = as.character(datamode(sink)),
                         paths = paths(sink),
                         filemode = filemode(sink),
                         nrow = nrow(sink),
                         ncol = ncol(sink),
                         rowMaj = rowMaj(sink),
                         dimnames = dimnames(sink),
                         offset = sink@offset,
                         extent = extent(sink),
                         chunksize = chunksize(sink))
    }
  } else if (is.array(data)) {
    if (is.null(sink@offset) & is.null(extent(sink))) {
      matter::matter_arr(data = data,
                         datamode = datamode(sink),
                         paths = paths(sink),
                         filemode = filemode(sink),
                         dim = dim(sink),
                         dimnames = dimnames(sink),
                         chunksize = chunksize(sink))
    } else {
      # NOTE: Advanced usage with non-default extent and offset
      matter::matter_arr(data = data,
                         datamode = datamode(sink),
                         paths = paths(sink),
                         filemode = filemode(sink),
                         dim = dim(sink),
                         dimnames = dimnames(sink),
                         offset = sink@offset,
                         extent = extent(sink),
                         chunksize = chunksize(sink))
    }
  } else {
    stop("Cannot write object of class '", class(data), "' to a matter file")
  }
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Detect and trim trailing slahes in a character vector
###

# NOTE: A copy of HDF5Array:::has_trailing_slash() to avoid depending on
#       HDF5Array
has_trailing_slash <- function(x) {
  stopifnot(is.character(x))
  grepl("/$", x)
}

# NOTE: A copy of HDF5Array:::trim_trailing_slashes() to avoid depending on
#       HDF5Array
trim_trailing_slashes <- function(x) {
  sub("/*$", "", x)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### A simple mechanism to lock/unlock a file so processes can get temporary
### exclusive access to it
###

# NOTE: A copy of HDF5Array:::.locked_path() to avoid depending on HDF5Array
.locked_path <- function(filepath) {
  if (!S4Vectors::isSingleString(filepath) || filepath == "") {
    stop("'filepath' must be a single non-empty string")
  }
  paste0(filepath, "-locked")
}

# NOTE: A copy of HDF5Array:::.safe_file_rename() to avoid depending on
#       HDF5Array
.safe_file_rename <- function(from, to) {
  !file.exists(to) && suppressWarnings(file.rename(from, to))
}

# NOTE: A copy of HDF5Array:::lock_file() to avoid depending on HDF5Array
lock_file <- function(filepath) {
  locked_path <- .locked_path(filepath)
  ## Must wait if the file is already locked.
  while (TRUE) {
    if (.safe_file_rename(filepath, locked_path)) {
      break
    }
    Sys.sleep(0.01)
  }
  locked_path
}

# NOTE: A copy of HDF5Array:::unlock_file() to avoid depending on HDF5Array
unlock_file <- function(filepath) {
  locked_path <- .locked_path(filepath)
  if (!.safe_file_rename(locked_path, filepath)) {
    stop("failed to unlock '", filepath, "' file")
  }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### A global counter that is safe to use in the context of parallelized
### execution
###

# NOTE: A copy of HDF5Array:::.read_counter() to avoid depending on HDF5Array
.read_counter <- function(filepath) {
  counter <- readLines(filepath)
  stopifnot(length(counter) == 1L)
  counter <- suppressWarnings(as.integer(counter))
  if (is.na(counter)) {
    stop("file '", filepath, "' does not contain a counter")
  }
  counter
}

# NOTE: Will overwrite an existing file.
# NOTE: A copy of HDF5Array:::.write_counter() to avoid depending on HDF5Array
.write_counter <- function(counter, filepath) {
  writeLines(as.character(counter), filepath)
  counter
}

# NOTE: NOT safe to use in the context of parallel execution!
# NOTE: A copy of HDF5Array:::init_global_counter() to avoid depending on
#       HDF5Array
init_global_counter <- function(filepath, counter = 1L) {
  if (!S4Vectors::isSingleString(filepath) || filepath == "") {
    stop("'filepath' must be a single non-empty string")
  }
  if (file.exists(filepath)) {
    stop("file '", filepath, "' already exists")
  }
  if (!S4Vectors::isSingleNumber(counter)) {
    stop("'counter' must be a single number")
  }
  if (!is.integer(counter)) {
    counter <- as.integer(counter)
  }
  .write_counter(counter, filepath)
}

### Use a lock mechanism to prevent several processes from trying to increment
### the counter simultaneously. So is safe to use in the context of parallel
### execution e.g.
###
###   library(BiocParallel)
###   filepath <- tempfile()
###   init_global_counter(filepath)
###   bplapply(1:10, function(i) get_global_counter(filepath, increment=TRUE))
###
# NOTE: A copy of HDF5Array:::get_global_counter() to avoid depending on
#       HDF5Array
get_global_counter <- function(filepath, increment = FALSE) {
  if (!S4Vectors::isTRUEorFALSE(increment)) {
    stop("'increment' must be TRUE or FALSE")
  }
  locked_path <- lock_file(filepath)
  on.exit(unlock_file(filepath))
  counter <- .read_counter(locked_path)
  if (increment) {
    .write_counter(counter + 1L, locked_path)
  }
  counter
}
