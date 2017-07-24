### =========================================================================
### writeMatterArray()
### -------------------------------------------------------------------------
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### matterRealizationSink objects
###
### The matterRealizationSink class is a concrete RealizationSink subclass that
### implements a matter realization sink.
###

setClassUnion("integer_OR_NULL", c("integer", "NULL"))
setClassUnion("numeric_OR_NULL", c("numeric", "NULL"))
setClassUnion("list_OR_NULL", c("list", "NULL"))

# NOTE: matterRealizationSink is essentially an S4 class to store the
#       parameters passed to matter::matter_*() functions when writing an
#       array-like object (data) to a matter file.
#' @exportClass matterRealizationSink
setClass("matterRealizationSink",
         contains = "RealizationSink",
         representation(
           datamode = "factor",
           paths = "character",
           filemode = "character",
           offset = "numeric_OR_NULL",
           extent = "numeric_OR_NULL",
           dim = "integer_OR_NULL",
           dimnames = "list_OR_NULL",
           rowMaj = "logical",
           chunksize = "integer"
         )
)

#' @importMethodsFrom matter datamode
#' @export
setMethod("datamode", "matterRealizationSink", function(x) x@datamode)
#' @importMethodsFrom matter paths
#' @export
setMethod("paths", "matterRealizationSink", function(x) x@paths)
#' @importMethodsFrom matter filemode
#' @export
setMethod("filemode", "matterRealizationSink", function(x) x@filemode)
#' @importMethodsFrom matter chunksize
#' @export
setMethod("chunksize", "matterRealizationSink", function(x) x@chunksize)
#' @export
setMethod("dim", "matterRealizationSink", function(x) x@dim)
#' @export
setMethod("dimnames", "matterRealizationSink",
          function(x)  {
            ans <- x@dimnames
            if (all(S4Vectors:::sapply_isNULL(ans))) {
              return(NULL)
            }
            ans
          }
)
setGeneric("rowMaj", function(x) standardGeneric("rowMaj"))
setMethod("rowMaj", "matterRealizationSink", function(x) x@rowMaj)
# NOTE: No offset() generic in order to avoid clash with stats::offset()
setGeneric("extent", function(x) standardGeneric("extent"))
setMethod("extent", "matterRealizationSink", function(x) x@extent)

# TODO: Investigate the possiblity to write the dimnames to the matter file.
#' @export
matterRealizationSink <-
  function(datamode = "double", paths = NULL, filemode = "rb+",
           chunksize = getOption("DelayedArray.block.size"), dim = NULL,
           dimnames = NULL, rowMaj = FALSE, offset = NULL, extent = NULL) {
    datamode <- matter:::make_datamode(datamode, type = "C")
    if (is.null(paths)) {
      paths <- getMatterDumpFile(for.use = TRUE)
    } else {
      paths <- normalize_dump_paths(paths)
    }
    appendDatasetCreationToMatterDumpLog(paths, dim, datamode)
    if (is.null(dimnames)) {
      dimnames <- vector("list", length(dim))
    } else {
      # TODO: Write the dimnames to the matter file.
    }
    S4Vectors::new2("matterRealizationSink", datamode = datamode,
                    paths = paths, filemode = filemode, chunksize = chunksize,
                    dim = dim, dimnames = dimnames, rowMaj = rowMaj,
                    offset = offset, extent = extent)
  }

#' @importMethodsFrom DelayedArray refdim
#' @export
setMethod("write_to_sink", c("array", "matterRealizationSink"),
          function(x, sink, viewport) {
            if (is.null(viewport)) {
              stopifnot(identical(dim(x), dim(sink)))
              index <- NULL
            } else {
              stopifnot(identical(dim(x), dim(viewport)),
                        identical(dim(sink), refdim(viewport)))
              index <- DelayedArray::makeNindexFromArrayViewport(viewport, TRUE)
            }
            matterWrite(data = x, sink = sink)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercing a matterRealizationSink object.
###

# NOTE: This must only be called once the sink has been written to, otherwise
#       it hangs because of 'bug' reported in matter_issues.md
# TODO: Add check for the above NOTE
# TODO: This coercion needs to propagate the dimnames *through* the matter file.
#       For more details about this, see TODO right before definition of
#       matterRealizationSink() above in this file and right before definition
#       of matterArraySeed() in matterArray-class.R.
setAs("matterRealizationSink", "matterArraySeed",
      function(from) {
        if (length(dim(from)) == 2L) {
          # NOTE: matrix (2D-array)
          if (is.null(from@offset) & is.null(extent(from))) {
            matter <- matter::matter_mat(datamode = datamode(from),
                                         paths = paths(from),
                                         filemode = filemode(from),
                                         chunksize = chunksize(from),
                                         nrow = nrow(from),
                                         ncol = ncol(from),
                                         dimnames = dimnames(from),
                                         rowMaj = rowMaj(from))
          } else {
            # NOTE: Advanced usage with non-default extent and offset
            matter <-
              matter::matter_mat(datamode = as.character(datamode(from)),
                                 paths = paths(from),
                                 filemode = filemode(from),
                                 chunksize = chunksize(from),
                                 nrow = nrow(from),
                                 col = col(from),
                                 dimnames = dimnames(from),
                                 rowMaj = rowMaj(from),
                                 offset = from@offset,
                                 extent = extent(from))
          }
        } else {
          if (is.null(from@offset) & is.null(extent(from))) {
            matter <-
              matter::matter_arr(datamode = as.character(datamode(from)),
                                 paths = paths(from),
                                 filemode = filemode(from),
                                 chunksize = chunksize(from),
                                 dim = dim(from),
                                 dimnames = dimnames(from))
          } else {
            # NOTE: Advanced usage with non-default extent and offset
            matter <-
              matter::matter_arr(datamode = as.character(datamode(from)),
                                 paths = paths(from),
                                 filemode = filemode(from),
                                 chunksize = chunksize(from),
                                 dim = dim(from),
                                 dimnames = dimnames(from),
                                 offset = from@offest,
                                 extent = extent(from))
          }
        }
        matterArraySeed(matter)
      }
)

# NOTE: This must only be called once the sink has been written to, otherwise
#       it hangs because of 'bug' reported in matter_issues.md
# TODO: Add check for the above NOTE
# NOTE: This coercion currently drops the dimnames but will naturally
#       propagate them when coercion from matterRealizationSink to
#       matterArraySeed propagates them. See TODO above.
setAs("matterRealizationSink", "matterArray",
      function(from) matterArray(as(from, "matterArraySeed"))
)

# NOTE: This must only be called once the sink has been written to, otherwise
#       it hangs because of 'bug' reported in matter_issues.md
# TODO: This hangs if no data has been written to the sink because of 'bug'
#       reported in matter_issues.md
setAs("matterRealizationSink", "DelayedArray",
      function(from) {
        ans <- matterArray(as(from, "matterArraySeed"))
        # NOTE: Temporarily needed because coercion from matterRealizationSink
        #       to matterArraySeed does not propagate the dimnames at the
        #       moment. See TODO above.
        # TODO: Remove line below when TODO above is addressed.
        dimnames(ans) <- dimnames(from)
      }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### writeMatterArray()
###

# Write the dataset to the current dump if 'file' is not specified.
# Return a matterArray object pointing to the newly written matter dataset on
# disk.
#' @importMethodsFrom DelayedArray type write_to_sink
#' @export
writeMatterArray <- function(x, paths = NULL, datamode = type(x),
                             chunksize = getOption("DelayedArray.block.size"),
                             rowMaj = FALSE, offset = NULL, extent = NULL,
                             verbose = FALSE) {
  if (!S4Vectors::isTRUEorFALSE(verbose)) {
    stop("'verbose' must be TRUE or FALSE")
  }
  sink <- matterRealizationSink(datamode = datamode,
                                paths = paths,
                                filemode = "rb+",
                                chunksize = chunksize,
                                dim = dim(x),
                                dimnames = dimnames(x),
                                rowMaj = rowMaj,
                                offset = offset,
                                extent = extent)
  if (verbose) {
    old_verbose <- DelayedArray:::set_verbose_block_processing(verbose)
    on.exit(DelayedArray:::set_verbose_block_processing(old_verbose))
  }
  write_to_sink(x, sink, NULL)
  as(sink, "matterArray")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion to matterArray.
###
### The methods below write the object to disk. Note that coercion from
### matterRealizationSink to matterArray is already taken care of by the
### specific method above and doesn't write anything to disk. So coercing to
### matterArray in general writes the object to disk *except* when the object
### to coerce is a matterRealizationSink object.
###

# NOTE: writes to current dump
.as_matterArray <- function(from) writeMatterArray(from)

setAs("ANY", "matterArray", .as_matterArray)

# NOTE: Automatic coercion method from DelayedArray to matterArray silently
#       returns a broken object (unfortunately these dummy automatic coercion
#       methods don't bother to validate the object they return). So we
#       overwrite it.
setAs("DelayedArray", "matterArray", .as_matterArray)
