### =========================================================================
### matterArray objects
### -------------------------------------------------------------------------

#' @exportClass matterArraySeed
setClass("matterArraySeed",
         contains = "Array",
         representation(
           matter = "matter")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

#' @export
setMethod("dim", "matterArraySeed", function(x) dim(x@matter))
#' @export
setMethod("dimnames", "matterArraySeed", function(x) dimnames(x@matter))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### subset_seed_as_array()
###

.subset_matterArraySeed_as_array <- function(seed, index) {
  # NOTE: Need special case for when all elements of index are NULL because
  #       of how subsetting is implemented for matter objects
  missing_idx <- S4Vectors:::sapply_isNULL(index)
  if (all(missing_idx)) {
    return(seed@matter[])
  }
  DelayedArray:::subset_by_Nindex(seed@matter, index)
}

#' @importMethodsFrom DelayedArray subset_seed_as_array
#' @export
setMethod("subset_seed_as_array", "matterArraySeed",
          .subset_matterArraySeed_as_array
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### matterArraySeed constructor
###

# NOTE: Not exported
# TODO: Investigate the possiblity to store the dimnames in the matter file and
#       make dimnames() on the object returned by matterArraySeed() bring them
#       back.
#' @export
matterArraySeed <- function(matter) {
  if (is(matter, "matter") &
      !is(matter, "matter_mat") && !is(matter, "matter_arr")) {
    stop("Only matter_mat and matter_arr instances are currently supported")
  }
  return(S4Vectors::new2("matterArraySeed", matter = matter))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### matterArray and matterMatrix objects
###
### We define these classes only for cosmetic reasons i.e. to hide the
### DelayedArray and DelayedMatrix classes from the user. The user will see
### and manipulate matterArray and matterMatrix objects instead of DelayedArray
### and DelayedMatrix objects.
###

#' @exportClass matterArray
setClass("matterArray", contains = "DelayedArray")

#' @exportClass matterMatrix
setClass("matterMatrix", contains = c("DelayedMatrix", "matterArray"))

# NOTE: Automatic coercion method from matterArray to matterMatrix silently
#       returns a broken object (unfortunately these dummy automatic coercion
#       methods don't bother to validate the object they return). So we
#       overwrite it.
setAs("matterArray", "matterMatrix", function(from) new("matterMatrix", from))

# NOTE: For internal use only.
setMethod("matrixClass", "matterArray", function(x) "matterMatrix")

.validate_matterArray <- function(x) {
  if (!is(x@seed, "matterArraySeed")) {
    return(S4Vectors::wmsg("'x@seed' must be a matterArraySeed object"))
  }
  if (!DelayedArray:::is_pristine(x)) {
    return(S4Vectors::wmsg("'x' carries delayed operations"))
  }
  TRUE
}

S4Vectors::setValidity2("matterArray", .validate_matterArray)

setAs("ANY", "matterMatrix",
      function(from) as(as(from, "matterArray"), "matterMatrix")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

#' @export
setMethod("DelayedArray", "matterArraySeed",
          function(seed) {
            DelayedArray:::new_DelayedArray(seed, Class = "matterArray")
          }
)

# NOTE: Works directly on a matterArraySeed object, in which case it must be
#       called with a single argument.
#' @importFrom DelayedArray DelayedArray
#' @export
matterArray <- function(matter) {
  if (is(matter, "matterArraySeed")) {
    seed <- matter
  } else {
    seed <- matterArraySeed(matter)
  }
  DelayedArray(seed)
}
