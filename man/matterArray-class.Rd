\name{matterArray-class}
\docType{class}

% matterArraySeed

\alias{class:matterArraySeed}
\alias{matterArraySeed-class}
\alias{matterArraySeed}

\alias{dim,matterArraySeed-method}
\alias{subset_seed_as_array,matterArraySeed-method}

% matterArray and matterMatrix

\alias{class:matterArray}
\alias{matterArray-class}

\alias{class:matterMatrix}
\alias{matterMatrix-class}
\alias{matterMatrix}

\alias{coerce,matterArray,matterMatrix-method}
\alias{matrixClass,matterArray-method}

\alias{coerce,ANY,matterMatrix-method}

\alias{DelayedArray,matterArraySeed-method}
\alias{matterArray}

\title{matter datasets as array-like objects}

\description{
  We provide 2 classes for representing an (on-disk) matter dataset as an
  array-like object in R:
    \itemize{
      \item matterArray: A high-level class matterArray that extends
      \link{DelayedArray}. All the operations available on
      \link{DelayedArray} objects work on matterArray objects.

      \item matterArraySeed: A low-level class for pointing to a matter
        dataset. No operation can be performed directly on an matterArraySeed
        object. It needs to be wrapped in a \link{DelayedArray} or matterArray
        object first. An matterArray object is just an matterArraySeed object
        wrapped in a \link{DelayedArray} object.
    }
}

\usage{
  # Constructor functions:
  matterArray(matter)
  matterArraySeed(matter)
}

\arguments{
  \item{matter}{A \linkS4class{matter} instance.
  }
}

\value{
  A matterArray object for \code{matterArray()}.

  A matterArraySeed object for \code{matterArraySeed()}.
}

\seealso{
  \itemize{
    \item \link{DelayedArray} objects.

    \item \link{DelayedArray-utils} for common operations on
    \link{DelayedArray} objects.

    \item \code{\link{writematterArray}} for writting an array-like object
    to a matter file.

    \item \link{matter-dump-management} for controlling the location of
    automatically created matter datasets.

    % TODO: Link once implemented and documented
    %\item \code{\link[SummarizedExperiment]{saveMatterSummarizedExperiment}} and
    %\code{\link[SummarizedExperiment]{loadMatterSummarizedExperiment}} in
    %the \pkg{SummarizedExperiment} package for saving/loading a
    %matter-based SummarizedExperiment object to/from disk.

    \item The \pkg{\link[matter]{matter}} package on top of which matterArray
    objects are implemented.

    \item \link[base]{array} objects in base R.
  }
}

\examples{
  ## ---------------------------------------------------------------------
  ## CONSTRUCTION
  ## ---------------------------------------------------------------------
  library(matter)

  # Construct a matter matrix
  exp_mat <- matter(matrix(runif(100000), ncol = 5))
  exp_mat

  # Construct a matterArray object from a matter instance
  exp <- matterArray(exp_mat)
  exp

  ## ---------------------------------------------------------------------
  ## dim/dimnames
  ## ---------------------------------------------------------------------
  dim(exp)

  dimnames(exp)
  dimnames(exp) <- list(NULL, paste0("S", 1:5))
  dimnames(exp)

  ## ---------------------------------------------------------------------
  ## SLICING (A.K.A. SUBSETTING)
  ## ---------------------------------------------------------------------

  # NOTE: Subsetting a matter instance loads the data into memory as an
  #       ordinary matrix or array
  exp_mat_subset <- exp_mat[90:1000, 3:5]
  class(exp_mat_subset)
  exp_mat_subset

  # NOTE: Subsetting a matterArray does *not* load the data into memory and
  #       the returned object is a DelayedMatrix or DelayedArray
  exp_subset <- exp[90:1000, 3:5]
  class(exp_subset)
  exp_subset

  dim(exp_subset)
  # NOTE: Calling as.array() on a matterArray explicitly loads the data into
  #       memory as an ordinary array
  as.array(exp_subset)
  stopifnot(identical(dim(as.array(exp_subset)), dim(exp_subset)))
  stopifnot(identical(dimnames(as.array(exp_subset)), dimnames(exp_subset)))

  ## ---------------------------------------------------------------------
  ## SummarizedExperiment OBJECTS WITH DELAYED ASSAYS
  ## ---------------------------------------------------------------------

  # NOTE: Both matter and matterArray objects can be used as an assay inside a
  #       SummarizedExperiment object. However, by using a matterArray object
  #       instead of a matter object, there is the advantage that subsetting
  #       the SummarizedExperiment object does not load the assay data into
  #       memory

  library(SummarizedExperiment)

  se_mat <- SummarizedExperiment(list(expression = exp_mat))
  se_mat
  stopifnot(validObject(se_mat, complete = TRUE))
  se <- SummarizedExperiment(list(expression = exp))
  se
  stopifnot(validObject(se, complete = TRUE))

  # NOTE: Subsetting the matter-backed SummarizedExperiment object loads the
  #       assay data into memory as an ordinary matrix or array.
  class(assay(se_mat[90:1000, 3:5]))
  # NOTE: Subsetting the matterArray-backed SummarizedExperiment object does
  #       *not* load the data into memory and the assay is a DelayedMatrix or
  #       DelayedArray
  class(assay(se[90:1000, 3:5]))
}
\keyword{methods}
\keyword{classes}