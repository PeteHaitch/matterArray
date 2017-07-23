\name{writeMatterArray}

\alias{class:matterRealizationSink}
\alias{matterRealizationSink-class}
\alias{matterRealizationSink}

\alias{dimnames,matterRealizationSink-method}
\alias{write_to_sink,array,matterRealizationSink-method}

\alias{coerce,matterRealizationSink,matterArraySeed-method}
\alias{coerce,matterRealizationSink,matterArray-method}
\alias{coerce,matterRealizationSink,DelayedArray-method}

\alias{writeMatterArray}
\alias{writeMatterDataset}

\alias{coerce,ANY,matterArray-method}
\alias{coerce,DelayedArray,matterArray-method}

\title{Write an array-like object to a matter file}

\description{
  A function for writting an array-like object to a matter file.
}

\usage{
  writeMatterArray(x, paths = NULL, datamode = type(x),
                  chunksize = getOption("DelayedArray.block.size"),
                  rowMaj = FALSE, offset = NULL, extent = NULL,
                  verbose = FALSE)
}

\arguments{
  \item{x}{
    The array-like object to write to a matter file.

    If \code{x} is a \link{DelayedArray} object, \code{writeMatterArray}
    \emph{realizes} it on disk, that is, all the delayed operations carried
    by the object are executed while the object is written to disk.
    See "On-disk realization of a DelayedArray object as a matter dataset"
    section below for more information.
  }
  \item{paths}{
    \code{NULL} or the path to the new matter file(s) where to write the
    dataset. If \code{NULL}, then the dataset will be written to the current
    \emph{matter dump file} i.e. the path returned by
    \code{\link{getMatterDumpFile}()} will be used.
  }
  \item{datamode}{A 'character' vector giving the storage mode of the data on
  disk. Allowable values are the C types ('char', 'uchar', short', 'ushort',
  'int', 'uint', 'long', 'ulong', 'float') and their R equivalents ('raw',
  'logical', 'integer', 'numeric').}
  \item{chunksize}{The maximum number of elements which should be loaded into
    memory at once. Used by methods implementing summary statistics and linear
    algebra. Ignored when explicitly subsetting the dataset.}
  \item{rowMaj}{Only applicable if \code{x} is a 2-dimensionsal array (i.e. a
    matrix). Whether the data should be stored in row-major order (as opposed
    to column-major order) on disk. Defaults to 'FALSE', for efficient access
    to columns. Set to 'TRUE' for more efficient access to rows instead.}
  \item{offset}{A vector giving the offsets in number of bytes from the
    beginning of each file in 'paths', specifying the start of the data to be
    accessed for each file. If \code{NULL}, then semsible default values are
    used.}
  \item{extent}{A vector giving the length of the data for each file in
    'paths', specifying the number of elements of size 'datamode' to be
    accessed from each file. If \code{NULL}, then semsible default values are
    used.}
  \item{verbose}{Set to \code{TRUE} to make the function display progress.}
}

\details{
  Please note that, depending on the size of the data to write to disk
  and the performance of the disk, \code{writeMatterArray} can take a long
  time to complete. Use \code{verbose=TRUE} to see its progress.

  Use \code{\link{setMatterDumpFile}} and \code{\link{setMatterDumpName}} to
  control the location of automatically created matter datasets.
}

\value{
  A matterArray object pointing to the newly written matter dataset on disk.
}

\section{On-disk realization of a DelayedArray object as a matter dataset}{
  When passed a \link{DelayedArray} object, \code{writeMatterArray}
  \emph{realizes} it on disk, that is, all the delayed operations carried
  by the object are executed on-the-fly while the object is written to disk.
  This uses a block-processing strategy so that the full object is not
  realized at once in memory. Instead the object is processed block by block
  i.e. the blocks are realized in memory and written to disk one at a time.

  In other words, \code{writeMatterArray(x, ...)} is semantically equivalent
  to \code{writeMatterArray(as.array(x), ...)}, except that \code{as.array(x)}
  is not called because this would realize the full object at once in memory.

  See \code{?\link{DelayedArray}} for general information about
  \link{DelayedArray} objects.
}

\seealso{
  \itemize{
    % TODO: Link once implemented and documented
    %\item \code{\link{saveMatterSummarizedExperiment}} and
    %      \code{\link{loadMatterSummarizedExperiment}} in
    %      the \pkg{SummarizedExperiment} package for saving/loading a
    %      matter-based SummarizedExperiment object to/from disk.

    \item \linkS4class{matter}, \code{\link[matter]{matter_mat}}, and
          \code{link[matter]{matter_arr}} for further information about the
          arguments.

    \item \link{matter-dump-management} for controlling the location of
          automatically created matter datasets.

    \item \link{matterArray} objects.

    \item \link{DelayedArray} objects.

    \item \link{DelayedArray-utils} for common operations on DelayedArray
          objects.
  }
}

\examples{
x <- matrix(runif(10000), ncol = 10, dimnames = list(NULL, LETTERS[1:10]))
X <- writeMatterArray(x)
X

# Dealyed operations are realised before writing to disk
y <- DelayedArray(x) + 100
Y <- writeMatterArray(y)
Y
}
\keyword{methods}