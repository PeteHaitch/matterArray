\name{matter-dump-management}

\alias{matter-dump-management}
\alias{dump-management}

\alias{setMatterDumpDir}
\alias{getMatterDumpDir}

\alias{setMatterDumpFile}
\alias{getMatterDumpFile}

\alias{appendDatasetCreationToMatterDumpLog}
\alias{showMatterDumpLog}

\title{matter dump management}

\description{
  A set of utilities to control the location of automatically created matter
  datasets.
}

\usage{
setMatterDumpDir(dir)
setMatterDumpFile(file)

getMatterDumpDir()
getMatterDumpFile(for.use=FALSE)

showMatterDumpLog()

## For developers:
appendDatasetCreationToMatterDumpLog(paths, dim, datamode)
}

\arguments{
  \item{dir}{
    The path (as a single string) to the current \emph{matter dump directory},
    that is, to the (new or existing) directory where \emph{matter dump files}
    with automatic names will be created. This is ignored if the user
    specified an \emph{matter dump file} with \code{setMatterDumpFile}.
    If \code{dir} is missing, then the \emph{matter dump directory} is set
    back to its default value i.e. to some directory under \code{tempdir()}
    (call \code{getMatterDumpDir()} to get the exact path).
  }
  \item{file}{
    For \code{setMatterDumpFile}:
    The path (as a single string) to the current \emph{matter dump file},
    that is, to the (new or existing) matter file where the \emph{next
    automatic matter datasets} will be written. If \code{file} is
    missing, then a new file with an automatic name will be created
    (in \code{getMatterDumpDir()}) and used for each new dataset.
  }
  \item{paths}{
    For \code{appendDatasetCreationToMatterDumpLog}:
    See the Note TO DEVELOPERS below.
  }
  \item{for.use}{
    Whether the returned file or dataset name is for use by the caller or not.
    See below for the details.
  }
  \item{dim}{
    The dimensions of the matter dataset to be written to disk, that is,
    an integer vector of length one or more giving the maximal indices in each
    dimension.
  }
  \item{datamode}{
    The storage mode of the accessed data when read into R.
    The type (a.k.a. storage mode) of the data to be written to disk. Can be
    obtained with \code{\link[DelayedArray]{type}()} on an array-like object
    (which is equivalent to \code{storage.mode()} or \code{typeof()} on an
    ordinary array). This is typically what an application writing datasets
    to the \emph{matter dump} should pass to the \code{datamode} argument
    of its call to \code{matter::\link{matter}()}.
    See the Note TO DEVELOPERS below for more information.
  }
}

\details{
  Calling \code{getMatterDumpFile()} with noargument should be
  \emph{informative} only i.e. it's a means for the user to know where the
  \emph{next automatic matter dataset} will be written. Since a given file can
  be used only once, the user should be careful to not use that file to
  explicitely create an matter datasetbecause that would get in the way of the
  creation of the \emph{next automatic matter dataset}.
  See the Note TO DEVELOPERS below if you actually need to use this file
  combination.
}

\value{
  \code{getMatterDumpDir} returns the absolute path to the directory
  where \emph{matter dump files} with automatic names will be created.
  Only meaningful if the user did NOT specify an \emph{matter dump file}
  with \code{setMatterDumpFile}.

  \code{getmMtterDumpFile} returns the absolute path to the matter file where
  the \emph{next automatic matter dataset} will be written.

  \code{showMatterDumpLog} returns the dump log in an invisible data frame.
}

\note{
  TO DEVELOPERS:

  If your application needs to write its own dataset to the \emph{matter dump}
  then it should:
  \enumerate{
    \item Get a file/name combination by calling
          \code{getMatterDumpFile(for.use = TRUE)}.

    \item Add an entry to the dump log by calling
          \code{appendDatasetCreationToMatterDumpLog}.
          Typically, this should be done right after creating the sink
          (e.g. with \code{matterRealizationSink}) and before starting to
          write the data to disk. The values passed to
          \code{appendDatasetCreationToMatterDumpLog} via the \code{paths},
          \code{dim}, and \code{datamode} arguments should be those that were
          passed to
          \code{matter::matter} via the \code{paths},
          \code{dim} or \code{nrow} & \code{ncol}, and
          \code{datamode} arguments, respectively. Note that
          \code{appendDatasetCreationToMatterDumpLog} uses a lock
          mechanism so is safe to use in the context of parallel execution.
  }
  This is actually what the coercion method to \link{matterArray} does
  internally.
}

\seealso{
  \itemize{
    \item \code{\link{writeMatterArray}} for writting an array-like object
          to an matter file.

    \item \link{matterArray} objects.

    \item \code{\link[DelayedArray]{type}} in the \pkg{DelayedArray} package.
  }
}

\examples{
getMatterDumpDir()
getMatterDumpFile()

## Use setMatterDumpFile() to change the current matter dump file.
## If the specified file exists, then it must be in matter format or
## an error will be raised. If it doesn't exist, then it will be
## created.
#setmatterDumpFile("path/to/some/matter/file")

a <- array(1:600, c(150, 4))
A <- as(a, "matterArray")
A

b <- array(runif(6000), c(4, 2, 150))
B <- as(b, "matterArray")
B

C <- (log(2 * A + 0.88) - 5) ^ 3 * t(drop(B[ , 1, ]))
as(C, "matterArray")  # realize C on disk

# TODO: Get matterArra added to supportedRealizationBackends(), then uncomment
## Matrix multiplication is not delayed: the output matrix is realized
## block by block. The current "realization backend" controls where
## realization happens e.g. in memory if set to NULL or in an matter file
## if set to "matterArray". See '?realize' in the DelayedArray package for
## more information about "realization backends".
# setRealizationBackend("matterArray")
# m <- matrix(runif(20), nrow = 4)
# P <- C \%*\% m

## See all the matter datasets created in the current session so far:
print(showMatterDumpLog())
}
\keyword{methods}