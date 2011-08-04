\name{hasMS2}
\alias{hasMS2}
\title{
Check which samples have MS2 spectra generated
}
\description{
check whether MS2 data are available for the ion/peak specified by mzRange and rtRange among samples. 
}
\usage{
hasMS2(MS2RAW, mzRange = c(1854, 1854.5), rtRange = c(280, 400))
}

\arguments{
  \item{MS2RAW}{MS2 raw data in R binary file, see \code{saveMSnRaw}}
  \item{mzRange}{m/z range}
  \item{rtRange}{rt range}
}
\details{
}
\value{
return sample index
}
\references{
}
\author{
Mingshu Cao
}
\note{
}

\seealso{
}
\examples{
}

\keyword{ utilities }
