\name{iontree-class}
\Rdversion{1.1}
\docType{class}
\alias{iontree-class}
\alias{plot,iontree-method}
\alias{show,iontree-method}

\title{Class "iontree"}
\description{
iontree representation in S4 class 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("iontree", ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{mz}:}{Object of class \code{"numeric"} peak or ion m/z }
    \item{\code{rt}:}{Object of class \code{"numeric"} peak or ion RT }
    \item{\code{MS2}:}{Object of class \code{"matrix"} ms2 spectrum }
    \item{\code{MS3}:}{Object of class \code{"list"} ms3 spectrum/spectra }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "iontree")}: ... }
    \item{show}{\code{signature(object = "iontree")}: ... }
	 }
}
\references{

}
\author{
Mingshu Cao
}
\note{
To be extended to MSn where n>3
}

\seealso{
}
\examples{
showClass("iontree")
}
\keyword{classes}
