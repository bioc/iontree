\name{getMSnRaw}
\alias{getMSnRaw}
\title{
Get MSn raw data 
}
\description{
query MSn data by the attribute of 'msLevel' and get MSn raw data into a R list. Users may just use function "saveMSnRaw" to retrieve ion tree from a data file and avoid a direct Java function call.   
}
\usage{
getMSnRaw(msdata, msLevel = 2)
}

\arguments{
  \item{msdata}{
msdata is a reference to a Java ArrayList. Obtained by calling getMSData Java function.
msdata=.jcall("XCMS", "Ljava/util/ArrayList;","getMSData", filename);
}
  \item{msLevel}{
msLevel in integer
}
}
\details{
}
\value{
  \item{premz}{ancestral precursor ions}
  \item{rt}{retention time}
  \item{msn.sp}{a list of spectrum (m/z, intensity)}
}
\references{

}
\author{
Mingshu Cao
}
\note{
MS1 data could be queried by msLevel=1. The return type is still a list but different components (rt, tic, sp). 
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
\keyword{utilities}

