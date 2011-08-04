\name{topIons}
\alias{topIons}
\title{
Retain spectrum with the most intense ions
}
\description{
Retain the most intense ions in a spectrum and return sorted spectrum in 2-col matrix 
}
\usage{
topIons(mz, intensity, top)
}
\arguments{
  \item{mz}{m/z}
  \item{intensity}{intensity}
  \item{top}{the number of most intense m/z to be maintained}
}
\details{

}
\value{
return a 2-col matrix
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mingshu Cao
}
\note{
%%  ~~further notes~~
}

\examples{
#Just sort:
#topIons(mz, intensity, top=length(mz))
}

\keyword{ utilities }