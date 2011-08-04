\name{searchMS2}
\alias{searchMS2}
\title{
Search MS2 spectrum from sqlite database
}
\description{
search MS2 spetra from the database. Ranking is based on the distance metric that
defined by Cao et al. 2008, cosine and Tanimoto similarity.
}
\usage{
searchMS2(querySpec, premz, dbname = "mzDB.db", scoreFun = "distMS2", output.record = 5, plot.top = TRUE)
}

\arguments{
  \item{querySpec}{query spectrum in 2-col matrix}
  \item{premz}{precursor m/z that query spectrum derived from}
  \item{dbname}{database name}
  \item{scoreFun}{score function, 'distMS2','cos' or 'tanimoto'}
  \item{output.record}{the number of records shown in console}
  \item{plot.top}{plot query spectrum and the top-ranked spectrum}
}

\details{

}
\value{
return top records
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

\keyword{ manip }

