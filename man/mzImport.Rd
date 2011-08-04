\name{mzImport}
\alias{mzImport}
\title{
Data entry of iontree into mz table 
}
\description{
import iontree object into table "mz" in the database
}
\usage{
mzImport(iontree, dbname = "mzDB.db", exp.id)
}

\arguments{
  \item{iontree}{\code{iontree} as defined in this package}
  \item{dbname}{database name}
  \item{exp.id}{id in table of experiment}
}
\details{
}
\value{

}
\references{

}
\author{
Mingshu Cao
}
\note{
see vignette for an example of batch loading
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#to check information just loaded
#db=dbConnect(dbDriver("SQLite"), dbname="mzDB.db")
#dbListTables(db)
#q1=dbSendQuery(db, "SELECT mz, rt, ms2 FROM mz") 
#fetch(q1, n=-1)
#dbClearResult(q1)
#dbDisconnect(db)
}

\keyword{ utilities }