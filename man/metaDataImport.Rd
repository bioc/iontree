\name{metaDataImport}
\alias{metaDataImport}
\title{
Data entry of meta information
}
\description{
data entry of table "experiment". Such meta information may include brief description of sample origin, biological treatment, extraction method, chromatography, ionization, polarity, collision energy and those might affect comparative analysis of iontrees. R default data editor was used to help
provide necessary informaiton. SQlite database browsers are also freely available for different platforms. 
}
\usage{
metaDataImport(dbname = "mzDB.db")
}

\arguments{
  \item{dbname}{database name}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mingshu Cao
}
\note{
A known issue: a call to use data editor (fix) might cause access violation, that was occasionally observed. 
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
#to check information just loaded
#db=dbConnect(dbDriver("SQLite"), dbname="mzDB.db")
#dbListTables(db)
#q1=dbSendQuery(db, "SELECT * FROM experiment") 
#fetch(q1, n=-1)
#dbClearResult(q1)
#dbDisconnect(db)
}

\keyword{ utilities }
