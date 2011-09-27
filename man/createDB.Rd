\name{createDB}
\alias{createDB}
\title{
Create a SQLite database
}
\description{
create a relational database based on a schema defined in this package if argument
sql is not specified. There are two tables (experiment and mz) defined to 
capture necessary information to annotate ions or peaks. 
}
\usage{
createDB(dbname = "mzDB.db", sql = "mzDBSchema.sql")
}

\arguments{
  \item{dbname}{database name}
  \item{sql}{predefined schema, or a modified definition}
}
\details{
}
\value{
A database file saved in the current folder
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

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
\dontrun{
    createDB(dbname="my.db")
}
}

\keyword{ utilities }
\keyword{ IO }