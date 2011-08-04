\name{saveMSnRaw}
\alias{saveMSnRaw}
\title{
Retrieve MS2 and MS3 data and save as R binary in the current folder
}
\description{
save MS2 and MS3 data for later processing such as ion tree construction. 
R binary files "MS2RAW.Rdata" and "MS3RAW.Rdata" may be found in the current folder, which can be reloaded. 
}
\usage{
saveMSnRaw(dataFolder = "D:/Data/Raw")
}

\arguments{
  \item{dataFolder}{current data folder}
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
#saveMSnRaw("D:/Data/Raw")
#load("D:/Data/Raw/MS2RAW.Rdata")  
#ls()
}

\keyword{ utilities }
\keyword{ IO }

