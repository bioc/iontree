\name{buildIonTree}
\alias{buildIonTree}
\title{
Build ion tree
}
\description{
build an ion tree derived from the specified m/z and RT ranges in one sample based on ms2 and ms3 raw data, see \code{saveMSnRaw} and \code{hasMS2}. 
}
\usage{
buildIonTree(mzRange = c(340.5, 341.5), rtRange = c(270, 282), ms2, ms3)
}
\arguments{
  \item{mzRange}{mz range}
  \item{rtRange}{rt range}
  \item{ms2}{ms2 data as list}
  \item{ms3}{ms3 data as list}
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
the full time range is used for direct infusion mass spectrometry. For instance, rtRange=c(0, 300) is used for 5-min total elution time. 
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#mz=867 
#mzDelta=0.5
#mzRange=c(mz-mzDelta, mz+mzDelta)
#rtRange=c(1, 600)  
#hasMS2(MS2RAW, mzRange=c(mz-mzDelta, mz+mzDelta), rtRange=c(0, 600))  

#idx.ms2=1
#ms2=MS2RAW[[idx.ms2]]
#ms3=MS3RAW[[idx.ms2]]

#tree1=buildIonTree(mzRange, rtRange=c(0, 600), ms2, ms3)
#plot(tree1) 
}
\keyword{ utilities }

