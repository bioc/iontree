\name{formatSpec}
\alias{formatSpec}
\title{
Format mass spec matrix data into a string format, or vice versa
}
\description{
argument x is a 2-column matrix of mz and intenity, or a string format of mz-internsity
paris. Character pair of mz and intensity is separated by semicolon, for example, 150 2345.6; 151 4325.67; .... which is often used to represent a mass spectrum as seen in NIST and MassBank. 
}
\usage{
formatSpec(x, fromTo = c("mat2str", "str2mat"))
}

\arguments{
  \item{x}{2-col matrix or type of character depends on "fromTo"}
  \item{fromTo}{type of conversion}
}
\details{
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x="150 2345.6; 151 4325.67;"
formatSpec(x, fromTo="str2mat") 
}

\keyword{ utilities }

