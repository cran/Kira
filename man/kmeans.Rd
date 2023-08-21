\name{kmeans}

\alias{kmeans}

\title{kmeans unsupervised classification.}

\description{Performs kmeans unsupervised classification analysis in a data set.}

\usage{kmeans(data, normalize = FALSE, num.groups = 2)}

\arguments{
  \item{data}{Data to be analyzed.}
  \item{normalize}{Normalize the data (default = FALSE).}
  \item{num.groups}{Number of groups to be formed (default = 2).}
}

\value{ 
  \item{groups}{Original data with groups formed.}
  \item{res.groups}{Results of the groups formed.}
  \item{R.sqt}{Result of the R squared.}
  \item{sum.sqt}{Total sum of squares.}
}

\references{
Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

Mingoti, S. A. \emph{analysis de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Ferreira, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
}

\author{
Paulo Cesar Ossani
}

\examples{
data(iris) # data set

data <- iris

res <- kmeans(data[,1:4], normalize = FALSE, num.groups = 3)
 
message("R squared: ", res$R.sqt)             
# message("Total sum of squares: ", res$sum.sqt)
message("Groups formed:"); res$groups
# message("Table with the results of the groups:"); res$res.groups

#write.table(file=file.path(tempdir(),"GroupData.csv"), res$groups, sep=";",
#            dec=",",row.names = TRUE)
}

\keyword{kmeans}
