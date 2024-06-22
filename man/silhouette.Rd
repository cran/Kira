\name{silhouette}

\alias{silhouette}

\title{Silhouette method to determine the optimal number of clusters.}

\description{Generates the silhouette graph and returns the ideal number of clusters in the k-means method.}

\usage{silhouette(data, k.cluster = 2:10, plot = TRUE, cut = TRUE,
           title = NA, xlabel = NA, ylabel = NA, size = 1.1, grid = TRUE, 
           color = TRUE, savptc = FALSE, width = 3236, height = 2000,
           res = 300, casc = TRUE)}
 
\arguments{
  \item{data}{Data with x and y coordinates.}
  \item{k.cluster}{Cluster numbers for comparison in the k-means method (default = 2:10).} 
  \item{plot}{Indicates whether to plot the silhouette graph (default = TRUE).}
  \item{cut}{Indicates whether to plot the best cluster indicative line (default = TRUE).}
  \item{title}{Title of the graphic, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{size}{Size of points on the graph and line thickness (default = 1.1).}
  \item{grid}{Put grid on graph (default = TRUE).}
  \item{color}{Colored graphic (default = TRUE).}
  \item{savptc}{Saves the graph image to a file (default = FALSE).} 
  \item{width}{Graphic image width when savptc = TRUE (defaul = 3236).}
  \item{height}{Graphic image height when savptc = TRUE (default = 2000).}
  \item{res}{Nominal resolution in ppi of the graphic image when savptc = TRUE (default = 300).}
  \item{casc}{Cascade effect in the presentation of the graphic (default = TRUE).}
}

\value{
  \item{k.ideal}{Ideal number of clusters.}
  \item{eve.si}{Vector with averages of silhouette indices of cluster groups (si). }
}

\author{Paulo Cesar Ossani}

\references{
Anitha, S.; Metilda, M. A. R. Y. An extensive investigation of outlier detection by cluster validation indices. \emph{Ciencia e Tecnica Vitivinicola - A Science and Technology Journal}, v. 34, n. 2, p. 22-32, 2019. doi: 10.13140/RG.2.2.26801.63848

Kaufman, L. and Rousseeuw, P. J. \emph{Finding Groups in Data: An Introduction to Cluster Analysis}, New York: John Wiley & Sons. 1990.

Martinez, W. L.; Martinez, A. R.; Solka, J. \emph{Exploratory data analysis with MATLAB}. 2nd ed. New York: Chapman & Hall/CRC, 2010. 499 p.

Rousseeuw P. J. Silhouettes: A Graphical Aid to the Interpretation and Validation of Cluster Analysis. \emph{Journal of Computational and Applied Mathematics}, 20:53-65. 1987. doi: 10.1016/0377-0427(87)90125-7
}

\examples{
data(iris) # data set

res <- silhouette(data = iris[,1:4], k.cluster = 2:10, cut = TRUE, 
                  plot = TRUE, title = NA, xlabel = NA, ylabel = NA, 
                  size = 1.1, grid = TRUE, savptc = FALSE, width = 3236, 
                  color = TRUE, height = 2000, res = 300, casc = TRUE)
             
res$k.ideal # number of clusters
res$eve.si  # vector with averages of si indices


res <- silhouette(data = iris[,1:4], k.cluster = 3, cut = TRUE, 
                  plot = TRUE, title = NA, xlabel = NA, ylabel = NA, 
                  size = 1.1, grid = TRUE, savptc = FALSE, width = 3236, 
                  color = TRUE, height = 2000, res = 300, casc = TRUE)
             
res$k.ideal # number of clusters
res$eve.si  # vector with averages of si indices
}

\keyword{silhouette}
