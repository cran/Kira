\name{elbow}

\alias{elbow}

\title{Elbow method to determine the optimal number of clusters.}

\description{Generates the Elbow graph and returns the ideal number of clusters.}

\usage{elbow(data, k.max = 20, method = "AutoElbow", plot = TRUE, 
      cut = TRUE, title = NA, xlabel = NA, ylabel = NA, size = 1.1,  
      grid = TRUE, color = TRUE, savptc = FALSE, width = 3236, 
      height = 2000, res = 300, casc = TRUE)}
 
\arguments{
  \item{data}{Data with x and y coordinates.}
  \item{k.max}{Maximum number of clusters for comparison (default = 20).} 
  \item{method}{Method used to find the ideal number k of clusters: "jump", "curvature", "Exp", "AutoElbow" (default).} 
  \item{plot}{Indicates whether to plot the elbow graph (default = TRUE).}
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
}

\author{Paulo Cesar Ossani}

\references{
Erich, S. Stop using the Elbow criterion for k-means and how to choose the number of clusters instead. \emph{ACM SIGKDD Explorations Newsletter.} 25 (1): 36-42. arXiv:2212.12189. 2023. doi: 10.1145/3606274.3606278

Sugar, C. A. and James, G. M. Finding the number of clusters in a dataset: An information-theoretic approach. \emph{Journal of the American Statistical Association}, 98, 463, 750-763. 2003. doi: 10.1198/016214503000000666

Zhang, Y.; Mandziuk, J.; Quek, H. C. and Goh, W. Curvature-based method for determining the number of clusters. \emph{Inf. Sci.} 415, 414-428, 2017. doi: 10.1016/j.ins.2017.05.024

Onumanyi, A. J.; Molokomme, D. N.; Isaac, S. J. and Abu-Mahfouz, A. M. Autoelbow: An automatic elbow detection method for estimating the number of clusters in a dataset. \emph{Applied Sciences 12}, 15. 2022. doi: 10.3390/app12157515
}

\examples{
data(iris) # data set

res <- elbow(data = iris[,1:4], k.max = 20, method = "AutoElbow", cut = TRUE, 
             plot = TRUE, title = NA, xlabel = NA, ylabel = NA, size = 1.1, 
             grid = TRUE, savptc = FALSE, width = 3236, color = TRUE, 
             height = 2000, res = 300, casc = FALSE)
             
res$k.ideal # number of clusters

}

\keyword{elbow}
