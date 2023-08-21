\name{knn}

\alias{knn}

\title{k-nearest neighbor (kNN) supervised classification method}

\description{Performs the k-nearest neighbor (kNN) supervised classification method.}

\usage{knn(train, test, class, k = 1, dist = "euclidean", lambda = 3)}
 
\arguments{
  \item{train}{Data set of training, without classes.}
  \item{test}{Test data set.}
  \item{class}{Vector with data classes names.}
  \item{k}{Number of nearest neighbors (default = 1).}
  \item{dist}{Distances used in the method: "euclidean" (default), "manhattan", "minkowski", "canberra", "maximum" or "chebyshev".}
  \item{lambda}{Value used in the minkowski distance (default = 3).}
}

\value{
  \item{predict}{The classified factors of the test set.}
}

\author{Paulo Cesar Ossani}

\references{
Aha, D. W.; Kibler, D.; Albert, M. K. Instance-based learning algorithms. \emph{Machine learning.} v.6, n.1, p.37-66. 1991.

Nicoletti, M. do C.. O modelo de aprendizado de maquina baseado em exemplares: principais caracteristicas e algoritmos. Sao Carlos: EdUFSCar, 2005. 61 p.

}

\seealso{\code{\link{plot_curve}} and \code{\link{results}}}

\examples{
data(iris) # data set

data  <- iris
names <- colnames(data)
colnames(data) <- c(names[1:4],"class")

#### Start - hold out validation method ####
dat.sample = sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))
data.train = data[dat.sample == 1,] # training data set
data.test  = data[dat.sample == 2,] # test data set
class.train = as.factor(data.train$class) # class names of the training data set
class.test  = as.factor(data.test$class)  # class names of the test data set
#### End - hold out validation method ####


dist = "euclidean" 
# dist = "manhattan"
# dist = "minkowski"
# dist = "canberra"
# dist = "maximum"
# dist = "chebyshev"

k = 1
lambda = 5

r <- (ncol(data) - 1)
res <- knn(train = data.train[,1:r], test = data.test[,1:r], class = class.train, 
           k = 1, dist = dist, lambda = lambda)

resp <- results(orig.class = class.test, predict = res$predict)

message("Confusion matrix:"); resp$conf.mtx  
message("Hit rate: ", resp$rate.hits)
message("Error rate: ", resp$rate.error)
message("Number of correct instances: ", resp$num.hits)
message("Number of wrong instances: ", resp$num.error)
message("Kappa coefficient: ", resp$kappa)
message("General results of the classes:"); resp$res.class

}

\keyword{knn}
