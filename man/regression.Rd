\name{regression}

\alias{regression}

\title{Linear regression supervised classification method}

\description{Performs supervised classification using the linear regression method.}

\usage{regression(train, test, class, intercept = TRUE)}
 
\arguments{
  \item{train}{Data set of training, without classes.}
  \item{test}{Test data set.}
  \item{class}{Vector with data classes names.}
  \item{intercept}{Consider the intercept in the regression (default = TRUE).}
}

\value{
  \item{predict}{The classified factors of the test set.}
}

\author{Paulo Cesar Ossani}

\references{
Charnet, R.; at al.. \emph{Analise de modelos de regressao lienar,} 2a ed. Campinas: Editora da Unicamp, 2008. 357 p.

Rencher, A. C.; Schaalje, G. B. \emph{Linear models in statisctic.} 2th. ed. New Jersey: John & Sons, 2008. 672 p.

Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

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

r <- (ncol(data) - 1)
res <- regression(train = data.train[,1:r], test = data.test[,1:r], 
                  class = class.train, intercept = TRUE)

resp <- results(orig.class = class.test, predict = res$predict)

message("Confusion matrix:"); resp$conf.mtx  
message("Hit rate: ", resp$rate.hits)
message("Error rate: ", resp$rate.error)
message("Number of correct instances: ", resp$num.hits)
message("Number of wrong instances: ", resp$num.error)
message("Kappa coefficient: ", resp$kappa)
message("General results of the classes:"); resp$res.class

}

\keyword{regression}
