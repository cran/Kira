\name{results}

\alias{results}

\title{Results of the classification process}

\description{Returns the results of the classification process.}

\usage{results(orig.class, predict)}
 
\arguments{
  \item{orig.class}{Data with the original classes.}
  \item{predict}{Data with classes of results of classifiers.}
}

\value{
  \item{conf.mtx}{Confusion matrix.}
  \item{rate.hits}{Hit rate.}
  \item{rate.error}{Error rate.}
  \item{num.hits}{Number of correct instances.}
  \item{num.error}{Number of wrong instances.}
  \item{kappa}{Kappa coefficient.}
  \item{roc.curve}{Data for the ROC curve in classes.}
  \item{prc.curve}{Data for the PRC curve in classes.}
  \item{res.class}{General results of the classes: Sensitivity, Specificity, Precision, TP Rate, FP Rate, NP Rate, F-Score, MCC, ROC Area, PRC Area.}
}

\author{Paulo Cesar Ossani}

\references{
Chicco, D.; Warrens, M. J.; Jurman, G. The matthews correlation coefficient (mcc) is more informative than cohen's kappa and brier score in binary classification assessment. \emph{IEEE Access, IEEE}, v. 9, p. 78368-78381, 2021.
}

\seealso{\code{\link{plot_curve}}}

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
# message("Data for the ROC curve in classes:"); resp$roc.curve 
# message("Data for the PRC curve in classes:"); resp$prc.curve
message("General results of the classes:"); resp$res.class

dat <- resp$roc.curve; tp = "roc"; ps = 3
# dat <- resp$prc.curve; tp = "prc"; ps = 4

plot_curve(data = dat, type = tp, title = NA, xlabel = NA, ylabel = NA,  
           posleg = ps, boxleg = FALSE, axis = TRUE, size = 1.1, grid = TRUE, 
           color = TRUE, classcolor = NA, savptc = FALSE,
           width = 3236, height = 2000, res = 300)

}

\keyword{results}
