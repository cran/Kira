\name{plot_curve}

\alias{plot_curve}

\title{Graphics of the results of the classification process}

\description{Return graphics of the results of the classification process.}

\usage{plot_curve(data, type = "ROC", title = NA, xlabel = NA, ylabel = NA,  
           posleg = 3, boxleg = FALSE, axis = TRUE, size = 1.1, grid = TRUE, 
           color = TRUE, classcolor = NA, savptc = FALSE, width = 3236, 
           height = 2000, res = 300)}
 
\arguments{
  \item{data}{Data with x and y coordinates.}
  \item{type}{ROC (default) or PRC graphics type.}
  \item{title}{Title of the graphic, if not set, assumes the default text.}
  \item{xlabel}{Names the X axis, if not set, assumes the default text.}
  \item{ylabel}{Names the Y axis, if not set, assumes the default text.}
  \item{posleg}{0 with no caption,\cr
                1 for caption in the left upper corner,\cr
                2 for caption in the right upper corner,\cr
                3 for caption in the right lower corner (default),\cr
                4 for caption in the left lower corner.}
  \item{boxleg}{Puts the frame in the caption (default = TRUE).}
  \item{axis}{Put the diagonal axis on the graph (default = TRUE).}
  \item{size}{Size of the points in the graphs (default = 1.1).}
  \item{grid}{Put grid on graphs (default = TRUE).}
  \item{color}{Colored graphics (default = TRUE).}
  \item{classcolor}{Vector with the colors of the classes.}
  \item{savptc}{Saves graphics images to files (default = FALSE).} 
  \item{width}{Graphics images width when savptc = TRUE (defaul = 3236).}
  \item{height}{Graphics images height when savptc = TRUE (default = 2000).}
  \item{res}{Nominal resolution in ppi of the graphics images when savptc = TRUE (default = 300).}
}

\value{ROC or PRC curve.}

\author{Paulo Cesar Ossani}

\seealso{\code{\link{results}}}

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

\keyword{plot.curve}
