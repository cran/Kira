\name{vote}

\alias{vote}

\title{Performs the supervised classification vote method.}

\description{Performs the supervised classification voting method, using maximum agreement between classifiers.}

\usage{vote(mtx.algtms = NA)}
 
\arguments{
  \item{mtx.algtms}{Matrix with the results of the supervised classification algorithms to be analyzed.}
}

\value{
  \item{predict}{The classified factors of the test set.}
}

\author{Paulo Cesar Ossani}

\references{
Kittler, J.; Hatef, M.; Duin, R. P. W. and Matas, J. On combining classifiers. \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence.} 20(3):226-239. 1998. doi: 10.1109/34.667881
}

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

test  <- as.integer(rownames(data.test)) # test data index
r     <- (ncol(data)-1)
class <- data[,c(r+1)] # classes names 

mod1 <- knn(train = data.train[,1:r], test = data.test[,1:r],
            class = class.train, k = 1, dist = 'EUC')
mod2 <- knn(train = data.train[,1:r], test = data.test[,1:r],
            class = class.train, k = 2, dist = 'EUC')
mod3 <- lda(data = data[,1:r], test = test, class = class,
            type = 'test', method = 'moment', prior = NA)
mod4 <- qda(data = data[,1:r], test = test, class = class,
            type = 'test', method = 'mle', prior = NA)
mod5 <- qda(data = data[,1:r], test = test, class = class,
            type = 'test', method = 'moment', prior = NA)
mod6 <- regression(train = data.train[,1:r], test = data.test[,1:r],
                   class = class.train, intercept = TRUE)

mod <- cbind(as.data.frame(mod1$predict), mod2$predict, mod3$predict, 
             mod4$predict, mod5$predict, mod6$predict)

res <- vote(mtx.algtms = mod)

resp <- results(orig.class = class.test, predict = res$predict)

print("Confusion matrix:"); resp$conf.mtx  
cat("Hit rate:", resp$rate.hits,
    "\nError rate:", resp$rate.error,
    "\nNumber of correct instances:", resp$num.hits,
    "\nNumber of wrong instances:", resp$num.error,
    "\nKappa coefficient:", resp$kappa)
print("General results of the classes:"); resp$res.class 
}

\keyword{vote}
