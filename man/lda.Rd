\name{lda}

\alias{lda}

\title{Linear discriminant analysis (LDA).}

\description{Perform linear discriminant analysis.}

\usage{lda(data, test = NA, class = NA, type = "train", 
   method = "moment", prior = NA)}

\arguments{
  \item{data}{Data to be classified.}
  \item{test}{Vector with indices that will be used in 'data' as test. For type = "train", one has test = NA.}
  \item{class}{Vector with data classes names.}
  \item{type}{Type of type:\cr
                    "train" - data training (default), or\cr
                    "test" - classifies the data of the vector "test".}
  \item{method}{Classification method:\cr 
                "mle" to MLEs,\cr
                "mve" to use cov.mv, \cr
                "moment" (default) for standard mean and variance estimators, or \cr
                "t" for robust estimates based on the t distribution.}
  \item{prior}{Probabilities of occurrence of classes. If not specified, it will take the proportions of the classes. If specified, probabilities must follow the order of factor levels.}
}

\value{
  \item{predict}{The classified factors of the set.}
}

\references{
Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

Venabless, W. N. and Ripley, B. D. \emph{Modern Applied Statistics with S.} Fourth edition. Springer, 2002. 

Mingoti, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Ferreira, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
}

\author{
Paulo Cesar Ossani
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
class <- data[,c(r+1)] # classes names

## Data training example
res <- lda(data = data[,1:r], test = NA, class = class, 
           type = "train", method = "moment", prior = NA)

resp <- results(orig.class = class, predict = res$predict)

message("Mean squared error:"); resp$mse
message("Mean absolute error:"); resp$mae
message("Relative absolute error:"); resp$rae
message("Confusion matrix:"); resp$conf.mtx  
message("Hit rate: ", resp$rate.hits)
message("Error rate: ", resp$rate.error)
message("Number of correct instances: ", resp$num.hits)
message("Number of wrong instances: ", resp$num.error)
message("Kappa coefficient: ", resp$kappa)
message("General results of the classes:"); resp$res.class 


## Data test example
class.table <- table(class) # table with the number of elements per class
prior <- as.double(class.table/sum(class.table))
test = as.integer(rownames(data.test)) # test data index

res <- lda(data = data[,1:r], test = test, class = class, 
           type = "test", method = "mle", prior = prior)

resp <- results(orig.class = class.test, predict = res$predict)

message("Mean squared error:"); resp$mse
message("Mean absolute error:"); resp$mae
message("Relative absolute error:"); resp$rae
message("Confusion matrix: "); resp$conf.mtx  
message("Hit rate: ", resp$rate.hits)
message("Error rate: ", resp$rate.error)
message("Number of correct instances: ", resp$num.hits)
message("Number of wrong instances: ", resp$num.error)
message("Kappa coefficient: ", resp$kappa)
message("General results of the classes:"); resp$res.class  

}

\keyword{Linear discriminant analysis}
