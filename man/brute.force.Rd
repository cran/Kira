\name{brute.force}

\alias{brute.force}

\title{Brute force method for variable selection.}

\description{Brute force method used to determine the smallest number of variables in a supervised classification model.}

\usage{brute.force(func = NA, train, test, class.train,
            class.test,  args = NA, measure = "Rate Hits", 
            output = 10)}

\arguments{
  \item{func}{Supervised classification function to be analyzed.}
  \item{train}{Data set of training, without classes.}
  \item{test}{Test data set.}
  \item{class.train}{Vector with training data class names.}
  \item{class.test}{Vector with test data class names.}
  \item{args}{Argument using in the classifier giving in 'func'.} 
  \item{measure}{Measure to evaluate the model:
                 "Rate Hits" (default), "Kappa Index", "Sensitivity",
                 "Specificity", "Precision", "FP Rate", "FN Rate",
                 "Negative Predictive Rate", "F-Score", "MCC",
                 "ROC Are" or "PRC Area".\cr
                 If measure = NA returns all metrics ordered by 'Rate Hits'.}
  \item{output}{Number of elements with the best combinations of variables in the matrix 'best.model' (default = 10).}
}

\value{
 \item{best.model}{Matrix with the names of the best combinations of variables, according to the evaluation measure used: accuracy, precision, recall etc.} 
 \item{text.model}{Structure of the classification model used.}
}

\author{Paulo Cesar Ossani}


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

res <- brute.force(func = "knn", train = data.train[,1:r], 
                   test = data.test[,1:r], class.train = class.train,  
                   class.test = class.test, args = "k = 1, dist = 'EUC'", 
                   measure = "Rate Hits", output = 20)
res$best.model
res$text.model

res <- brute.force(func = "regression", train = data.train[,1:r], 
                   test = data.test[,1:r], class.train = class.train, 
                   class.test = class.test, args = "intercept = TRUE", 
                   measure = "Rate Hits", output = 20)
res$best.model
res$text.model

test_a <- as.integer(rownames(data.test)) # test data index
class  <- data[,c(r+1)] # classes names
res <- brute.force(func = "lda", train = data[,1:r], test = test_a, 
                   class.train = class, class.test = class.test, 
                   args = "type = 'test', method = 'mle'", 
                   measure = "Rate Hits", output = 20)
res$best.model 
res$text.model
}

\keyword{brute.force}
