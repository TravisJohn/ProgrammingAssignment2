## Put comments here that give an overall description of what your
## functions do

## the -makecachematrix- function is a customised function the accepts a matrix. Where you can set the matrix
## definition and immediately return a list of numeric values of mean per column



makeCacheMatrix <- function(x = matrix()) {
  #creating an object with NULL values
  m <- NULL
  
  
  # setting the input argument to the x object in the parent environment
  # assigning the null to the M object in the parent environment - This line of code clears any value of m that had been cache
  #prior to the execution of cache mean
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #retrieves the x from the parent environment
  get <- function() x
  
  #define the setter for the mean ,
  setmean <- function(colMeans) m <<- colMeans
  getmean <- function() m
  
  #summarizing the function by putting it as an element of the list
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


## Write a short comment describing this function


#cache solve  is needed to populate and or retrieve mean from makecachematrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  #check to see if it is null where if this is not equal to null then cache mean is returned to the parent
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if it is false, then the cache mean gets the vector from input object calculates a mean and uses the setmean function
  #on the input object to set the mean in the input object and then returns the value of the
  #mean to the parent environment by printing the mean of object
  
  
  data <- x$get()
  m <- colMeans(data, ...)
  x$setmean(m)
  m
}


#to test my code try running -

# Matr <- makeCacheMatrix(matrix(c(1:16), c(4,4)))
# Matr$get()
# Matr$getmean()
# Matr$set(matrix(c(1:4),c(2,2)))
# cacheSolve(Matr)
# Matr$getmean()













