## Caching inverser of matrix (assignment week 3)

## creates matrix that can cache inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) { #this functions sets x and i in the parent evironment
    x <<- y
    i <<- NULL
  }
  get <- function() x #this function gets the matrix x
  setinverse <- function(solve) i <<- solve #this function sets the inverse
  getinverse <- function() i #this function retrieves the inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #this line is needed so you can later use the $ operator because this doesn't work on atomic vectors so you need to name your functions
}


## computes inverse and retrieves if already been computed before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() #this sets i in this local environment as the element x of the get inverse function from the previous environment
  if(!is.null(i)) { #if this local i is not NULL (which will only happen if we have something in cache because we previously set the i in the previous environment as Null so unless we replace it with a cached value it will be Null)
    message("getting cached data") #then the message "getting cached data" is printed and the cached value is returned
    return(i)
  }
  data <- x$get() #this is the part where we actually invert the matrix when running the function for the first time; it starts by retrieving the input matrix using the get function and storing it as an object
  i <- solve(data, ...) #then we use the solve function on that object to invert it
  x$setinverse(i) #then we store this inverted matrix
  i
}

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

b1 <- makeCacheMatrix(m1)
cacheSolve(b1)
