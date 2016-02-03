## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    new_matrix <- NULL # create a new variable 'm' within the current environment inside makeCacheMatrix() function
    set <- function(y) { # set the value of the matrix
        x <<- y # assign value of variable 'x' in the current environment to variable 'y' in the parent environment outside makeCacheMatrix() function
        new_matrix <<- NULL # assign value of variable 'm' in the current environment to be NULL in the parent environment outside makeCacheMatrix() function
    }
    get <- function() x # get the value of the matrix
    setinverse <- function(solve) new_matrix <<- solve # set the value of the inverse of the matrix
    getinverse <- function() new_matrix # get the value of the inverse of the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    new_matrix <- x$getinverse()
    if(!is.null(new_matrix)) { # new_matrix has already been calculated...
        message("getting cached data") # this message given
        return(new_matrix) # and inverse of matrix is retrieved
    }
    data <- x$get() # if new_matrix has not been calculated, get the matrix 'x'
    new_matrix <- solve(data, ...) # solve new_matrix for its inverse
    x$setinverse(new_matrix) # set the value of the inverse 
    new_matrix # give back the inverse of new_matrix
}
