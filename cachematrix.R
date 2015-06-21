## This file contains two functions makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.
##
## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above making use of the cache if available. 
##
## Example Usage :
##
## m <- matrix(c(4,3,3,2),2,2)
## m2 <- makeCacheMatrix(m)
## cacheSolve(m2)
##

## The below function takes in a matrix and returns a special matrix object, 
## which is really a list containing the following 4 functions :
## set : to set the value of the matrix
## get : to get the value of the matrix
## setinverse : to set the value of the inverse
## getinverse : to get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #i stores the cached value of inverse
    set <- function(y) {
        x <<- y #setting the matrix
        i <<- NULL #resetting cached value
    }
    get <- function() x 
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## The following function calculates the inverse of the special matix created by
## the above function. If the inverse has already been computed before it is 
## retrieved from the cache, else it is computed using the solve function and 
## then stored in the cache using the setinverse function. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i)) { #checking if value is present in cache
        message("getting cached data")
        return(i) #return cached data
    }
    data <- x$get()
    i <- solve(data, ...) #computing inverse using solve function
    x$setinverse(i) #storing in cache
    i #returning the inverse
    
}
