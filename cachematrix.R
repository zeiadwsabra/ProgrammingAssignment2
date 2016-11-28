#The following functions create a Matrix and compute its inverse by the use of caching 
##to avoid expensive computations.

##This Function creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { #sets the matrix to y
                x <<- y
                inv <<- NULL
        }
        get <- function() x # returns the matrix
        setinverse <- function(inverse) inv <<- inverse #sets the inverse of the matrix
        getinverse <- function() inv
        list(set = set, get = get,
                       setinverse = setinverse,
                       getinverse = getinverse)
        
}


## function calculates the inverse of the special "matrix" created with the above function.
##However, it first checks to see if the inverse has already been calculated. If so
##, it gets the inverse from the cache and skips the computation. Otherwise,
##it calculates the inverse of the data 
##and sets the value of the inverse in the cache via the setinvere function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) #computes the inverse of matrix
        x$setinverse(inv)  #caches the inverse of the matrix
        inv
        
}
