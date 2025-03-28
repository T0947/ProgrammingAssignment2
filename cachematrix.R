## Calculate the inverse of the matrix and cache it to save computation time
## Cache the Matrix x and its inverse 
makeCacheMatrix <- function(x = matrix()) { # takes matrix as input
        inverse <- NULL # placeholder for storing the cached inverse
        set <- function(y) { # helper function that allows to update x with y 
                x <<- y # <<- ensures changes persist beyond the function's local scope.
                inverse <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) inverse <<- solve # stores the inverse 
        getinverse <- function() inverse # gets the cached inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Calculate the inverse of matrix x, if the inverse is not yet cached
cacheSolve <- function(x, ...) { # input is matrix and functions makeCacheMatrix
        inverse <- x$getinverse() # checks if the inverse is cached first
        if(!is.null(inverse)) { # if inverse was cached returns the cached inverse to avoid computing it again
                message("Getting cached inverse matrix.")
                return(inverse)
        }
        matrix <- x$get() # gets the matrix stored in 'x' 
        inverse <- solve(matrix,...) # solve(a, b) default b is identity matrix      
        x$setinverse(inverse) # caches the newly computed inverse matrix
        inverse
}
