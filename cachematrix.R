## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ##Initializing the object that will hold the inverse of the matrix
        i <- NULL
        
        ##Defining the function that will set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ##Defining the function that will get the value of the matrix
        get <- function() x
        
        ##Defining the function that will set the inverse of the matrix
        setInverse <- function(solve) i <<- solve
        
        ##Defining the function that will get the inverse of the matrix
        getInverse <- function() i

        ##Returing a list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

        ##Return the inverse of matrix "x" and assign it to object "i"
        i <- x$getInverse()

        ##If the inverse of matrix "x" has already been computed and is available, 
        ##send a message that the "cached" inverse is being returned, and just return the inverse
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        ##Return the value of the matrix and assign it to object "data"
        data <- x$get()

        ##Calculate the inverse of the given matrix using Solve function and assign the value to object "i"
        i <- solve(data, ...)

        ##Set the value of the inverse of the given matrix 
        x$setInverse(i) 
        
        ##Return the value of the inverse of the given matrix
        i
}
