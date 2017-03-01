
## This is a function  created to make functions to get, set, set the
##inverse and get the inverse of the matrix you inform as an argument
  
 makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse)
                inv <<- inverse
        getinverse <- function() inv
        list( set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}
  
  
## Write a short comment describing this function
## This function will verify if there is already an inverse of the matrix cached
## and return it if there is such a matrix cached.
## If there is none, the function will take the functions created in the function
## above in order to cache the inverse of the matrix in the value "inv" after 
## finding it with the "solve" function.
  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Returning the cached inverted matrix")
                inv
        }
        nmatrix <- x$get()
        inv <- solve(nmatrix, ...)
        x$setinverse(inv) ## caching the inverse of the matrix
        inv ## Return a matrix that is the inverse of 'x'
}
