## This pair of functions allow the user to store and compute operations
## for a specified matrix.

## Function that creates a special "matrix", which is really a list containing
## a function to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Function that verifies if there is a cached matrix in 'x' and returns it,
## otherwise computes the inverse of 'x'  with the `solve` function in R
## and  saves the result in 'x' and returns it.
cacheSolve <- function(x, ...) {
        
        ##if there is a cached inverse matrix returns the same result.
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        
        ##Compute the inverse of the original square matrix in 'x'.
        originalMatrix <- x$get()       
        i <-  solve(originalMatrix)
        ##Set the result of the inversion 'x'
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
