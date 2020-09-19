## Since matrix inversion is usually a costly computation, we will write a pair 
## of functions that cache the inverse of a matrix. This functions will be
## makeCacheMatrix and cacheSolve.

## makeCacheMatrix will store the inverse of a matrix once it is calculated
## by cacheSolve. The next time we asked for the inverse of that matrix,
## we will look for that in makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        #i will the inverse of the matrix x
        i <- NULL
        #the setter for matrix x. Not needed the first time matrix x is created
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #the getter for matrix x. Will be used by makeCacheInverse()
        get <- function() x
        #the setter for i provided by chacheSolve() when it calculates the inverse matrix of x for the first time
        setinverse <- function(inverse) i <<- inverse
        #the getter for i. cacheSolve() will find the inverse matrix of x from here if it had been calculate before
        getinverse <- function() i
        #the result of the makeCacheMatrix() will be a list where the outputs of the functions it contains have been named
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will be the function that we will used to check if we had
## calculated and stored the inverse matrix before in makeCacheMatrix.
## If we can find the inverse matrix we will the stored result. If not,
## we will calculate (and print) it and store the result in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        #cacheSolve() will try to find if the inverse matrix of x had been calculate before and stored in makeCacheMatrix()
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #if it had not been calculate then cacheSolve() will use the function Solve.
        #Solve() will get the matrix x from makeCacheMatrix()
        data <- x$get()
        i <- solve(data, ...)
        #Once it is calculated the output of the inverse matrix x will be stored in makeChacheMatrix()
        x$setinverse(i)
        #the result will be printed in the console
        i
}