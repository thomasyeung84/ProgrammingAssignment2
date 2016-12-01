## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## set the object inv is an empty object
        set <- function(y) {
                x<<-y
                inv <<- NULL
        } ## create a function 'set' to change the matrix refering to the special 'matrix created by the function
        get <- function () x ## create get function to allow calling the original matrix from the special 'matrix'
        setinv <- function(s) inv <<- s ## setinv make object inv equal to 's' 
        getinv <- function () inv ## getinv is a function to call the stored inv values
        list( set = set, get = get, setinv = setinv, getinv = getinv) ## this line create a list (i.e. the special 'matrix') that contain the 4 functions create in previous step.
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## if the inverse has already been calculated (and the matrix has not changed), then the coachSolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        inv <- x$getinv () ## set the variable inv by calling the values from the special 'matrix'. 
        if (!is.null(inv)) {
                message("Getting cached data")
                return (inv)
        } ## if the stored inv in the speical matrix is not empty, this condition print out the message and show the inv values on the console
        data <- x$get () ## if the getinv is empty, i.e. it has not be calculated before, a variable 'data' is created, which is equal to the value of the targeting matrix, which set up in makeCacheMatrix function.
        inv <- solve(data ,...) ## this step calculates the inverse of the targeting matrix. The restuls is stored as 'inv'
        x$setinv(inv) ## this step runs the function of setinv, which assign 'inv' (i.e. the inverse of the targeting matrix) into the special 'matrix'. 
        inv ## Return a matrix that is the inverse of 'x'
}
