#  This program does 2 things:
#  1. Compute the inverse of a matrix
#  2. Retrieve this inverse from memory if the same matrix's inverse is requested
#  instead of re-computing the inverse
# 
# The program does this by creating a custom S3 object of the "makeCacheMatrix"
# class from a matrix input. This object is then passed to the cacheSolve() 
# function which either freshly computes the inverse of the matrix or simply
# returns a cached inverse if the input matrix is the same as from the previous
# computation.
# 
# The program uses the in-built solve() function, which returns the inverse of a
# matrix when only one matrix is provided as an input argument to it.
# 
# 
# ## Write a short comment describing this function
# The makeCacheMatrix function takes a matrix as its argument and returns a list
# that contains "setters" and "getters" for the matrix and its inverse. The
# function uses the variable "flag" as a flag to determine whether a fresh inverse
# computation is necessary.
# 
# The value of the flag variable is set to null when:
# 1. A new makeCacheMatrix object is created, or 
# 2. The setter method of an existing makeCacheMatrix object is invoked (thereby
# changing its value)
# Otherwise, the flag variable stores an inverse matrix. The state of the flag
# variable is used to determine whether a new inverse computation is necessary.
#
# This function heavily uses R's lexical scoping rules in that 4 functions are 
# defined within makeCacheMatrix and the variables used in each of the functions
# are defined in the functions' parent environment (makeCacheMatrix).
# 
makeCacheMatrix <- function(x = matrix()) {
    flag <- NULL
    set <- function(y) {
        x <<- y
        flag <<- NULL
    }
    get <- function() x
    setflag <- function(inv) flag <<- inv
    getflag <- function() flag
    list(set = set, get = get,
         setflag = setflag,
         getflag = getflag)
}



## Write a short comment describing this function
# 
# The cacheSolve function takes a makeCacheMatrix object as its argument and
# returns the inverse of the matrix described in the makeCacheMatrix object.
# The function checks the value of the flag variable "flag" to determine whether
# the inverse has to be computed freshly using the one-argument variant of the 
# solve() function.
# 
# If the inverse of say matrix "a" was just computed and the user requests the
# inverse of the same matrix again, the result is not computed but simply 
# "fetched" from memory. This is possible due to the fact that the 
# myCacheMatrix environment is kept in memory because the myCacheMatrix object
# (the one that is passed as an argument to cacheSolve()) references the 
# functions within the myCacheMatrix environment.
# 
# As a result, cacheSolve can access the variables in this environment to 
# determine the flag variable's state with the "getter" method and decide
# whether the inverse needs to be re-computed or not. cacheSolve can also write
# a value into the flag variable with the "setter" method setflag().
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    f <- x$getflag()
    if(!is.null(f)) {
        message("getting cached data")
        return(f)
    }
    data <- x$get()
    f <- solve(data, ...)
    x$setflag(f)
    f
}
