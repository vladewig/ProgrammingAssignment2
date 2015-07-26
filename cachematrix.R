### This file name is "cachematrix.R"
### Calculating the inverse of a matrix is usually computationally expensive - more so when this inverse is used repeatedly
### The two functions when used together will calculate the inverse of an invertible matrix and cache the result for future use
### The command - source("cachematrix.R") - will make the two functions available
### The functions are "makeCachematrix" and "cacheSolve"


## "makeCacheMatrix" is called first in your code - from outside of any loop requiring repeated use of a matrix inverse
## "makeCacheMatrix" is passed the matrix to be inverted
## "makeCacheMatrix" has a list of functions as its output
##
## "cacheSolve" is called within the repeated code e.g. where the "base" package function "solve" would usually be used
## "cacheSolve" is passed the list of functions created by "makeCacheMatrix"
## "cacheSolve" returns any cached inverse - if no cached inverse is found it is calculated then stored in the list and returned


# "makeCacheMatrix" takes an ivertible matrix as its argument and returns a list - the components of which are functions
# The names of the components of the list are:
#     $set, $get, $setinv, and $getinv
# Use: "theList <- makeCacheMatrix(theMatrixtoInvert)"

makeCacheMatrix <- function(aMatrix = matrix()) {
    # Start with a null inverse
    theInverse <- NULL
    
    # need to ensure that "aMatrix" is available to the parent environment
    set <- function(y){
        aMatrix <<- y
        theInverse <<- NULL
    }
    
    # The function "get" returns "aMatrix"
    get <- function() aMatrix
    
    # "setinv" function assigns the passed argument to "theInverse" in the enclosing environment
    setinv <- function(solve) theInverse <<- solve
    
    # "getinv" function returns "theInverse"
    getinv <- function() theInverse
    
    # Construct the list 
      list(set = set,
           get = get,
           setinv = setinv,
           getinv = getinv)
}




# "cacheSolve" takes, as input, the output of "makeCacheMatrix" - a list
# The inverse matrix is returned as output
# Use: "theInverseMatrix <- cacheSolve(theList, ...)"

cacheSolve <- function(theList, ...) {
    # First get whatever is in the cache
    invCache <- theList$getinv()
    
    # Check if the cache holds an inverse if so finish early and return that value as output to the function
    if (!is.null(invCache)) {
        message("getting cached data")
        return(invCache)
    }
    
    # If the cache is empty then get the matrix stored by "makeCacheMatrix" and calculate the inverse
    data <- theList$get()
    invCache <- solve(data, ...)
    
    # Store the inverse in the cache
    theList$setinv(invCache)
    
    #Return the calculated inverse
    invCache
}

# EXAMPLE
# Matrix3x3 <- matrix(c(7, 0, -3, 2, 3, 4, 1, -1, -2), nrow = 3, ncol = 3)
# theList <- makeCacheMatrix(Matrix3x3)
# for (i in 1:3) {
#     theInverseMatrix <- cacheSolve(theList)
#     message("str theInverseMatrix in for loop")
#     str(theInverseMatrix)
# }




