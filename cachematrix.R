# function makeCacheMatrix creates a special matrix,
# list containing a function of:
# set the value of the matrix
# get the value of the matrix
# setSolve the value of the inverse
# getSolve the value of the inverse
# It is storing a numeric matrix and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
    # if (det(x) == 0) print("A matrix is inversible if and only if
    # its determinant is non-zero!")
    inverse <- NULL # inverse is initialise

    # assign a value to an object in an environment that is different
    set <- function(y) {
        x <<- y  # push "x" out of is lexical scope! (Global variable)
        inverse <<- NULL # inverse matrix is becoming global & initialise
    }

    get <- function() x # just get the matrix x
    # simply return the value x in its lexical scope

    setSolve <- function(solve) {
        inverse <<- solve  # inverse is globalise
    }

    getSolve <- function() inverse     # get the inverse

    # Define the new 4 list objects
    # Can be call as a list i.e. var$get()
    list( set = set,
          get = get,
          setSolve = setSolve,
          getSolve = getSolve
    )
}

# calculates the inverse of the matrix created with makeCacheMatrix
# looked up in the cache rather than redoing the calculation
# take advantage of the scoping rules of R # to preserve state
# inside of an object.

cacheSolve <- function(x, ...) {
    inverse <- x$getSolve()

    if ( !is.null(inverse) ) { # if the inverse is NOT NULL. Return inverse.
        message("getting cached data of inverse matrix")
        return(inverse) # return inverse
    }
    data <- x$get() #get the matrix

    # Computing the inverse of a square matrix
    # can be done with the solve function
    inverse <- solve(data, ...) # solve matrix to inverse. Return its inverse.
    x$setSolve(inverse)
    inverse
}


mat <- matrix(c(1, 2, 3, 4), 2, 2)
x <- makeCacheMatrix(mat)
x$get()
cacheSolve(x)
