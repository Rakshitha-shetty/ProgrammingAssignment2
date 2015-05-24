
## Caching the inverse of a matrix is efficient/less time consuming rather than compute it repeatedly.
## Includes Functions for creating and using inverted matrices with caching ability.




## makeCacheMatrix function creates a "matrix" that can cache its inverse.
## Includes list containing a function to set and get the values of matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {

    inverted.matrix <- NULL

    set <- function(y) {
        x <<- y
        inverted.matrix <- NULL
    }

    get <- function() x

 #Inversing the matrix using solve() function in R
    set_inverse <- function(solve) inverted.matrix <<- solve
    get_inverse <- function() inverted.matrix
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)

}



## The following function returns the inverse of the matrix. checks if the
## inverse has already been computed. If so, it gets the result and skips the computation.Else,
## it computes the inverse, sets the value in the cache via setinverse function.



cacheSolve <- function(x, ...) {
    inverted.matrix <- x$get_inverse()

    if(!is.null(inverted.matrix)) {
        message("Getting cached inverted matrix")
        return(inverted.matrix)

    }

# Create inverted matrix if there's no cached matrix available.
    data <- x$get()
    inverted.matrix <- solve(data)
    x$set_inverse(inverted.matrix)
    inverted.matrix
}

