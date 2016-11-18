## Put comments here that give an overall description of what your
## functions do


# The following Will compute and cache the inverse of a square invertible matrix
# in order to recall it later without needing to recompute it. 
# Tested with the 2X2 identity matrix and the 
# 2X2 matrix:
# [4, 7]
# [2, 6]
#################################################################################
makeCacheMatrix <- function(xmatrix = matrix()) {
    mymatrix <- NULL
    setmatrix <-function(yourmatrix) {
        xmatrix <<- yourmatrix
        mymatrix <<- NULL
    }
    getmatrix <- function() xmatrix
    set_invert_matrix <- function(solve) mymatrix <<- solve
    get_invert_matrix <- function() mymatrix
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
        set_invert_matrix = set_invert_matrix,
        get_invert_matrix =get_invert_matrix)

}


# This will determine if the inverse of the input matrix is cached.  If it
# is not, it will compute the inverse and then cache is, otherwise it will
# return the cached matrix
cacheSolve <- function(xmatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        mymatrix <- xmatrix$get_invert_matrix()
        #if this has been cached already, retrieve it
        if(!is.null(mymatrix)) {
            message("Returning cached data for this inverse matrix")
            return(mymatrix)
        }
        #otherwise need to compute the matrix
        message("Computing the inverse of the matrix")
        matrix <- xmatrix$getmatrix()
        mymatrix <- solve(matrix)
        xmatrix$set_invert_matrix(mymatrix)
        mymatrix    
}
