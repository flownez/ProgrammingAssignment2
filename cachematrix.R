## This set of functions includes an object to store a invertible matrix and
## its inverse, as well as a function to return the objects inverse matrix, 
## calculating and caching the inverse matrix if it has not been calculated 
## previously

## This function creates an object for storing a matrix and its inverse.
## It provides access to these matricies via getter and setter functions. 
## These are exposed to the parent environment via a list containing 
## 4 symbols linked to the anonymous getter/setter functions.
makeCacheMatrix <- function(x = matrix()) {
        cached_inv_matrix <- NULL
        
        ## Base matrix setter
        set <- function(new_matrix){
                ## Assign new_matrix to an environment variable in the
                ## parent scope
                x <<- new_matrix
                
                ## Clear the cached inverted matrix as it is now invalid
                cached_inv_matrix <<- NULL
        }
        
        ## Base matrix getter
        get <- function() {
                #return the base matrix
                x
        }
        
        ## Inverted matrix setter
        setinvertedmatrix <- function (inv_matrix){
                ## Set the cached inverted matrix
                cached_inv_matrix <<- inv_matrix
        }
        
        ## Inverted matrix getter
        getinvertedmatrix <- function(){
                ## Return the cached inverted matrix
                cached_inv_matrix
        }
        
        # The list object that gives access to the internal environment 
        # functions:
        list( set = set,
              get = get,
              setinvertedmatrix = setinvertedmatrix,
              getinvertedmatrix = getinvertedmatrix
              )
}

## This function returns the inverse of the matrix found in the 'x' object.
## If a cached version of the inverted matrix exists then it is returned, 
## otherwise the inverse of the matrix is calculated, cached in 'x', and 
## returned.  This function assumes object x contains an invertible matrix.
cacheSolve <- function(x, ...) {
        ## Get the inv_matrix stored in object x
        inv_matrix = x$getinvertedmatrix()
        
        ## Check if the inv_matrix is valid, if so then return it
        if (!is.null(inv_matrix)){
                message("Returning cached inverted matrix")
                return(inv_matrix)
        }
        
        ## Calculate the inverse of the matrix in object x 
        data <- x$get()
        inv_matrix <- solve(data)
        
        ## Cache it
        x$setinvertedmatrix(inv_matrix)
        
        ## Return it
        message("Returning newly calculated inverted matrix")
        inv_matrix
}
