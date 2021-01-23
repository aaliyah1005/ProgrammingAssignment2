## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
## two functions that are used to create a special object 
## that stores a matrix and cache's its inverse.

## Create a special object that stores a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}
## The pair of functions makeCacheMatrix and cacheSolve
## cache the inverse of a matrix, so that once inverse of
## a matrix is calculated, it's retrieved from the cache
## subsequently, but only until the main matrix is changed

## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    # Initialize symbol for cached inverse of matrix
    inv <- NULL
    # Initialize symbol to indicate matrix has changed
    matChanged <- TRUE
    
    # Set a matrix in the data structure
    setMatrix <- function(x) {
        m <<- x
        inv <<- NULL
        matChanged <<- TRUE
    }
    
    # Get the currently set matrix
    getMatrix <- function() m
    
    # Set the calculated inverse of a matrix, 
    # and indicate that the matrix is not changed
    setMatInverse <- function(inverse) {
        matChanged <<- FALSE
        inv <<- inverse
    }
    
    # Get the inverse of the matrix
    getMatInverse <- function() inv
    
    # Returns whether the matrix has changed or not
    isMatChanged <- function() matChanged
    
    # Return the list of functions that can be executed
    # on this data structure
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setMatInverse = setMatInverse,
         getMatInverse = getMatInverse,
         isMatChanged = isMatChanged)
}


## cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the 
## matrix has not changed), then it retrieves the inverse 
## from the cache.

cacheSolve <- function(specialMatrix, ...) {
    # Get the cached inverse of matrix
    inv <- specialMatrix$getMatInverse()
    # Ask whether the matrix has changed or not
    isMChanged <- specialMatrix$isMatChanged()
    
    # If the cached inverse is not NULL and
    # if the matrix has not changed
    if(!is.null(inv) && !isMChanged) {
        message("Getting cached inverse of matrix")
        return(inv)
    }
    
    # Get the current matrix for which inverse 
    # has been requested
    m <- specialMatrix$getMatrix()
    
    # Calculate the inverse of matrix
    inv <- solve(m, ...)
    # Set the calculated inverse in data structure
    specialMatrix$setMatInverse(inv)
    
    message("Getting fresh calculated inverse of matrix")
    inv
}

## This function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Cached matrix is returned. This has a getter and setter for the actual matrix.
# This automatically invalidates cache when the original matrix has changed in the setter.
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


## Write a short comment describing this function
#Check if the matrix has cached solution and return if there is one.
#Otherwise solve the matrix, store the solution in cache and return the solution.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
## makeCacheMatrix will cache the inverse of a matrix
## It will test for a square matrix

## get() will get the matrix of type makeCacheMatrix
## set() will set the matrix of type makeCacheMatrix and initialize its inverse
## getinverse() will get the inverse of the matrix
## setinverse() will set the inverse of the matrix

## Annie Flippo - 4/16/2014  Initial Creation
## Annie Flippo - 4/20/2014  Added comments

makeCacheMatrix <- function(x = matrix()) {
        
        x <- x
        
        # Initialize inv_x (inverse of X) to NULL 
        inv_x <- NULL
        
        # Find number of rows and columns for x
        nCol <<- ncol(x)
        nRow <<- nrow(x)
        
        # If it's not a square matrix then get out - you're all done
        if (nRow != nCol) {
                stop("It is not a square matrix")
        } 
        
	# Sets the x matrix with the argument passed in as y
	# Sets the inv_x as a matrix of a NULL
        set <- function(y) {
                x <<- y
                inv_x <<- matrix() 
        }

	# gets the original matrix 
        get <- function() {
                x
        }

        # Sets the inverse of the matrix with the argument passed in as matrixInverse
        setinverse <- function(matrixInverse) {
               inv_x <<- matrixInverse
        }

        # Gets the inverse of the matrix, inv_x
        getinverse <- function() {
                inv_x
        }

        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}


## cacheSolve will try to get the cached value of the inverse of the matrix
## otherwise it will calculate and set the inverse for the matrix

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        
        # Gets the inverse of x
        inv_x <- x$getinverse()
        
        # Check to see if the inverse of matrix is NOT NULL
	# If it's NOT NULL, return inv_x as retrieved above and ends cacheSolve function
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        
        # Inverse of matrix is NULL so calculate and set it
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinverse(inv_x)

	# Returning inv_x
        inv_x
}
