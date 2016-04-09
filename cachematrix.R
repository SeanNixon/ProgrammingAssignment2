## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      ## This function creates a list with the following four functions
      #     setMatrix(x):     Caches the matrix x and clears any saved inverse.
      #     getMatrix():      The matrix x is saved in this function to be retrieved later
      #     setInv():         Caches the inverse matrix as xInv
      #     getInv():         Calls the set inverse matrix, xInv.
      ## This List can then be used in place of x. 
      
      #Clear out any possible saved data.
      xInv <- NULL
      setMatrix <- function(y) {
            x <<- y
            xInv <<- NULL
      }
      getMatrix <- function() x
      setInv <- function(inv) xInv <<- inv
      getInv <- function() xInv
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setInv = setInv,
           getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(mCM) {
      ## This function calculates and returns the inverse of the saved matrix x.
      
      ## Try to retrieve the inverse matrix xInv.
      xInv <- mCM$getInv()
      ## If it's there, return the previously computed inverse matrix.
      if(!is.null(xInv)) {
            message("getting cached data")
            return(xInv)
      }
      ## Otherwise calculate the inverse of the matrix stored in mCM
      xInv <- solve(mCM$getMatrix())
      x$setInv(xInv)
      xInv
}
