## Put comments here that give an overall description of what your
## functions do


## this function creates a special matrix object that can cache its inverse.
MakeCacheMatrix <- function(x = matrix()) 
{
   z <- NULL
   
   set <- function (y)
   {
      x <<- y
      z <<- NULL
   }
    
   get <- function() x
   
   setinv <- function(answer) z <<- answer
   
   getinv <- function() z
   
   list(set = set, get = get, setinv = setinv, getinv = getinv)   


}


## Write a short comment describing this function
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'

  z <- x$getinv()
  if(!is.null(z))
  {
    message("Getting Cached Data")
    return (z)
  }
  else
    {
      result <- x$get()
      z <- solve(result, ...)
      x$setinv(z)
      return(z)
  }
}
