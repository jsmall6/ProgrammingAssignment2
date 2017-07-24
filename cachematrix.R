## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #null out the object variable m
  m <- NULL
  
  #setter function
  #sets x of the parent object to be the matrix passed to argument y
  #null out the object variable m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Getter - Return x
  get <- function() x
  
  #Setter setMatrix set the function solve to the m object of the parent
  setMatrix <- function(solve) m <<- solve
  
  #getter - return the object m
  getMatrix <- function() m
  
  #setting attributes of the functions for easiers innvocation
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #call getter function of the object 'x' passed in 
  m <- x$getMatrix()
  
  #If m is null it means solve has not been invoked on the matrix yet.
  #Else m was solved previous and is in the cache of object 'x' return the previously solved inverse of 'x'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Else m is null; set data by getting 'x' and call solve on data to store in m local.
  data <- x$get()
  m <- solve(data, ...)
  #call the setter to set the solve of 'x' and effectively cache the result
  x$setMatrix(m)
  #return the solve of the matrix. Alternatively the x$getMatrix() could have been called.
  m
}
