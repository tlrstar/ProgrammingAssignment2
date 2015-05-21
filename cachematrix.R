## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                       #Assign m to null
  set <- function(y) {            #create set function
    x <<- y                       #x holds the matrix y
    m <<- NULL                    #m is null
  }
  get <- function() x

  #setinverse is a function that holds values (in this case the inverted matrix)
  setinverse <- function(invmatrix) m <<- invmatrix  

  #getinverse is a function that gets values (in this case also the inverted matrix)
  getinverse <- function() m

  list(set = set, get = get,      #return a list with all the functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()     #assign the value from the cache to m
  if(!is.null(m)) {       #if m is assigned a value, great! it's already cached!
    message("getting cached data")
    return(m)             #return the inverted matrix
  }
  data <- x$get()         #else, get the matrix and assign it to data
  m <- solve(data, ...)   #calculate the inverse of the matrix 'data', assign to m
  x$setinverse(m)         #make setinverse equal to the inverted matrix (i.e. cache it)
  m                       #return the inverse matrix
}
