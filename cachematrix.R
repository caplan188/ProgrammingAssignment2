## Takes input in matrix form and allows for the setting and getting of that matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) { ##general function, defines input as matrix
  i <- NULL     ##sets 'i' internally
  set <- function(y) { ##function to set global values for 'x' and 'i'
    x <<- y
    i <<- NULL 
  }
  get <- function() x ## prints original matrix when called
  setinverse <- function(inverse) i <<- inverse ##Sets 'i' to inverse matrix of 'x', called by cacheSolve if 'i' is null
  getinverse <- function() i ## prints 'i', either null or inverted matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ##defines list of functions
}
##Checks if inverse has been calculated, calculates or prints it as needed

cacheSolve <- function(x, ...) { ##function to get or set value of 'i'
  i <- x$getinverse() ##gets 'i' internally from x's getinverse() function
  if(!is.null(i)) { ##checks if 'i' is null, if not prints message, then 'i'
    message("getting cached data")
    return(i)
  }
  data <- x$get() ##internmally defines matrix
  i <- solve(data, ...) ## internally sets 'i' to inverse matrix value
  x$setinverse(i) ##globally sets 'i' value
  i ## prints i
}
