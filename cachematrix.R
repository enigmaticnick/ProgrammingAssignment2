makeCacheMatrix <- function(x = matrix()) 
{
	inv <- NULL
	init <- function(y)
	{
		x <<- y
		inv <<- NULL
	}
	get <- function() 
	{
		x
	}
	setinv <- function(z)
	{
		inv <<- z
	}
	getinv <- function()
	{
		inv
	}
	list(init = init, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) 
{
        inv <- x$getinv()
        if(!is.null(inv))
	{
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
