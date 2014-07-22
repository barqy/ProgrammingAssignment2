## The first function creates a special matrix, which can store its inverse.
## The second function inverts the special matrix if the invesre was not cached yet,
## otherwise it uses the cache.

## Accept a matrix, and create a special matrix,
## which can store its inverse.

makeCacheMatrix <- function(X = matrix()) {
        invX <- NULL
        set <- function(Y) {
                X <<- Y
                invX <<- NULL
        }
        get <- function() X
        setInv <- function(invY) invX <<- invY
        getInv <- function() invX
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Invert the special matrix if it was not inverted yet and store the
## inverse in the cache. If the matrix was already inverted, use cached inverse.

## To test this function use the following code:
## > magic3 = matrix(c(2,9,4,7,5,3,6,1,8),3,3)
## > magic3
##      [,1] [,2] [,3]
## [1,]    2    7    6
## [2,]    9    5    1
## [3,]    4    3    8

## If you have magic package, you could simply type:
## magic3 = magic(3)

## Create a special matrix:
## > x = makeCacheMatrix(magic3)

## On first call, inverse the matrix:
## > cacheSolve(x)
##             [,1]        [,2]        [,3]
## [1,] -0.10277778  0.10555556  0.06388889
## [2,]  0.18888889  0.02222222 -0.14444444
## [3,] -0.01944444 -0.06111111  0.14722222

## On subsequent calls, it will use cache:
## > cacheSolve(x)
## getting cached data
## ...

cacheSolve <- function(X, ...) {
        invX <- X$getInv()
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        mat <- X$get()
        invX <- solve(mat)
        X$setInv(invX)
        invX
}


