## Put comments here that give an overall description of what your
## functions do

## 创建可缓存逆矩阵的特殊“矩阵”对象

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## 用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。如果已经计算逆
##矩阵（且尚未更改矩阵），那么cachesolve将检索缓存中的逆矩阵

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() ##首先把getsolve赋值给m，如果m不是NULL的话弹出下列提示
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
