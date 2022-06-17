huber_loss <- function(diff, d=1) {
  if (abs(diff) < d) {
    return(0.5 * diff ** 2)
  } else {
    return(0.5*d**2 + d * (abs(diff) - d))
  }
}

x = -100:100/70 
y = mapply(huber_loss, x)

p1 <- ggplot(data.table(x=x, y=y), aes(x=x, y=y)) + geom_point(color="blue")

p2 <- ggplot(data.table(x=x, y=y), aes(x=x, y=y)) + geom_point(color="yellow")

v <- c(p1, p2)
class(v)

library(data.table)
dt <- data.table(aaa=c(1,2,2), bbb=c(3,4,5))
dt[aaa == 2, aaa := aaa + bbb]
dt



# dt <- data.table(y=c(0, 0, 1, 1))
y <- c(0, 0, 1, 1, 1)

log_loss <- function(y, p, eps = 10^(-8)) {
  sum(y * log(p + eps) + (1.-y) * log(1.-p + eps))
}

rmse <- function(y, p) {
  sqrt(sum((y - p)**2))
}

mae <- function(y, p) {
  sum(abs(y - p))
}


solver <- function(f, min_x=0., max_x=1., obj=0., eps=10^(-6)) {
  cur_x <- (min_x + max_x) / 2.
  cur_y <- f(cur_x)
  if ((f(max_x) - obj) * (f(min_x) - obj) > 0) {
    stop("objective not reachable")
  }
  while (abs(cur_y - obj) > eps) {
    if ((cur_y - obj) * (f(min_x) - obj) < 0) {
      max_x <- cur_x
    } else {
      min_x <- cur_x
    }
    cur_x <- (min_x + max_x) / 2.
    cur_y <- f(cur_x)
  }
  cur_x
}

find_stationary_point <- function(f, min_x=0., max_x=1., obj=0., eps=10^(-8)) {
  diff_f <- function(x) (f(x+eps/2) - f(x-eps/2)) / eps
  solver(diff_f, min_x, max_x, obj, eps)
}

eps <- 10^(-6)

solver(function(x) (x^2 - 0.5))
find_stationary_point(function(x) rmse(y, x))
find_stationary_point(function(x) mae(y, x))
find_stationary_point(function(x) -log_loss(y, x))

p_l <- 0:100/100
plot(p_l, mapply(function(x) log_loss(y, x), p_l))
plot(p_l, mapply(function(x) rmse(y, x), p_l))
plot(p_l, mapply(function(x) mae(y, x), p_l))



log_loss(y, 0.99)

solver(function(x) (rmse(y, x+eps/2) - rmse(y, x-eps/2)) / eps)
solver(function(x) (rmse(y, x+eps/2) - rmse(y, x-eps/2)) / eps) 


