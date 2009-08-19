# N: number of training data
# s: number of hidden unit
# l: learning rate parameter

N=21
s=10
l=0.03

# generate traning data
x <- seq(0,1,by=1/(N-1))
t <- sin(2*pi*x)+rnorm(N,sd=0.25)
plot(x,t)

w1 <- matrix(0.7,s,2)
w2 <- matrix(0.4,1,s+1)

xt <- seq(0,1,by=0.025)
yt <- array()
for (i in 1:41){
  a <- w1 %*% c(1, xt[i])
  z <- tanh(a)
  y <- w2 %*% c(1,z)
  yt[i] <- y
}
plot(xt, yt)

for (k in 1:10) {
for (i in 1:N) {
  # forward propagation
  a <- w1 %*% c(1, x[i])
  cat("===a===\n")
  print(a)
  z <- tanh (a)
  cat("===z===\n")
  print(z)
  y = w2 %*% c(1, z)
  cat("===y===\n")
  print(y)

  # back propagation
  d2 = y - t[i]
  cat("===d2===\n")
  print(d2)
  w2 = w2 - l * d2 * c(1, z)

  d1 = d2 %*% (1-tanh(t(a))^2) * t(w2[1,1:s])
  cat("===d1===\n")
  print(d1)
  w1 = w1 - l * t(d1) %*% c(1, x[i])
  cat("===w1===\n")
  print(w1)
}
}

cat("learning end print w1 and w2\n")
print(w1)
print(w2)

# test data
xt <- seq(0,1,by=0.025)
yt <- array()
for (i in 1:41){
  a <- w1 %*% c(1,xt[i])
  z <- tanh(a)
  y <- w2 %*% c(1,z)
  yt[i] <- y
}
plot(xt, yt)