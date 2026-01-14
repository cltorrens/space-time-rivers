diff(1:20, 3)
diff(1:10, 2, 2)
x <- cumsum(cumsum(1:10))
diff(x)
diff(x, lag = 2)
diff(x, differences = 2)

diff(.leap.seconds)

seq(1,20, by=2)
