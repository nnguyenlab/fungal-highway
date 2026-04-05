# Takes a raw csv file of hypha points.
# Will create a linear model and set up hypha endpoints for bacteriamath.
# Will need to manually change the video title and mess with the model prediction.

# Import hypha coordinates. Format: x,y
coordinates <- read.csv("")

# Name video title for graph.
videoTitle <- ""

hx <- coordinates$x
hy <- coordinates$y

#SWITCH THIS: (hy ~ hx) depending on where hypha ends (The frames are 3072x2048).
#predict(lm(hx ~ hy), data.frame(hy = c(1)))
hP1 <- c(predict(lm(hx ~ hy), data.frame(hy = 0)), 0)
hP2 <- c(3072, predict(lm(hy ~ hx), data.frame(hx = 3072)))

