# Takes raw csv files of bacteria points across multiple frames.
# Will calculate hypha angle, bacteria midpoints, angles, angle differences, distances.
# You may download the calculations as CSV.

# Import bacteria and math for categories involving hypha.
# Make sure hyphamodel.R is run beforehand so that this is defined.
hA <- ifelse(hP2[1]-hP1[1] != 0, atan((hP2[2]-hP1[2])/(hP2[1]-hP1[1])) * (180/pi), 90)

# CHANGE THIS: Paths for each frame. Change the \ to /.
f1 <- read.csv("")
f2 <- read.csv("")
f3 <- read.csv("")

bP <- rbind(f1, f2, f3)

# Micron/pixel ratio. Adjust if needed.
umpxR <- 0.2397

# Midpoints
bP$xm <- (bP$x2 + bP$x1)/2
bP$ym <- (bP$y2 + bP$y1)/2

# Angle of each bacteria
bP$angle <- ifelse(bP$x2-bP$x1 != 0, atan((bP$y2-bP$y1)/(bP$x2-bP$x1)) * (180/pi), 90)

# |degree of difference from hypha|.
bP$absDegDif <- ifelse(abs(hA-bP$angle) <= 90, abs(hA-bP$angle),180-abs(hA-bP$angle))

# Classify absDegDif into 6 bins.
bP <- bP %>%
  mutate(degDifInterval = 
  case_when(
    bP$absDegDif >= 0 & bP$absDegDif < 15 ~ "[0°,15°)",
    bP$absDegDif >= 15 & bP$absDegDif < 30 ~ "[15°,30°)",
    bP$absDegDif >= 30 & bP$absDegDif < 45 ~ "[30°,45°)",
    bP$absDegDif >= 45 & bP$absDegDif < 60 ~ "[45°,60°)",
    bP$absDegDif >= 60 & bP$absDegDif < 75 ~ "[60°,75°)",
    bP$absDegDif >= 75 & bP$absDegDif <= 90 ~ "[75°,90°]",
))

# Bacteria distance from hypha based on midpoints.
bP$hyphaDist <- abs(((bP$ym-hP1[2])*(hP2[1]-hP1[1])-(bP$xm-hP1[1])*(hP2[2]-hP1[2]))/sqrt((hP2[1]-hP1[1])^2+(hP2[2]-hP1[2])^2)) * umpxR

# Classify hyphaDist into bins of size 50.
bP$distInterval <- cut(bP$hyphaDist, seq(0,max(bP$hyphaDist)+50,50))

#CHANGE THIS: Download CSV.
write.csv(bP,"") 