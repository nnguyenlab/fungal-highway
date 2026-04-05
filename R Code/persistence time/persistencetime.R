# Takes a sequence of bacteria positions over time and creates an unmerged table of vector cosines (cos) with consideration for vector lengths (pLen) by iterating through gaps of time (deltaT) and each pair of vectors in tracks.
# Able to summarize the table with a weighted average cos for each deltaT.

library(tidyverse)

# Import tracks CSV. Format: track,slice,x,y.
raw <- read.csv("")

# Create empty 'pool' df containing all deltaT and cosines.
pool <- data.frame(track = numeric(0),
                   deltaT = numeric(0),
                   cos = numeric(0),
                   pLen = numeric(0))

# Returns a df of velocity vectors with ID and filters out points that don't move.
velocity <- function(pos, t){
  v <- data.frame(track = rep(t, nrow(pos)-1), 
                  xv = (tail(pos,-1)$x - head(pos,-1)$x)/(tail(pos,-1)$slice - head(pos,-1)$slice),
                  yv = (tail(pos,-1)$y - head(pos,-1)$y)/(tail(pos,-1)$slice - head(pos,-1)$slice)
  )  
  return(v)
}

# Add any velocity vectors shorter than min with the next.
smoothen <- function(v, min){
  while (min(sqrt((v$xv)^2+(v$yv)^2)) < min){
    h <- which.min(sqrt((v$xv)^2+(v$yv)^2))
    if (h != nrow(v)){
      v$xv[h+1] <- v$xv[h+1] + v$xv[h]
      v$yv[h+1] <- v$yv[h+1] + v$yv[h]
    }
    v <- v[-h,]
    rownames(v) <- 1:nrow(v)
  }
  return(v)
}

# Returns cos(theta) between two velocity vectors.
getCos <- function(u1, u2){
  return(
    (u1[1]*u2[1] + u1[2]*u2[2])/(sqrt(u1[1]^2+u1[2]^2)*sqrt(u2[1]^2+u2[2]^2))
  )
}

# Main loop (nested nested for loop).
for (numTrack in 1:length(unique(raw$track))){ #iterate track
  subpool <- data.frame(track = numeric(0),
                   deltaT = numeric(0),
                   cos = numeric(0),
                   pLen = numeric(0))
  v <- velocity(subset(raw, track == numTrack), numTrack)#isolate one of the tracks and get velocity vectors
  print(paste("track ",numTrack," with ",nrow(v)," vectors")) #keeps track of progress 
  if (nrow(v) <= 1) next #skip track if it has less than indicated slices
  for (dt in 0:(nrow(v)-1)){ #iterate deltaT 
    for (i in 1:(nrow(v)-dt)){ #iterate through each pair of velocity vectors
      if ((v$xv[i] == 0 & v$yv[i] == 0) | (v$xv[i+dt] == 0 & v$yv[i+dt] == 0)) next
      subpool[nrow(subpool) + 1,] <- c(numTrack, dt, getCos(c(v$xv[i], v$yv[i]),c(v$xv[i+dt], v$yv[i+dt])), sqrt(v$xv[i]^2+v$yv[i]^2)*sqrt(v$xv[i+dt]^2+v$yv[i+dt]^2))
    } #add calculated cosine value to 'subpool'
  }
  pool <- rbind(pool, subpool) #add to main pool (way faster than directly appending)
}

# Average everything in pool.
results <- pool %>% 
group_by(deltaT) %>%
dplyr::summarize(cos = weighted.mean(cos,pLen))

# Check how much vector calculations are involved in each deltaT.
results1 <- pool %>%
count(deltaT)

# Check how much tracks are involved in each deltaT
results2 <- pool %>%
group_by(deltaT) %>%
dplyr::summarize(tracks = n_distinct(track))

# Check how much slices each track has
j <- raw %>%
group_by(track) %>%
dplyr::summarize(max = max(slice))

# Download results as CSV.
write.table(results, "", sep=",", row.names = FALSE)
write.csv(results1, "", row.names = FALSE)
write.csv(results2, "", row.names = FALSE)
write.csv(pool, "", row.names = FALSE)
write.csv(v, "")