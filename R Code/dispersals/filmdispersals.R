# Takes a sequence of bacteria projected positions over time and creates an unmerged table of s2 distances by iterating through gaps of time (deltaT) and each pair of positions in tracks.
# Able to summarize the table with an average s2 for each deltaT.

library(tidyverse)
# Import tracks as CSV. Format: track,slice,s. s column needs to be a projected distance along hypha.
raw <- read.csv("")

# Micron/pixel ratio. Adjust if needed.
scale = 0.2397

# Create empty 'pool' df containing all deltaT and s2 (called d2).
pool <- data.frame(track = numeric(0),
                   deltaT = numeric(0),
                   d2 = numeric(0))

# Main loop (nested nested for loop).
for (numTrack in 1:length(unique(raw$track))){ #iterate track
  subpool <- data.frame(track = numeric(0),
                   deltaT = numeric(0),
                   d2 = numeric(0))
  t <- subset(raw, track == numTrack)
  print(paste("track ",numTrack," with ",nrow(t)," points")) #keeps track of progress 
  if (nrow(t) <= 1) next #skip track if it has less than indicated slices
  for (dt in 0:(nrow(t)-1)){ #iterate deltaT 
    for (i in 1:(nrow(t)-dt)){ #iterate through each pair of points
      subpool[nrow(subpool) + 1,] <- c(numTrack, dt, (((t$s[i+dt]-t$s[i]))^2))
    } #add calculated d2 to 'subpool'
  }
  pool <- rbind(pool, subpool) #add to main pool (way faster than directly appending)
}

# Average everything in pool.
results <- pool %>% 
group_by(deltaT) %>%
dplyr::summarize(d2 = mean(d2))

# Download CSV of results.
write.table(results, "", sep=",", row.names = FALSE)

# Download CSV of unaveraged results.
write.csv(pool, "", row.names = FALSE)