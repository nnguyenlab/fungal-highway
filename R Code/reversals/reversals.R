# Takes a CSV of bacteria tracks and reports changes in the projected movement direction along a stretch of hypha. Includes considerations for stationary bacteria.

# Segment by velocity direction.
segment <- function(rT, track){
  #initialize segments df
  segments <- data.frame(track = numeric(nrow(rT)),
                         start = numeric(nrow(rT)),
                         end = numeric(nrow(rT)))
  segments$track[1] <- track
  segments$start[1] <- 1

  s <- 1
  for (q in 2:(nrow(rT)-1)){
    if (sign(rT$projVelo[q+1]) != sign(rT$projVelo[q])){
      segments$end[s] <- q
      s <- s + 1
      segments$track[s] <- track
      segments$start[s] <- q + 1
    }
  }
  segments$end[s] <- nrow(rT)
  print(track)
  return(subset(segments, track != 0))
}

# Determine segment's projection displacement.
appendProjDisp <- function(sT, rT){
  sT$projDisp <- rT$hyphaProj[sT$end]-rT$hyphaProj[sT$start]
  tryCatch( 
    expr = {                      
      for (i in 2:nrow(sT)){
        sT$projDisp[i] <- rT$hyphaProj[sT$end[i]]-rT$hyphaProj[sT$start[i] - 1]
      }
    },
    error = {
      return(sT)
    })
  print(sT)
  return(sT)
}

# Loop to smoothen out segments based on criteria.
smoothen <- function(sT){
  while (min(abs(sT$projDisp)) < stopDist){
    h <- which.min(abs(sT$projDisp))
    if (h == 1){
      sT$end[h] <- sT$end[h+1]
      sT$projDisp[h] <- sum(sT$projDisp[c(h,h+1)])
      sT <- sT[-(h+1),]
    } else if (h == nrow(sT)){
      sT$start[h] <- sT$start[h-1]
      sT$projDisp[h] <- sum(sT$projDisp[c(h-1,h)])
      sT <- sT[-(h-1),]
    } else if (sign(sT$projDisp[h-1]) != sign(sT$projDisp[h+1])){
      sT$start[h+1] <- sT$start[h]
      sT <- sT[-h,]
    } else{
      sT$start[h] <- sT$start[h-1]
      sT$end[h] <- sT$end[h+1]
      sT$projDisp[h] <- sum(sT$projDisp[c((h-1):(h+1))])
      sT <- sT[-c(h-1,h+1),]
    }
    rownames(sT) <- 1:nrow(sT)
  }
  return(sT)
}

# Write as results.
format <- function(sT, rT){
  return(data.frame(bacteria = sT$track, 
                    slice = sT$end, 
                    x = rT$x[sT$end], 
                    y = rT$y[sT$end], 
                    projDisp = sT$projDisp)[-nrow(sT),])
}

# Import data as CSV. Format: track,slice,x,y.
raw <- read.csv("")

# Px threshold for what is considered stationary. Adjust as needed.
stopDist <- 20

# Hypha end coordinates. Adjust as needed.
hx1 <- 
hy1 <- 
hx2 <- 
hy2 <- 

# Get distance along hypha.
raw$hyphaProj <- ((raw$x-hx1)*(hx2-hx1)+(raw$y-hy1)*(hy2-hy1))/sqrt((hy2-hy1)^2+(hx2-hx1)^2)

# Get velocity based on distance along hypha.
raw$projVelo <- NA
for (p in 2:nrow(raw)){
  raw$projVelo[p] <- raw$hyphaProj[p]-raw$hyphaProj[p-1]
}

# Fix dist.
for (p in 2:nrow(raw)){
  raw$dist[p] <- sqrt((raw$x[p]-raw$x[p-1])^2+(raw$y[p]-raw$y[p-1])^2)
}

# Initialize blank df.
finally <- data.frame()

# Main loop.
for (numTrack in 1:length(unique(raw$track))){
  r <- subset(raw, track == numTrack)
  finally <- rbind(finally, 
    format(
      smoothen(
        appendProjDisp(
          segment(r, numTrack), 
          r
        )
      ),
      r
    )
  )
}

# Save summary.
write.csv(finally, "", row.names = FALSE)

# Save raw.
write.csv(raw, "", row.names = FALSE)

