source("hz2note.R")

fracs2tones <- function (positions, N, startpos = 300, endpos = 600, 
                         timelength = 360, denom = 16, tempo = 60, name = "untitled") {
  
  tempo <- as.integer(tempo)
  timelength <- as.integer(timelength)
  
  fracs <- (positions - 1) / (N - 1)
  fracs <- sort(fracs)
  fracpos <- startpos + fracs * (endpos - startpos)
  fracnotes <- hz2note(fracpos, startpos, startstep = 0)[1:length(fracs)]
  
  if (denom %% 4 != 0)
    stop("denom must be divisible by 4.")
  
  # Reporting beat diagnosis
  if ((timelength * tempo) %% 60 != 0) {
    message("Warning: time span ends between two beats.")
  }
  beats <- floor(timelength * tempo / 60)
  message(paste(timelength, "seconds requested."))
  message(paste(beats, "beats available at", tempo, "bpm."))
  message(paste("Beats will end at", round(beats * 60 / tempo, 2), "seconds."))
  tics <- beats * (denom / 4)
  
  # Computing grid and placement
  fracplace <- round(1 + fracs * tics)
  fracmeas <- floor(fracplace / (denom)) + 1
  fracmeas_pos <- fracplace %% denom
  
  retdf <- data.frame(Positions = sort(positions),
                      Decimal = fracs,
                      StringPosition = fracpos,
                      Notes = fracnotes,
                      Measure = fracmeas,
                      Placement = fracmeas_pos)
  
  fn <- paste0(name, "_start", startpos, "_end", endpos,
               "_length", timelength, "_denom", denom, "_tempo", tempo, ".txt")
  
  max.print <- getOption('max.print')
  options(max.print = nrow(retdf) * ncol(retdf))
  sink(fn)
  print(retdf)
  sink()
  
}