notenames <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")

hz2note <- function (hz, refhz = 440, startstep = 57) { 
  offset <- round((log(hz) - log(refhz)) / (log(2) / 12))
  pos <- startstep + offset
  note <- notenames[pos %% 12 + 1]
  octave <- as.character(floor(pos / 12))
  return(c(note, octave))
}
  