source("fracs2tones.R")
trumpSpeech <- readLines("Trump_Inaug_Speech.txt")
trumpSpeech <- unlist(lapply(trumpSpeech, strsplit, split = " "))
trumpSpeech <- trumpSpeech[!grepl("--", trumpSpeech, fixed = TRUE)]
positions <- which(grepl("America", trumpSpeech))
N <- length(trumpSpeech)

fracs2tones(positions, N, name = "trumpSpeech")
