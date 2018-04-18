library(reshape2)
library(ggplot2)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

plot_mode <- function (voicing, njump = 16, name = "name", guide = TRUE, undertone = FALSE) {
  
  k <- length(voicing)
  voicing <- rev(voicing)
  modemat <- matrix(rep(voicing, njump), ncol = k, byrow = TRUE)
  if (undertone) {
    modemat <- modemat / njump:1
  } else {
    modemat <- modemat * 1:njump
  }
  matcolor <- as.data.frame(apply(modemat, c(1, 2), function (entry) hz2note(entry)[1]))
  matlab <- as.data.frame(apply(modemat, c(1, 2), function (entry) paste0(hz2note(entry), collapse = '')))
  melt_matlab <- melt(as.matrix(matlab))
  melt_matcolor <- melt(as.matrix(matcolor))
  names(melt_matcolor) <- c("Harmonic", "Voicing_Start", "Note")
  melt_matcolor$TrueNote <- melt_matlab$value
  melt_matcolor$Frequency <- format(round(melt(modemat)[ , 3], 2), nsmall = 2)
  melt_matcolor <- as.data.frame(melt_matcolor, stringsAsFactors = FALSE)
  melt_matcolor$TrueNote <- as.character(melt_matcolor$TrueNote)
  melt_matcolor$TrueNote <- sapply(melt_matcolor$TrueNote, function (note) {
    ifelse(nchar(note) == 2, paste0(note, " "), note)})
  melt_matcolor$Label <- paste(melt_matcolor$Frequency, melt_matcolor$TrueNote)
  
  # Counting reappearance of TrueNote
  trueNoteTab <- table(melt_matcolor$TrueNote)
  counts <- trueNoteTab[melt_matcolor$TrueNote]
  melt_matcolor$Note <- as.character(melt_matcolor$Note)
  melt_matcolor$TrueNote[counts <= 1] <- 'NA'
  
  # Calculating absolute disturbance variance
  freqs_of_interest <- (log(as.numeric(melt_matcolor$Frequency)[counts > 1]) - log(440)) / (log(2) / 12)
  notes_of_interest <- melt_matcolor$TrueNote[counts > 1]
  adv <- sum((freqs_of_interest - round(freqs_of_interest))^2) / sum(counts > 1)
  
  # Calculating reduced mat
  redmm <- subset(melt_matcolor, !(melt_matcolor$TrueNote == 'NA'))
  redmm$Voicing_Start <- rep(redmm$Voicing_Start[1], nrow(redmm))
  redmm <- redmm[order(redmm$Frequency), ]
  redmm$Harmonic <- 1:nrow(redmm)
  p2 <- ggplot(data = redmm, aes(x = Voicing_Start, y = Harmonic, fill = TrueNote)) + 
    geom_tile() + guides(fill = FALSE) + 
    scale_fill_manual(values = c(rainbow(length(unique(melt_matcolor$TrueNote)) - 1), "#FFFFFF"),
                      labels = unique(melt_matcolor$TrueNote)) + 
    geom_text(aes(label = Label), size = 3.5) + ggtitle(name) + 
    theme_minimal() + 
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) 
  
  # Calculating relative disturbance variance
  means <- tapply(freqs_of_interest, notes_of_interest, mean)
  meanvec <- means[match(notes_of_interest, names(means))]
  rdv <- sum((freqs_of_interest - meanvec)^2) / sum(counts > 1)

  # Naming the voicing starts
  melt_matcolor$Voicing_Start
  p <- ggplot(data = melt_matcolor, aes(x = Voicing_Start, y = Harmonic, fill = TrueNote)) + 
    geom_tile() + guides(fill = FALSE) + 
    scale_fill_manual(values = c(rainbow(length(unique(melt_matcolor$TrueNote)) - 1), "#FFFFFF"),
                      labels = unique(melt_matcolor$TrueNote)) + 
    geom_text(aes(label = Label), size = 3.5) + ggtitle(name) + 
    scale_x_discrete(labels=paste("Note", seq_along(voicing)))
  if (!guide)
    p <- p + guides(fill = FALSE)
  
  # Creating disturbance df
  ddf <- data.frame(c(adv, rdv))
  names(ddf) <- name
  row.names(ddf) <- c("adv", "rdv")
  
  return(list(p, ddf, p2))
}
  
  
  
  
