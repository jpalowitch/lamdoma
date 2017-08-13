source("hz2note.R")
C4 <- 261.6256
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
colors <- gg_color_hue(12)
names(colors) <- notenames
library(reshape2); library(ggplot2)
lamdoma <- function (k = 4, boxnotes = NULL, fn = NULL, version = 2) {
  
  basemat <- tcrossprod(1 / 1:k, 1:k)
  fracmat <- outer(1:k, 1:k, function (j, i) paste0(i, "/", j))
  hzmat <- basemat * C4
  notemat <- apply(hzmat, c(1, 2), function (ij) hz2note(ij)[1])
  octavemat <- apply(hzmat, c(1, 2), function (ij) as.numeric(hz2note(ij)[2])) - 4
  aposmat <- apply(octavemat, c(1, 2), function (ij) {
    ifelse(sign(ij) == 1, paste0(rep("'", ij), collapse = ''),
           paste0(rep(",", abs(ij)), collapse = ''))
  })
  notemat2 <- paste0(notemat, aposmat)
  subnums <- round(abs(log2(basemat)) %% 1, 3) * 1000
  subnums[row(subnums) > col(subnums)] <- 1000 - subnums[row(subnums) > col(subnums)]
  subnums[subnums == 1000] <- 0
  labmat <- matrix(paste0(notemat2, "\n", fracmat, "\n", subnums), ncol = k)
  labmat <- t(labmat[ , k:1])[k:1 , k:1]
  notemat <- t(notemat[ , k:1])[k:1 , k:1]
  labmatm <- melt(labmat)
  labmatm$note <- melt(notemat)$value
  labmatm$notecolor <- colors[labmatm$note]
  labmatm$box <- labmatm$note %in% boxnotes
  labmatm$linecols <- colors[labmatm$note]
  labmatm$linecols[labmatm$box] <- "#FFFFFF"
  #labmatm$linecols[!labmatm$box] <- "#000000"
  if (version == 1) {
    labmatm$notecolor[labmatm$box] <- "#FFFFFF"
  } 
  if (version == 2) {
    labmatm$notecolor[!labmatm$box] <- "#000000"
  }
  if (version == 3) {
    labmatm$notecolor[labmatm$box] <- "#FFFFFF"
    labmatm$notecolor[!labmatm$box] <- "#000000"
  }
    
  p <- ggplot(data = labmatm, aes(x = Var1, y = Var2, label = value, fill = notecolor)) + 
    geom_tile(aes(width = 0.94, height = 0.94), size = 4) + 
    geom_text(colour = "black", size = 10) + 
    scale_color_identity() + scale_fill_identity() + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) + 
    guides(fill = FALSE, colour = FALSE) + 
    theme(panel.background = element_rect(fill = 'black', colour = 'black'))
  if (is.null(fn)) {
    fn <- paste0("lamdoma", k, "_", paste0(boxnotes, collapse = "_"), "_version", version, ".png")
  } else {
    fn <- paste0(fn, ".png")
  }
  ggsave(fn, p, width = 2 * k, height = 2 * k, limitsize = FALSE)
}
