library(grid)
library(gridExtra)

#source("read_ranges.R")
source("plot_mode.R")
source("hz2note.R")

# Getting plot and filenames
fns <- list.files("voicings")
plotnames <- unname(sapply(fns, function (fn) substr(fn, 1, nchar(fn) - 4)))

# Doing the plotting
plotdir <- "plots"
if (!dir.exists(plotdir)) dir.create(plotdir)
for (j in seq_along(plotnames)) {
#for (j in 1:1) {
  
  cat("doing", plotnames[j], "\n")

  # Getting mode data
  modes <- readLines(file.path("voicings", fns[j]))
  modes <- lapply(modes, function (char) {
    split1 <- strsplit(char, " ")[[1]]
    split2 <- unname(unlist(sapply(split1, function (char2) strsplit(char2, "\t"))))
  })
  modes <- modes[unlist(lapply(modes, length)) > 0]
  mdf <- as.data.frame(matrix(0, ncol = length(modes[[1]]), nrow = length(modes) - 1))
  names(mdf) <- modes[[1]]
  for (i in 2:length(modes)) mdf[i - 1, ] <- as.numeric(modes[[i]])
  modes <- mdf
  
  ddf_full <- data.frame(Dummy = rep(0, 2))
    
  # Plotting mode data
  datalist <- lapply(1:length(modes), function(L) {
    name <- names(modes)[L]
    voicing <- rev(modes[[name]])
    guide <- L == length(modes)
    retobj <- plot_mode(voicing, name = name, guide = guide)
    return(retobj)
  })
  
  plotlist <- lapply(datalist, function (L) L[[1]])
  ggsave(file.path(plotdir, paste0(plotnames[j], ".png")), 
         grid.arrange(grobs = plotlist, ncol = ceiling(length(modes) / 2),
                      top = textGrob(plotnames[j], gp = gpar(fontsize = 20))),
         height = 5 * 2, width = 5 * ceiling(length(modes) / 2))
  
  plotlist2 <- lapply(datalist, function (L) L[[3]])
  ggsave(file.path(plotdir, paste0(plotnames[j], "__org.png")), 
         grid.arrange(grobs = plotlist2, ncol = length(modes),
                      top = textGrob(plotnames[j], gp = gpar(fontsize = 20))),
         height = 12, width = 12)
  
  # Saving disturbance values
  ddf_full <- data.frame(matrix(0, nrow = 2, ncol = length(modes)))
  names(ddf_full) <- names(modes)
  row.names(ddf_full) <- c("adv", "rdv")
  for (k in seq_along(datalist)){ ddf_full[ , k] <- datalist[[k]][[2]][ , 1] }
  max.print <- getOption('max.print')
  options(max.print=nrow(ddf_full) * ncol(ddf_full))
  sink(file.path(plotdir, paste0(plotnames[j], "_dist.txt")))
  print(ddf_full)
  sink()
  
  rm(datalist, plotlist, ddf_full)
  
  # Plotting mode data (undertone)
  datalist <- lapply(1:length(modes), function(L) {
    name <- names(modes)[L]
    voicing <- modes[[name]]
    guide <- L == length(modes)
    return(plot_mode(voicing, name = name, guide = guide, undertone = TRUE))
  })
  
  plotlist <- lapply(datalist, function (L) L[[1]])
  ggsave(file.path(plotdir, paste0(plotnames[j], "__undertone.png")), 
         grid.arrange(grobs = plotlist, ncol = ceiling(length(modes) / 2),
                      top = textGrob(paste0(plotnames[j], " (undertones)"), gp = gpar(fontsize = 20))),
         height = 5 * 2, width = 5 * ceiling(length(modes) / 2))
  
  plotlist2 <- lapply(datalist, function (L) L[[3]])
  ggsave(file.path(plotdir, paste0(plotnames[j], "__undertone__org.png")), 
         grid.arrange(grobs = plotlist2, ncol = length(modes),
                      top = textGrob(plotnames[j], gp = gpar(fontsize = 20))),
         height = 12, width = 12)
}
