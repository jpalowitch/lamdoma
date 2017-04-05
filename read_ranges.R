ranges <- read.table("ranges.txt", sep = "\t", stringsAsFactors = FALSE,
                     comment.char = "")
names(ranges) <- c("Quarter_Step_Down", "Frequency", "Note")

ranges$TrueNote <- sapply(ranges$Note, 
                      function (note) {
                        n <- nchar(note)
                        ifelse(substr(note, n, n) %in% as.character(0:9), 
                               substr(note, 1, n - 1), 
                               note)
                      }
)
ranges$Quarter_Step_Down[1] <- 0
ranges <- rbind(ranges, data.frame("Quarter_Step_Down" = Inf,
                                   "Frequency" = Inf,
                                   "Note" = 'NA',
                                   "TrueNote" = 'NA'))
