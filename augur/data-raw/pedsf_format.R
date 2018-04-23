
#to_fix <- read.csv("./data-raw/tabula-PEDSF.csv", stringsAsFactors = FALSE)
#pedsf_format <- to_fix[to_fix$COL != "",]
#names(pedsf_format) <- 
#  c("positions", "vars", "widths")
#pedsf_format$col_types <- "c"
#pedsf_format$vars <- 
#  gsub("\\.", "_", make.names(pedsf_format$vars))
#pedsf_format <- pedsf_format[pedsf_format$positions != "COL",]
##just use the formatfile I pulled
#pedsf_format$widths[pedsf_format$widths == ""] <- 
#c("2", "1", "8", "53", "12", "24", "2", "2", "12")
#pedsf_format$widths[grep("\\*", pedsf_format$widths)] <- "12"
#pedsf_format <- pedsf_format[pedsf_format$positions != "COL",]
  
pedsf_format <- 
  read.csv("data-raw/tabula-PEDSF-modified.csv", stringsAsFactors = FALSE)
names(pedsf_format) <- c("positions", "vars", "widths")
pedsf_format$vars <- gsub("\\(|\\)", "", pedsf_format$vars)
pedsf_format <- pedsf_format[pedsf_format$vars != "Filler",]
to_fix <- which(pedsf_format$vars == "eod4")
pedsf_format$vars[to_fix] <- 
  paste("eod4", seq_along(to_fix), sep = "_")


