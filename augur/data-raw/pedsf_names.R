

pedsf_names <- 
  read.csv("./data-raw/tabula-pedsf-attachment-a-format.csv", 
           stringsAsFactors = FALSE, header = TRUE)

names(pedsf_names) <- c("NAACCR", "RM", "RM1", "vars", "RM_YRS", "RM2", "Position", "widths")

#for all meaningful variables
#pedsf_names <- pedsf_names[,-c(2, 3, 5, 6)]

pedsf_names <- pedsf_names[,-c(2:3, 5:8)]

pedsf_names$vars <- tolower(pedsf_names$vars)

match_names <- function(x, md = 1){
  agrep(x, pedsf_names$NAACR, max.distance = md, ignore.case = TRUE)
}

#MUST RUN THIS SCRIPT AFTER OTHERS BECAUSE OF THIS NEXT LINE


