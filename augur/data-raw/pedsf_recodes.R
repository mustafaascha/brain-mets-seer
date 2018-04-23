#This file ends with a key-value pairing for each variable whose factor levels were provided in PEDSF PDF documentation

pedsf_recodes <- read.csv("./data-raw/tabula-PEDSF-recode.csv", 
                          stringsAsFactors = FALSE, 
                          header = TRUE)

names(pedsf_recodes)[1] <- "Var"
pedsf_recodes$Var <- gsub("\\(|\\)|-.*", "", pedsf_recodes$Var)

pedsf_recodes$Split_Notes <- strsplit(pedsf_recodes$NOTES, "\\ =\\ ")
pedsf_recodes$NOTES <- NULL

pedsf_recodes$Code <- 
  vapply(pedsf_recodes$Split_Notes, function(x) x[1], character(1))
pedsf_recodes$Meaning <- 
  vapply(pedsf_recodes$Split_Notes, function(x) x[2], character(1))
pedsf_recodes$Split_Notes <- NULL

add_rows <- function(x) rbind(pedsf_recodes, x)

add_recodes <- function(add_to, to_add, added_meaning) {
  add_rows(data.frame(
              Var = rep_len(add_to, length(to_add)),
              Code = to_add,
              Meaning = added_meaning, 
              stringsAsFactors = FALSE))
}



# codes for races 7 through 9 are missing!!
#add_recodes("race", 7:9, c(


attachment_recodes <- 
  read.csv("./data-raw/tabula-pedsf_attachment_a.csv", 
           stringsAsFactors = FALSE, 
           header = TRUE)[,1:3]

prc <- pedsf_recodes <- rbind(pedsf_recodes, attachment_recodes)

n_range <- 
  which(prc$Meaning == "NX"):(which(prc$Meaning == "N3c")[1] + 1)
pedsf_recodes[["Var"]][n_range] <- "dajccn"

t_range <- 
  which(prc$Meaning == "TX"):(which(prc$Meaning == "T1b")[1] + 1)
pedsf_recodes[["Var"]][t_range] <- "dajcct"






