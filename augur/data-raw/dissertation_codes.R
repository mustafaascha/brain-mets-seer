
dissertation_codes <- read.csv("./data-raw/application_codes.csv", 
                               stringsAsFactors = FALSE)

#Prep for text cleanup
f_fill <- function(vec){
  to_return <- list()
  for(i in seq_along(vec)){
    if(vec[i] == ""){
      to_return[[i]] <- to_return[[i - 1]]
    } else{
      to_return[[i]] <- vec[i]
    }
  }
  to_return
}

trim_string <- function(st) gsub("^\\s+|\\s+$|\\.", "", st)

grep_raw <- function(x, p) {
  to_return <- regmatches(x, regexpr(p, x))
  ifelse(is.logical(to_return), "", to_return)
}

#clean up text
dissertation_codes$Treatment.Type <- f_fill(dissertation_codes$Treatment.Type)
dissertation_codes[] <- lapply(dissertation_codes, trim_string)


dissertation_codes$Code <- gsub("and\\ ", ",\\ ", dissertation_codes$Code)
dissertation_codes$Code <- strsplit(x = dissertation_codes$Code, split = ",")
dissertation_codes <- tidyr::unnest(dissertation_codes)

vars_to_expand <- grep("-|–", dissertation_codes$Code)
to_expand <- dissertation_codes[vars_to_expand,]
code_backup <- to_expand$Code
to_expand$Code <- strsplit(to_expand$Code, "-|–")
to_expand$First <- vapply(to_expand$Code, 
                          function(x) as.numeric(grep_raw(x[1], "[0-9]+")), 
                          numeric(1))
to_expand$Second <- vapply(to_expand$Code, 
                           function(x) as.numeric(grep_raw(x[2], "[0-9]+")),
                           numeric(1))
to_expand$Code_Letters <- vapply(to_expand$Code, 
                                 function(y) grep_raw(x = y[1], "[A-Z]0"),
                                 character(1))
to_expand$Code <- NULL

to_expand$Code <- with(to_expand, mapply(First, Second, FUN = `:`))
expanded <- tidyr::unnest(to_expand)
expanded$Code <- with(expanded, 
                       ifelse(is.na(Code_Letters), Code, 
                              paste(Code_Letters, Code, sep = "")))
expanded <- expanded[,-c(3:5)]
rm(to_expand)
dissertation_codes <- dissertation_codes[-vars_to_expand,]
dissertation_codes <- rbind(dissertation_codes, expanded)

dissertation_codes[["Code"]][139:143] <- 
  paste("0", dissertation_codes[["Code"]][139:143], sep = "")

dissertation_codes[] <- lapply(dissertation_codes, trim_string)

dissertation_codes <- dissertation_codes[dissertation_codes[["Code"]] != "",]

dissertation_codes <- 
  rbind(dissertation_codes, 
        data.frame(Treatment.Type = "Diagnostic ICD", 
                   Code.Classification = "ICD9", 
                   Code = c(as.character(19830:19839 / 100), "198.30"),
                   stringsAsFactors = FALSE))





