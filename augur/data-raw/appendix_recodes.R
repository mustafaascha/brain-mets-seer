


appendix <- read.csv("./data-raw/tabula-Appendix-handmade.csv",
                     stringsAsFactors = FALSE,            
                     header = FALSE, col.names = c("Var", "Codes", "rm", "rm1"))
appendix$rm <- NULL
appendix$rm1 <- NULL

appendix[appendix == ""] <- NA 
appendix$Var <- gsub("\\(|\\)", "", zoo::na.locf(appendix$Var))
appendix$Var <- tolower(appendix$Var)

appendix$Split <- strsplit(appendix$Codes, "\\ =\\ ")
appendix$Codes <- NULL
appendix$Code <- vapply(appendix$Split, function(x) x[1], character(1))        
appendix$Meaning <- vapply(appendix$Split, function(x) x[2], character(1))     
appendix$Split <- NULL

appendix <- appendix[complete.cases(appendix),]

appendix_recodes <- appendix


rm(appendix)
