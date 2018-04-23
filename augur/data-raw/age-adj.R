
example_age_adjustment <- function(){
  the_site <- 
    xml2::read_html("https://seer.cancer.gov/seerstat/tutorials/aarates/step3.html")
  the_table <- rvest::html_text(rvest::html_node(the_site, "table"))
  split_text <- unlist(strsplit(the_table, "\\s+"))
  last_row <- split_text[168:175]
  the_data <- data.frame(t(matrix(split_text[16:167], ncol = 19)), 
                         stringsAsFactors = FALSE)

  table_names <- split_text[1:15]
  under_paste <- function(x) paste0(x, collapse = "_")
  column_names <- 
    c(table_names[1], "_", table_names[2:3], 
      under_paste(table_names[4:5]), 
      under_paste(table_names[6:9]), 
      under_paste(table_names[10:14]), "Component")
  names(the_data) <- column_names

  remove_commas <- function(x) gsub("\\,", "", x)
  commas <- 3:length(the_data)
  the_data[,commas] <- lapply(the_data[,commas], remove_commas)
  the_data <- the_data[,-2]
  the_data
}

seer_adj_example <- example_age_adjustment()

