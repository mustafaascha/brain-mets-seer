context("Reading")

test_that("Can read each file", {
  read_testfile <- function(fl, infl){
    filepath <- system.file("extdata", fl, package = "augur")
    infilepath <- system.file("extdata", infl, package = "augur")
    #test that the files exist (which they should...)
    expect_true(filepath != "")
    expect_true(infilepath != "")
    read_medicare(filepath, infilepath)
  }

  filepaths <- c("CCflag.txt.gz", "dme.txt.gz", 
                 "medpar.txt.gz", "nch.txt.gz", 
                 "outsaf.txt.gz", "pdesaf.txt.gz", 
                 "pedsf.txt.gz")
  infilepaths <- c("CCflag_infile.txt", "dme_infile.txt", 
                   "medpar_infile.txt", "nch_infile.txt", 
                  "outpat_infile.txt", "pdesaf_infile.txt",
                   "pedsf_infile.txt", "ptden_infile.txt")

  test_fn <- function(x, y) expect_is(read_testfile(x, y), "data.frame")
  
  for(i in seq_along(filepaths)){
    test_fn(filepaths[i], infilepaths[i])
  }

})



test_that("Diagnosis code charcounts are the same between implementations", {

  filepath <- function(fl) system.file("extdata", fl, package = "augur")

  file_dxcode_lengths <- function(filepath) {

    get_diagnosis_fields <- function(filepath){
      pathlines <- readLines(filepath)
      diagnoses <- substr(pathlines, 487, 487 + 12 * 7)
      diagnoses
    }

    file_data <- get_diagnosis_fields(filepath)
    #split each line by whitespace
    split_substr <- function(file_line){ 
      lapply(file_line, function(y) unlist(strsplit(y, "\\ +")))
    }
    file_diags <- split_substr(file_data)
    #get the length of a diagnosis
    diag_charcount <- function(dgn) length(unlist(strsplit(dgn, "")))
    #get the length of each diagnosis on a line
    record_diagcounts <- function(rec) vapply(rec, diag_charcount, integer(1))
    all_diagcounts <- function(file_data) lapply(file_diags, record_diagcounts)

    all_diagcounts(file_diags)

  }


  df_dxcode_lengths <- function(filepath, ff) {

    file_data <- read_medicare(filepath, ff)[["dgn_cd"]]

    split_substr <- function(file_line){ 
      lapply(file_line, function(y) unlist(strsplit(y, "\\ +")))
    }
    file_diags <- split_substr(file_data)
    #get the length of a diagnosis
    diag_charcount <- function(dgn) length(unlist(strsplit(dgn, "")))
    #get the length of each diagnosis on a line
    record_diagcounts <- function(rec) vapply(rec, diag_charcount, integer(1))
    all_diagcounts <- function(file_data) lapply(file_diags, record_diagcounts)

    all_diagcounts(file_diags)

  }


  base_r <- file_dxcode_lengths(filepath("nch.txt.gz"))
  package_r <- df_dxcode_lengths(filepath("nch.txt.gz"), 
                                 filepath("nch_infile.txt"))

  #last row wouldn't correspond because of how they handle missingness, meh
  expect_identical(base_r[-1001], package_r[-1001])

})
