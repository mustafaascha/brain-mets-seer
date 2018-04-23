#context("recodes")

#test_that("filetype-specific recodes are the same using the general and specific recode functions", {
#  expect_identical(recode_pedsf(breast, Race_Recode_A), 
#                   recode_variable(breast, Race_Recode_A, which_key = pedsf_recodes, for_real = FALSE))
#})
