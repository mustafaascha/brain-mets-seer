#' Calculate age-adjusted incidence rates for Martin et al.
#'
#' @param pand Whether to pander the object versus return it
#'
#' @return data.frame
#' @export
#'
martin <- function(pand = FALSE) {
  fcy <- frequencies::census_yrly
  pop_ages <- function(the_ages) sum(as.numeric(fcy$count[fcy$age_yr %in% the_ages]))
  martin2017 <- 
    data.frame(the_count = c(57, 423, 425, 63), 
               the_pop = c(13564, 98184, 103700, 23265),
               the_age = c("18-40", "41-60", "61-80", ">80"), 
               census_std = map_dbl(list(18:40, 41:60, 61:80, 80:115), pop_ages))
  
  the_aair <- with(martin2017, ageadjust(the_count, the_pop, census_std) * 1e4)
  
  if(pand){
    pander(the_aair, 
           caption = "Martin 2017 crude and age-adjusted incidence rate of synchronous brain metastases")
  } else {
    return(the_aair)
  }
}

#' Make a dataframe to use for Barnholtz-Sloan incidence calculations
#'
#' @param the_count The number of subjects with brain metastases, using five age categories
#' @param the_pop   The number of subjects overall, using five age categories
#'
#' @return data.frame
#' @export
#'
jbs_df <- function(the_count, the_pop){
  
  fcy <- frequencies::census_yrly
  pop_ages <- function(the_ages) sum(as.numeric(fcy$count[fcy$age_yr %in% the_ages]))
  
  data.frame(
    the_age = c("20-39", "40-49", "50-59", "60-69", "70+"),
    the_count = the_count,
    the_pop = the_pop, 
    census_std = map_dbl(list(20:39, 40:49, 50:59, 60:69, 70:110), pop_ages)
  )
}

#' Calculate age-adjusted incidence for Barnholtz-Sloan
#'
#' @param df The df return by `jbs_df`
#' @param pop_fctr The denominator (per 1,000, 100, etc)
#'
#' @return
#' @export
#'
aa_mdcss <- function(df, pop_fctr = 1e4){
  tr <- 
    with(df, 
         ageadjust(the_count, the_pop, census_std)
    ) * pop_fctr
  spread(data.frame(var = names(tr), val = unname(tr)), 
         var, val) 
}
