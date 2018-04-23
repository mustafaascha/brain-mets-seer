#' Dataframes to support recoding SEER and Medicare diagnosis and procedure data
#'
#' Each dataframe contains the key-value pairs necessary to recode a variable 
#'  so that it is interpretable, rather than its coded numeric value. These 
#'  data come from several sources, the URL for each of which is listed here. 
#'  For more information on each dataframe listed here, see its respective 
#'  documentation. 
#'
#' @format Four data frames of key-value pairs that can be used with 
#'        `recode_variable` and appropriate claims files to translate 
#'         codes into human-readable corresponding drug or procedure names.
#' 
#' 
#'\describe{ 
#'  \item{chemo_drugs}{8703 chemotherapy drugs over 9 recode variables}
#'  \item{chemo_procs}{569 chemo procedures over 8 recode variables}
#'  \item{radio_procs}{266 radiotherapy procedures over 6 variables}
#'  \item{radio_diags}{5 diagnoses described by 5 variables}
#'}
#'@source \url{https://crn.cancer.gov/resources/ctcodes-drugs.csv}
#'@source \url{https://crn.cancer.gov/resources/ctcodes-procedures.csv}
#'@source \url{https://crn.cancer.gov/resources/Radiation Therapy Codes_Procedures.csv}
#'@source \url{https://crn.cancer.gov/resources/Radiation Therapy Codes_Diagnoses.csv}
#'@seealso recode_variable
#'

#' PEDSF reading and recoding dataframes
#' 
#' This is a list of three dataframes that are useful for PEDSF data reading and recoding.
#'   The most important of these three dataframes is `pedsf_format`, whose content 
#'   was pulled from the SEER-Medicare PDF PEDSF documentation. The 
#'   `pedsf_recodes` dataframe is, of course, useful in conjunction with the 
#'   `recode_variable` or `recode_pedsf` functions. Finally, the `pedsf_names`
#'   dataframe is useful for converting between full versus short variable names. 
#' 
#' @format Three dataframes, each containing useful recoding or reading-relevant data. 
#' 
#' 
#'\describe{
#'  \item{pedsf_recodes}{1090 rows by 3 columns, this is PEDSF recode data 
#'                        extracted from an NCI HDRP PDF. 
#'       \itemize{
#'               \item{Var}{- The name of the PEDSF variable to be recoded}
#'               \item{Code}{- The coded representation of variables
#'                          that are included as part of PEDSF.} 
#'               \item{Meaning}{- The value that is encoded by `Code` in the 
#'                          PEDSF variable `Var`.} 
#'               }
#'       }
#'  \item{pedsf_format}{A `data.frame` consisting of 245 rows of PEDSF file 
#'                      format data, produced using a SEER-Medicare 
#'                      PEDSF SAS infile. These are useful for reading PEDSF data. 
#'        \itemize{
#'               \item{positions}{- In the PEDSF ASCII file, this is the 
#'                      starting character position for the variable shown in 
#'                      `vars`.}
#'               \item{vars}{- The name of a variable that will be read using 
#'                      a starting position of `positions` and character width
#'                      of `widths` and variable type `col_types`.}
#'                }
#'       }
#'  \item{pedsf_names}{134 full NAACCR variable names alongside their 
#'                     corresponding SAS variable names, again from the i
#'                     SEER-Medicare PEDSF SAS infile.}
#' }
#' 
#'@source \url{https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/pedsf.txt}
#'@source \url{https://healthcaredelivery.cancer.gov/seermedicare/aboutdata/pedsf_attachment_a.pdf}                                                                           
#' 
#' 
"pedsf_utils"


#' Zeroed-out claims files for testing and demonstration
#' 
#' These were produced by replacing every character of sample claims files 
#'   with the number 0. 
#' 
#' @format Each of these files is of the same format as their respectively-named input SAS statements
#' 
#'
#'\describe{
#'  \item{dme.txt.gz}{- Durable Medical Equipment claims}
#'  \item{outsaf.txt.gz}{- Outpatient claims}
#'  \item{CCflag.txt.gz}{- Chronic Conditions Flags}
#'  \item{nch.txt.gz}{- Carrier Claims}
#'  \item{pedsf.txt.gz}{- Patient Entitlement and Diagnosis }
#'  \item{medpar.txt.gz}{- Part A short-stay, long-stay, and skilled nursing facility bills}
#'  \item{pdesaf.txt.gz}{- Part D prescription coverage claims}
#' }
#' 
#' 
#' 
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/dme.txt}
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/medicare/DME.pdf}
#' 
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/outpat.txt}
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/medicare/OUTPAT.pdf}
#' 
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/CCflag.txt}
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/medicare/chronic-conditions-flags.pdf}
#' 
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/nch.txt}
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/medicare/NCH.pdf}
#' 
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/pedsf.txt}
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/aboutdata/PEDSF.pdf}
#' 
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/medpar.txt}
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/medicare/MEDPAR.pdf}
#' 
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/pdesaf.txt}
#' @source \url{https://healthcaredelivery.cancer.gov/seermedicare/medicare/partd.pdf}
#' 

#' 2010 65+ population census data
#' 
#' These data are meant for age-adjustment in the Medicare population. See the 
#' script 'augur/data-raw/
#' 
#' 
#' @format This is a seven-column `tbl`
#' 
#' 
#'\describe{
#'  \item{pop}{This column specifies which age group is represented by that row}
#'  \item{count_2000}{This is a count for each age group, from the year 2000}
#'  \item{prop_2000_65}{This is the proportion of the population 65 or older that is represented in the count for that row, in the year 2000}
#'  \item{prop_2000_all}{This is the proportion of the population at large that is represented in the count for that row, in the year 2000}
#'  \item{count_2010}{This is a count for each age group, from the year 2010}
#'  \item{prop_2010_65}{This is the proportion of the population 65 or older that is represented in the count for that row, in the year 2010}
#'  \item{prop_2010_all}{This is the proportion of the population at large that is represented in the count for that row, in the year 2010}
#' }
#' 
'census2010'


#' Fixed-Width File (FWF) Column Specifications
#' 
#' For each of the claims and PEDSF files, a SAS input statement is provided to describe which character positions reflect which data. The input statements are available online and linked below, and but they're also included in this package. Refer to "augur/data-raw/infiles.R". 
#'
#' @format This is a list of 8 data.frames, each with 4 variables "position", "vars", "widths", "col_types", representing the 2016 SEER-Medicare FWF specifications. The position is the starting character position for that variable with "width" number of characters of type "col_types"
#' 
#'
#'\describe{
#'  \item{CCflag}{\link[=https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/CCflag.txt]{Chronic Conditions Flags}}
#'  \item{dme}{\link[=https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/dme.txt]{Durable Medical Equipment}}
#'  \item{medpar{\link[=https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/medpar.txt]{Medicare Provider Analysis and Review}}
#'  \item{nch}{\link[=https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/nch.txt]{Carrier Claims}}
#'  \item{outpat}{{\link[=https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/outpat.txt]{Outpatient claims}}
#'  \item{pdesaf}{{\link[=https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/pdesaf.txt]{Part D claims (I want to change this so it is more distinct from `pedsf`, but I don't want to deviate from established specs.)}}
#'  \item{pedsf}{{\link[=https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/pedsf.txt]{Patient Entitlement and Diagnosis Summary}}
#'  \item{ptden}{{\link[=https://healthcaredelivery.cancer.gov/seermedicare/program/inputs/ptden.txt]{Part D enrollment data}}
#'}
#' 
"infiles"

#' SEER Direct Age-Adjusted Rates Example Table
#' 
#' From \url{https://seer.cancer.gov/seerstat/tutorials/aarates/step3.html}, 
#' this table has weights for the US 2000 Standard Population. See the script 
#' 'augur/data-raw/age-adj.R'. 
#' 
#' @format This is a data.frame with 19 observations of 7 variables.
#' 
#' 
#' 
#' \describe{
#'   \item{Age}{}
#'   \item{Count}{}
#'   \item{Population}{}
#'   \item{Crude_Rate}{}
#'   \item{U.S._2000_Standard_Populations}{}
#'   \item{Age_Distribution_of_Std_Pop}{}
#'   \item{Component}{}
#' }
#'
"seer_adj_example"

 
#' Appendix for SEER-Medicare 11/2016 Claims Files (May 31, 2017)
#' 
#' From: https://healthcaredelivery.cancer.gov/seermedicare/medicare/appendix.versionk.pdf
#' 
#' @format A data.frame of 2,605 observations over 3 variables
#' 
#' 
#' \describe{
#'   \item{Var}{This is the short name representation of a SEER variable}
#'   \item{Code}{This corresponds to the representation of an observation of `Var` as seen in ASCII FWF data files}
#'   \item{Meaning}{This is the interpreted/decoded value of `Code`}
#' }
#' 
"appendix_recodes"


#' Codes used for my dissertation
#' 
#' @format This is a data.frame
#' 
#' 
#' \describe{
#'   \item{Treatment.Type}{This is a brief description of the code}
#'   \item{Code.Classification}{This is a description of which classification system is used, between ICD-9-CM, ICD-O-3, HCPCS, and maybe others.}
#'   \item{Code}{This is the `Code.Classification`-encoded representation of `Treatment.Type`.}
#' }
#' 
"dissertation_codes"

#' Names of all variables in all filetypes
#'
#' @format This is a data.frame of 1,009 observations over 2 variables
#' 
#'
#' \describe{
#'   \item{Var}{This is the name of the variable in a SEER-Medicare file}
#'   \item{which_file}{This is the file where one might find `Var`}
#' }
#'
#'
"all_filetypes_varnames"




