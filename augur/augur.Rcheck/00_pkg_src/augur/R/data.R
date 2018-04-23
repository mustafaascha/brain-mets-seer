#' Dataframes to support recoding SEER and Medicare diagnosis and procedure data
#'
#' Each dataframe contains the key-value pairs necessary to recode a variable 
#'  so that it is interpretable, rather than its coded numeric value. These 
#'  data come from several sources, the URL for each of which is listed here. 
#'  For more information on each dataframe listed here, see its respective 
#'  documentation. 
#'
#'@format Four data frames of key-value pairs that can be used with 
#'        `recode_variable` and appropriate claims files to translate 
#'         codes into human-readable corresponding drug or procedure names.
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
"radio_chemo_codes"

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
#' 


#' 
#' 
#' 
#' 
#' CCflag_infile.txt  dme_infile.txt    nch_infile.txt 
#' medpar_infile.txt  ptden_infile.txt  pdesaf_infile.txt  
#' pedsf_infile.txt   outpat_infile.txt   
#' 
#' 
#' 
#' 
#' 
#' 
#' 







