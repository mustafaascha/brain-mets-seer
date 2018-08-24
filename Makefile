all: dirs claims cancers analysis manuscript

.PHONY : dirs claims cancers analysis manuscript clean help

R_OPTS=--vanilla

VPATH = extraction:munge:analysis:cache:cache/dx-imaging:cache/diagnoses
DXDIR = cache/diagnoses/
IMGDIR = cache/dx-imaging/

## dirs        : Create cache directories
dirs : \
      cache\
      cache/dx-imaging\
      cache/diagnoses

## claims      : Extract relevant records from claims
claims :\
     cpt-img-dme.csv.gz\
     cpt-img-nch.csv.gz\
     cpt-img-out.csv.gz\
     icd-dx-nch.csv.gz\
     icd-dx-nch-p.csv.gz\
     icd-dx-dme.csv.gz\
     icd-dx-dme-p.csv.gz\
     icd-dx-mpr.csv.gz\
     icd-dx-out.csv.gz

## cancers     : Assemble an analytic dataset
cancers :\
     cancers_prerecode.csv.gz\
     cancers_postrecode.csv.gz\
     cancers.csv.gz\
     claims

## analysis    : Run analysis
analysis : paper_products.rds cancers

## manuscript  : Prepare/present results
manuscript : tables-and-figures.html analysis

cache :
	if [ ! -e "cache" ]; then mkdir cache ; fi
cache/diagnoses :
	cd cache;\
	if [ ! -e "diagnoses" ]; then mkdir diagnoses ; fi;\
	cd ..
cache/dx-imaging :
	cd cache;\
	if [ ! -e "dx-imaging" ]; then mkdir dx-imaging; fi;\
	cd ..

#extract data
#$(patsub %.R, %.csv.gz, $<): $*.R
#	Rscript $*.R
cpt-img-dme.csv.gz : cpt-img-dme.R
	Rscript $<
cpt-img-nch.csv.gz : cpt-img-nch.R
	Rscript $<
cpt-img-out.csv.gz : cpt-img-out.R
	Rscript $<

icd-dx-nch.csv.gz : icd-dx-nch.R 
	Rscript $<
icd-dx-nch-p.csv.gz : icd-dx-nch-p.R
	Rscript $<
icd-dx-dme.csv.gz : icd-dx-dme.R
	Rscript $<
icd-dx-dme-p.csv.gz : icd-dx-dme-p.R
	Rscript $<
icd-dx-mpr.csv.gz : icd-dx-mpr.R
	Rscript $<
icd-dx-out.csv.gz : icd-dx-out.R
	Rscript $<

cancers_prerecode.csv.gz : load-claims.R claims
	Rscript $<
cancers_postrecode.csv.gz : recoding.R load-claims.R
	Rscript $<
cancers.csv.gz : exclusion.R recoding.R load-claims.R
	Rscript $<

paper_products.rds : premanuscript.R exclusion.R load.R 
	Rscript analysis/$<

tables-and-figures.html : tables-and-figures.Rmd dirs claims cancers analysis
	Rscript -e 'rmarkdown::render("$<", output_format = "html_document")'

clean: rm -f *.aux *.bbl *.blg *.log *.bak *~ *.Rout */*~ */*.Rout */*.aux */*.log

help : Makefile
	@sed -n 's/^##//p' $<




