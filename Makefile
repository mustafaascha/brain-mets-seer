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
		 img\
		 dx

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

# cache       : make temporary files directory
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


EXD = extraction/
CD = cache/

extract = $(wildcard $(EXD)*$1*.R)
target = $(subst $(EXD),$(CD)$2,$(subst R,csv,$1))
targets = $(foreach a,$1,$(call target,$(a),$2/))
resource = $(EXD)$(subst csv,R,$(notdir $1))

clm_types = img dx

img_dir = dx-imaging
dx_dir = diagnoses

img_srcs = $(call extract,img)
img_targets = $(call targets,$(img_srcs),$(img_dir))
img : $(img_targets)
$(img_targets) : $(img_srcs)
	Rscript $(call resource,$@)

dx_srcs = $(call extract,dx)
dx_targets = $(call targets,$(dx_srcs),$(dx_dir))
dx : $(dx_targets)
$(dx_targets) : $(dx_srcs)
	Rscript $(call resource,$@)

cancers_prerecode.csv.gz : load-claims.R claims
	Rscript $<
cancers_postrecode.csv.gz : recoding.R cancers_prerecode.csv.gz
	Rscript $<
cancers.csv.gz : exclusion.R cancers_postrecode.csv.gz
	Rscript $<

paper_products.rds : premanuscript.R exclusion.R load.R 
	Rscript analysis/$<

tables-and-figures.html : tables-and-figures.Rmd dirs claims cancers analysis
	Rscript -e 'rmarkdown::render("$<", output_format = "html_document")'

clean: rm -f *.aux *.bbl *.blg *.log *.bak *~ *.Rout */*~ */*.Rout */*.aux */*.log

help : Makefile
	@sed -n 's/^##//p' $<




