all: dirs claims cancers analysis manuscript

.PHONY : dirs claims cancers analysis manuscript clean help

R_OPTS = --vanilla
VPATH  = extraction:munge:analysis:cache:cache/dx-imaging:cache/diagnoses
EXD    = extraction/
CD     = cache/
DXDIR  = $(CD)diagnoses
IMGDIR = $(CD)dx-imaging

## dirs        : Create cache directories
dirs : cache cache/dx-imaging cache/diagnoses

## claims      : Extract relevant records from claims
claims : img dx

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
	if [ ! -e $(CD) ]; then mkdir cache ; fi
cache/diagnoses :
	if [ ! -e $(DXDIR) ]; then mkdir $(DXDIR) ; fi;
cache/dx-imaging :
	if [ ! -e $(IMGDIR) ]; then mkdir $(IMGDIR); fi;

extract  = $(wildcard $(EXD)*$1*.R)
target   = $(subst $(EXD),$2,$(subst R,csv.gz,$1))
targets  = $(foreach a,$1,$(call target,$(a),$2/))
resource = $(EXD)$(subst csv.gz,R,$(notdir $1))

img_srcs       = $(call extract,img)
img_targets    = $(call targets,$(img_srcs),$(IMGDIR))
img            : $(img_targets)
$(img_targets) : $(img_srcs)
	Rscript $(call resource,$@)

dx_srcs       = $(call extract,dx)
dx_targets    = $(call targets,$(dx_srcs),$(DXDIR))
dx            : $(dx_targets)
$(dx_targets) : $(dx_srcs)
	Rscript $(call resource,$@)

cancers_prerecode.csv.gz :  load-claims.R claims
	Rscript $<
cancers_postrecode.csv.gz : recoding.R cancers_prerecode.csv.gz
	Rscript $<
cancers.csv.gz :            exclusion.R cancers_postrecode.csv.gz
	Rscript $<

paper_products.rds :        premanuscript.R exclusion.R load.R 
	Rscript $<
tables-and-figures.html :   tables-and-figures.Rmd dirs claims cancers analysis
	Rscript -e 'rmarkdown::render("$<", output_format = "html_document")'

clean: rm -f *.aux *.bbl *.blg *.log *.bak *~ *.Rout */*~ */*.Rout */*.aux */*.log

help : Makefile
	@sed -n 's/^##//p' $<

debug: 
	@echo img targets: $(img_targets)
	@echo
	@echo dx targets: $(dx_targets)
	@echo


