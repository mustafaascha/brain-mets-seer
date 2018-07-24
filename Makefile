all: dir-cache\
     dir-dx-imaging\
     nch-dx-imaging.csv.gz\
     dme-dx-imaging.csv.gz\
     outsaf-dx-imaging.csv.gz\
     dir-diagnoses\
     nch-icd-dx.csv.gz\
     nch-icd-dx-p.csv.gz\
     dme-icd-dx.csv.gz\
     dme-icd-dx-p.csv.gz\
     outsaf-icd-dx.csv.gz\
     medpar-icd-dx.csv.gz\
     cancers_prerecode.csv.gz\
     cancers_postrecode.csv.gz\
     cancers.csv.gz\
     paper_products.rds\
     tables-and-figures.html

.PHONY: doc rmobj dir-cache dir-diagnoses dir-dx-imaging

R_OPTS=--vanilla

VPATH = cache cache/dx-imaging cache/diagnoses extraction munge analysis
DXDIR = cache/diagnoses/
IMGDIR = cache/dx-imaging/

dir-cache:
	if [ ! -e "cache" ]; then mkdir cache ; fi
dir-diagnoses:
	cd cache;\
	if [ ! -e "diagnoses" ]; then mkdir diagnoses ; fi;\
	cd ..
dir-dx-imaging:
	cd cache;\
	if [ ! -e "dx-imaging" ]; then mkdir dx-imaging; fi;\
	cd ..

#extract data
$(IMGDIR)nch-dx-imaging.csv.gz: cpt-img-nch.R
	Rscript extraction/cpt-img-nch.R
$(IMGDIR)dme-dx-imaging.csv.gz: cpt-img-dme.R
	Rscript extraction/cpt-img-dme.R
$(IMGDIR)outsaf-dx-imaging.csv.gz: cpt-img-out.R
	Rscript extraction/cpt-img-out.R

$(DXDIR)nch-icd-dx.csv.gz: icd-dx-nch.R 
	Rscript extraction/icd-dx-nch.R
$(DXDIR)nch-icd-dx-p.csv.gz: icd-dx-nch-p.R
	Rscript extraction/icd-dx-nch-p.R
$(DXDIR)dme-icd-dx.csv.gz: icd-dx-dme.R
	Rscript extraction/icd-dx-dme.R
$(DXDIR)dme-icd-dx-p.csv.gz: icd-dx-dme-p.R
	Rscript extraction/icd-dx-dme-p.R
$(DXDIR)medpar-icd-dx.csv.gz: icd-dx-mpr.R
	Rscript extraction/icd-dx-mpr.R
$(DXDIR)outsaf-icd-dx.csv.gz: icd-dx-out.R
	Rscript extraction/icd-dx-out.R

cache/cancers_prerecode.csv.gz: load-claims.R\
                                cpt-img-nch.R   cpt-img-dme.R\
                                cpt-img-out.R   icd-dx-nch.R\
                                icd-dx-nch-p.R  icd-dx-dme.R\
                                icd-dx-dme-p.R  icd-dx-mpr.R\
                                icd-dx-out.R 
	Rscript munge/load-claims.R

cache/cancers_postrecode.csv.gz: recoding.R load-claims.R
	Rscript munge/recoding.R 

cache/cancers.csv.gz: exclusion.R recoding.R
	Rscript munge/exclusion.R

paper_products.rds: exclusion.R load.R premanuscript.R
	Rscript analysis/premanuscript.R

tables-and-figures.html: tables-and-figures.Rmd exclusion.R load.R recoding.R premanuscript.R manuscript.R
	Rscript -e 'rmarkdown::render("tables-and-figures.Rmd", output_format = "html_document")'

clean: rm -f *.aux *.bbl *.blg *.log *.bak *~ *.Rout */*~ */*.Rout */*.aux */*.log
