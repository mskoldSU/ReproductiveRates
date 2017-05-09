FIGS := Figs/counts_and_rain_fig.pdf Figs/high_low_prec_fig.pdf Figs/logistic_vs_exp_fig.pdf Figs/rate_vs_rain_fig.pdf Figs/sd_fig.pdf Figs/waterbuck_giraffe_central_fig.pdf Figs/waterbuck_rates_fig.pdf
FIGS.SUPP := Figs_supp/giraffe_par_fig.pdf Figs_supp/giraffe_pop_fig.pdf Figs_supp/impala_par_fig.pdf Figs_supp/impala_pop_fig.pdf Figs_supp/kudu_par_fig.pdf Figs_supp/kudu_pop_fig.pdf Figs_supp/sable_par_fig.pdf Figs_supp/sable_pop_fig.pdf Figs_supp/waterbuck_par_fig.pdf Figs_supp/waterbuck_pop_fig.pdf Figs_supp/wildebeest_par_fig.pdf Figs_supp/wildebeest_pop_fig.pdf Figs_supp/zebra_par_fig.pdf Figs_supp/zebra_pop_fig.pdf
DATA := $(wildcard data/*)
MCMC := mcmc/Waterbuck.Rdata mcmc/Giraffe.Rdata mcmc/Impala.Rdata mcmc/Kudu.Rdata mcmc/Sable.Rdata mcmc/Zebra.Rdata

all: $(MCMC) $(FIGS) $(FIGS.SUPP) ReproductiveRates.pdf SupportingInformation.pdf

$(MCMC): mcmc_all.R load_data.R $(DATA)
	Rscript --no-save $<
	
$(FIGS): figures_main.R load_data.R $(MCMC) $(DATA)
	Rscript --no-save $<

$(FIGS.SUPP): figures_supp.R load_data.R $(MCMC) $(DATA)
	Rscript --no-save $<
	
ReproductiveRates.pdf: ReproductiveRates.Rmd mcmc_all.R load_data.R figures_main.R mcmc/Waterbuck.Rdata $(FIGS) $(DATA) $(MCMC) MS_Bounded_rmax.bib
	Rscript --no-save -e 'rmarkdown::render("$<")'

SupportingInformation.pdf: SupportingInformation.Rmd mcmc_all.R load_data.R figures_supp.R $(FIGS) $(DATA) $(MCMC) MS_Bounded_rmax.bib
	Rscript --no-save -e 'rmarkdown::render("$<")'