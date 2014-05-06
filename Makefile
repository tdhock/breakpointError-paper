HOCKING-breakpointError.pdf: HOCKING-breakpointError.tex refs.bib
	pdflatex HOCKING-breakpointError
	bibtex HOCKING-breakpointError
	pdflatex HOCKING-breakpointError
	pdflatex HOCKING-breakpointError

figures/variable-density-sigerr.tex: figures/variable-density-sigerr.R data/variable.density.show.RData scripts/fp.fn.colors.R
	R --vanilla < $<
figures/variable-density-annotation-cost.png: figures/variable-density-annotation-cost.R data/variable.density.show.RData scripts/geom_tallrect.R scripts/breakpoint.colors.R scripts/signal.colors.R
	R --vanilla < $<
figures/variable-breaks-constant-size-alpha.tex: figures/variable-breaks-constant-size-alpha.R data/variable.breaks.constant.size.RData
	R --vanilla < $<
figures/variable-breaks-constant-size.tex: figures/variable-breaks-constant-size.R data/variable.breaks.constant.size.show.RData scripts/signal.colors.R
	R --vanilla < $<
figures/variable-breaks-constant-size-berr.tex: figures/variable-breaks-constant-size-berr.R data/variable.breaks.constant.size.show.RData
	R --vanilla < $<
figures/variable-size-berr.tex: figures/variable-size-berr.R data/variable.size.show.RData
	R --vanilla < $<
figures/variable-size-error-alpha.tex: figures/variable-size-error-alpha.R data/variable.size.signals.RData
	R --vanilla < $<
figures/breakpoint-error-pieces.tex: figures/breakpoint-error-pieces.R scripts/signal.colors.R
	R --vanilla < $<
figures/variable-size-signals.png: figures/variable-size-signals.R data/variable.size.show.RData scripts/signal.colors.R
	R --vanilla < $<
figures/variable-scale-signals.tex: figures/variable-scale-signals.R data/variable.scale.show.RData scripts/signal.colors.R
	R --vanilla < $<
figures/variable-scale-berr.tex: figures/variable-scale-berr.R data/variable.scale.show.RData
	R --vanilla < $<
figures/variable-density-error-train.tex: figures/variable-density-error-train.R data/variable.density.signals.RData
	R --vanilla < $<
figures/variable-density-error-alpha.tex: figures/variable-density-error-alpha.R data/variable.density.signals.RData 
	R --vanilla < $<
figures/variable-density-error-alpha-flsa.tex: figures/variable-density-error-alpha-flsa.R data/variable.density.signals.RData 
	R --vanilla < $<
figures/variable-scale-error-alpha.tex: figures/variable-scale-error-alpha.R data/variable.scale.signals.RData 
	R --vanilla < $<
figures/variable-density-signals.png: figures/variable-density-signals.R data/variable.density.show.RData scripts/signal.colors.R
	R --vanilla < $<
figures/variable-density-berr.tex: figures/variable-density-berr.R data/variable.density.show.RData
	R --vanilla < $<
figures/variable-density-berr-flsa.tex: figures/variable-density-berr-flsa.R data/variable.density.show.RData
	R --vanilla < $<
figures/penalty-2-size.tex: figures/penalty-2-size.R
	R --vanilla < $<
figures/penalty-1-points.tex: figures/penalty-1-points.R
	R --vanilla < $<
figures/penalty-4-variance.tex: figures/penalty-4-variance.R
	R --vanilla < $<
figures/penalty-1-cghseg.tex: figures/penalty-1-cghseg.R
	R --vanilla < $<
tables/penalty-real-data.tex: tables/penalty-real-data.R data/all.stats.RData
	R --vanilla < $<
