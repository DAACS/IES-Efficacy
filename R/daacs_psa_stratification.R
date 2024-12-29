daacs_psa_stratification <- function(
		df,
		rows, 
		outcome_col, 
		outcome_label,
		group_label,
		institution,
		treatment_col = 'Complier',
		strata_col = 'strata',
		digits = 2
) {
	psa_out <- PSAgraphics::circ.psa(
		df[rows, outcome_col],
		treatment = df[rows,treatment_col],
		strata = df[rows,strata_col])
	# TODO: Add parameter to save the plot
	out_df <- data.frame(
		outcome = outcome_label,
		group = group_label,
		institution = institution,
		treatment = psa_out$wtd.Mn.TRUE,
		control = psa_out$wtd.Mn.FALSE,
		ATE = psa_out$ATE,
		Cohens_d = psa_out$ATE / sd(df[rows,outcome_col]),
		statistic = psa_out$approx.t,
		df = psa_out$df,
		p = 2 * (1 - pt(abs(psa_out$approx.t), psa_out$df))
	)
	
	# Reformat p-value (maybe make a parameter)
	out_df$p <- sapply(
		out_df$p, FUN = function(x) {
			ifelse(x < 1 / 10^digits,
				   paste0('< ', 1 / 10^digits),
				   sprintf(paste0("%.", digits, "f"), x)
			)
		}
	)
	
	return(out_df)
}
