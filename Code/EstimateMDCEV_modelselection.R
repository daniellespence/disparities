
# Estimate different model specifications
df_mdcev_temp <- df_mdcev %>%
	mutate(birding = ifelse(activity == "birding", 1, 0))

formula_kt = formula(paste("~ 0", "| 0 | 0 "))

# parallel test of all models
df_input <- list(model = list(alpha = "alpha",
							  gamma = "gamma",
							  hybrid = "hybrid",
							  kt_ee = "kt_ee"),
				 mdcev_formula = list(mdcev_vars = mdcev_formula,
				 					 mdcev_vars = mdcev_formula,
				 					 mdcev_vars = mdcev_formula,
				 					 mdcev_vars = formula_kt ))
	
plan(multiprocess, workers = length(df_input$model))


init = list(scale = array(1, dim=c(1)))

df_mdcev_temp <- mdcev.data(data = df_mdcev_temp, 
					   id.var = "id", 
					   alt.var = "activity", 
					   choice = "quant" )

output <- future_pmap(df_input, ~mdcev(formula = .y,
										   data = df_mdcev_temp,
									   initial.parameters = init,
									   gamma_ascs = 1,
									   psi_ascs = 1,
										   model = .x,
										   algorithm = "MLE"),
					  .progress = TRUE,
					  furrr_options(seed = 123))
closeAllConnections()

model_summary <- map_dfr(output, MdcevSelection)
model_summary

save(output, file = file_name_model_selection)
write_csv(model_summary, file_name_model_selection_summary)

rm(mdcev_formula_all_psi, formula_kt, init, df_input, output, model_summary, df_mdcev_temp)
