
#df_mdcev <- CreatePsiASCs(df_mdcev)

df_mdcev_temp <- df_mdcev %>%
	select(-n_people, -num_overnt_trips, -age, -region)


df_mdcev_temp <- mdcev.data(data = df_mdcev_temp, id.var = "id", 
					   alt.var = "activity", choice = "quant" )

plan(multiprocess, workers = length(lc_list))

output_lc <- future_map(lc_list, 
						~mdcev(formula = mdcev_formula_lc,
								data = df_mdcev_temp,
								n_classes = .x,
								single_scale = 1,
								model = model,
								algorithm = "MLE"),
						.progress = TRUE,
						furrr_options(seed = 123))
closeAllConnections()


model_summary <- map_dfr(output_lc, MdcevSelection)

model_summary

save(output_lc, file = file_name_nclass_selection)
write_csv(model_summary, file_name_nclass_selection_summary)

rm(df_mdcev_temp, lc_list, output_lc, model_summary)
