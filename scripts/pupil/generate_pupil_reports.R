# usage: Rscript generate_pupil_reports.R

rmarkdown::render(
  "pupil_retrieval_timeseries.Rmd",
  output_format = "html_document"
)

rmarkdown::render(
  "pupil_temporal_pca.Rmd",
  output_format = "html_document"
)

rmarkdown::render(
  "pupil_retrieval_timeseries_by_experiment.Rmd",
  output_format = "html_document"
)

rmarkdown::render(
  "pupil_retrieval_timeseries_include_incorrect.Rmd",
  output_format = "html_document"
)

rmarkdown::render(
  "pupil_temporal_pca_bootstrap.Rmd",
  output_format = "html_document"
)

rmarkdown::render(
  "pupil_temporal_pca_include_incorrect.Rmd", 
  output_format = "html_document"
)

rmarkdown::render(
  "compare_experiment_tPCA_loadings.Rmd", 
  output_format = "html_document"
)

rmarkdown::render(
  "pupil_trial_history_effects.Rmd", 
  output_format = "html_document"
)




