# usage: Rscript generate_power_reports.R

source("compile_eeg_and_pupil_betas.R")

rmarkdown::render(
  "theta_alpha_corr.Rmd",
  output_format = "html_document"
)

rmarkdown::render(
  "spectral_timeseries.Rmd",
  params = list(
    power_name = "frontal-theta"),
  output_format = "html_document",
  output_file = "MPE_asso_retrieval_frontal_theta.html"
)

rmarkdown::render(
  "spectral_timeseries.Rmd",
  params = list(
    power_name = "posterior-alpha"),
  output_format = "html_document",
  output_file = "MPE_asso_retrieval_posterior_alpha.html"
)

rmarkdown::render(
  "spectral_analyses_and_pupil.Rmd",
  params = list(
    power_name = "frontal-theta"),
  output_format = "html_document",
  output_file = "MPE_asso_retrieval_frontal_theta_and_pupil.html"
)

rmarkdown::render(
  "spectral_analyses_and_pupil.Rmd",
  params = list(
    power_name = "posterior-alpha"),
  output_format = "html_document",
  output_file = "MPE_asso_retrieval_posterior_alpha_and_pupil.html"
)

rmarkdown::render(
  "spectral_analyses_and_sub_mem.Rmd",
  params = list(
    power_name = "frontal-theta"),
  output_format = "html_document",
  output_file = "MPE_frontal_theta_submem.html"
)

rmarkdown::render(
  "spectral_analyses_and_sub_mem.Rmd",
  params = list(
    power_name = "posterior-alpha"),
  output_format = "html_document",
  output_file = "MPE_posterior_alpha_submem.html"
)

rmarkdown::render(
  "spectral_analyses_include_incorrect.Rmd",
  params = list(
    power_name = "frontal-theta"),
  output_format = "html_document",
  output_file = "frontal_theta_include_incorrect.html"
)

rmarkdown::render(
  "spectral_analyses_include_incorrect.Rmd",
  params = list(
    power_name = "posterior-alpha"),
  output_format = "html_document",
  output_file = "posterior_alpha_include_incorrect.html"
)



