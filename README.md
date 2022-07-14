# Analytic code for "Authorship concentration in health sciences journals from Latin America and the Caribbean"

[![](https://zenodo.org/static/img/orcid.png)](https://orcid.org/0000-0003-4064-433X) Fontenelle, Leonardo Ferreira

Analysis code for [Fontenelle (2022)](https://doi.org/10.1590/SciELOPreprints.3647 "preprint"); written with the [R language and environment for statistical computing](https://www.r-project.org/).

-   *LILACS.Rproj*, *.Rprofile*, *renv.lock* and *renv/activate.R* - used by {[renv](https://cran.r-project.org/package=renv)} to assure the appropriate versions of the necessary packages are installed.
-   *tests/test_functions.R* - provides unit testing for *0_functions.R*.
-   *0_functions.R* - provides auxiliary functions for  the next two scripts.
-   *1_summarize.R* - uses data available from the LILACS (Latin American and the Caribbean Literature on Health Sciences) bibliographic database and [data deposited in Zenodo](https://doi.org/10.5281/zenodo.6126801) to create a derived dataset, which was [also deposited in Zenodo](https://doi.org/10.5281/zenodo.6126801).
-   *2_describe.R* - uses [data deposited in Zenodo](https://doi.org/10.5281/zenodo.6126801) and also other authors' [data deposited in OSF](https://osf.io/bvzfp/) to produce the tables and figures in [Fontenelle (2022)](https://doi.org/10.1590/SciELOPreprints.3647 "preprint").
