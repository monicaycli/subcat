install.packages("versions",
                 repos = "http://cran.rstudio.com",
                 dependencies = TRUE,
                 lib = "/usr/local/lib/R/site-library")

# as of 2018-05-14
# update GAMM related packages
versions::install.dates("devtools", "2018-05-14", dependencies = TRUE)
versions::install.dates("Hmisc", "2018-05-14", dependencies = TRUE)
versions::install.dates("MASS", "2018-05-14", dependencies = TRUE)
versions::install.dates("car", "2018-05-14", dependencies = TRUE)
versions::install.dates("knitr", "2018-05-14", dependencies = TRUE)
versions::install.dates("ggplot2", "2018-05-14", dependencies = TRUE)
versions::install.dates("reshape", "2018-05-14", dependencies = TRUE)
versions::install.dates("gridExtra", "2018-05-14", dependencies = TRUE)
versions::install.dates("cluster", "2018-05-14", dependencies = TRUE)
versions::install.dates("lme4", "2018-05-14", dependencies = TRUE)
versions::install.dates("kableExtra", "2018-05-14", dependencies = TRUE)
versions::install.dates("tidyr", "2018-05-14", dependencies = TRUE)
versions::install.dates("magrittr", "2018-05-14", dependencies = TRUE)
versions::install.dates("VWPre", "2018-08-23", dependencies = TRUE)
versions::install.dates("mgcv", "2018-08-23", dependencies = TRUE)
versions::install.dates("itsadug", "2018-08-23", dependencies = TRUE)
versions::install.dates("stargazer", "2018-05-14", dependencies = TRUE)
