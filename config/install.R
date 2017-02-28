setwd("~/projects/turtle-scripts/")
install.packages("spacemakeR", repos="http://R-Forge.R-project.org")
install.packages("adespatial", repos="http://R-Forge.R-project.org")
install.packages("ape")
install.packages("spdep")
install.packages("ade4")
install.packages("vegan")
# install.packages("AEM", repos="http://R-Forge.R-project.org")
install.packages("data/AEM_0.6.tar.gz", repos = NULL, type = "source")
# install.packages("PCNM", repos="http://R-Forge.R-project.org")
install.packages("data/PCNM_2.1-4.tgz", repos = NULL, type = .Platform$pkgType)

# require(devtools); install_github("sdray/adespatial") # forward.sel, replaces packfor
# install.packages("packfor", repos="http://R-Forge.R-project.org")
install.packages("adespatial")

