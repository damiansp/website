#install.packages('knitr', dependencies = T, repos = 'http://cran.us.r-project.org')
library(knitr)

knit2html('~/Desktop/PRA/WebsiteRoot/diseaseSim/simDeer.rmd', 
		  '~/Desktop/PRA/WebsiteRoot/diseaseSim/simDeer' )