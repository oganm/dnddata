library(here)

set_file_wd = function(){
	command = commandArgs(trailingOnly = FALSE)

	file = gsub('--file=','',command[grepl('--file',command)])
	if(length(file) == 1){
		setwd(dirname(file))
	}
}
set_file_wd()

setwd(here())
print(.libPaths())

rmarkdown::render(input = 'README.Rmd',output_file = 'README.md')
