#' Pretty document scripts
#'
#' Document scripts in .html *with* running the examples and giving the output
#'
#' I understand that this is no wanted standard behaviour because this takes much
#' space.
#' @param path.R.script
#' 	Path to the R-script
#' @param outputfile
#' Where should the output html be saved?
#' @param currentpackage
#' If currentpackage is supplied, currentpackage is loaded before every example.
#'
#' Maybe not necessary
#' @param check_package
#' Should the function be checked? Default false because it takes much time and
#' I assume that the package has to be built in other ways.
#'
#' @return
#' invisible(NULL)
#' @export
#'
#' @examples
#'
#' pretty_document_script(
#' 	path.R.script = file.path("PrettyManpage", "hello2.R") #"R/hello2.R"
#' 	,outputfile = "prettyhelp/hello2.html"
#' 	,check_package = FALSE)
#'
pretty_document_script <- function(path.R.script
								   ,outputfile
								   ,currentpackage
								   ,check_package=FALSE){
	if(missing(outputfile)){
		outputfile <- sub("\\.R", "", path.R.script)
	}
	output.dir <- dirname(outputfile)
	if(!dir.exists(output.dir)){
		dir.create(output.dir, recursive = TRUE)
	}

	tmpdir <- file.path(dirname(path.R.script), "tmp_removeme")
	dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
	# Building the R helppage
	# Package checking should be done during build process. It just takes time.
	withCallingHandlers(expr = {
		withRestarts({
			documented.paths <- document::document(path.R.script, check_package = check_package
												   ,output_directory = tmpdir)
		}, muffleStopWarning=function() {
			message(paste0("No roxygen comments in ", path.R.script, " --> File skipped"))
		})

	}, warning=function(w){
		if(grepl("Couldn't find roxygen comments", w)){
			invokeRestart("muffleStopWarning")
		}else{
			warning(w)
			invokeRestart("muffleWarning")
		}
		return(NULL)
	})
	if(!exists("documented.paths"))
		return(invisible(NULL))

	examples <- readLines(documented.paths$txt_path)
	scriptname <- examples[1]
	examples <- examples[-c(1:which(examples == "Examples:"))]
	examples <- sub("^[ ]*", "", examples)

	# Extracting the examples
	# examples <- utils::capture.output(tools::Rd2ex(path.Rd.file))
	if(length(examples) == 0){
		warning(paste0("No \"Examples:\" found in ", documented.paths$txt_path))
		return(NULL)
	}
	example.tmp_R <- file.path(tmpdir, "example_tmpfile.R")
	example.tmp_html <- file.path(tmpdir, "example_tmpfile.html")
	# make a new file really only including the examples and no further information
	sink(example.tmp_R)
	if(!missing(currentpackage)){
		cat("library(\"", currentpackage, "\")\n", sep="")
	}
	cat(examples, sep="\n")
	sink()
	# render the examples to markdown
	rmarkdown::render(example.tmp_R, output_format = "html_document")

	# read the examples html
	run_example.html <- xml2::read_html(example.tmp_html)
	# read the helppage html
	manpage.html <- xml2::read_html(documented.paths[["html_path"]])
	# extract the body of the helppage
	manpage.roxygen.body <- xml2::xml_children(xml2::xml_children(manpage.html)[2])
	# remove strange table
	xml2::xml_remove(manpage.roxygen.body[1])
	# remove all code-parts
	xml2::xml_remove(xml2::xml_find_all(manpage.html, xpath = "//pre"))

	# replace title and author by the date
	a <- xml2::xml_replace(xml2::xml_children(xml2::xml_find_all(x = run_example.html, xpath = "//div[@class='fluid-row']"))[1:2]
						   ,xml2::xml_children(xml2::xml_find_all(x = run_example.html, xpath = "//div[@class='fluid-row']"))[3])

	# replace the second element by the manpage.html
	a <- xml2::xml_replace(xml2::xml_children(xml2::xml_find_all(x = run_example.html, xpath = "//div[@class='fluid-row']"))[2]
						   ,xml2::xml_children(manpage.html)[2])
	# remove the third element (redundant and unnecessary)
	xml2::xml_remove(xml2::xml_children(xml2::xml_find_all(x = run_example.html, xpath = "//div[@class='fluid-row']"))[3])

	# Overwrite the html title (Shown in the browser)
	title.node <- xml2::xml_find_all(x = run_example.html, xpath = "//title")
	xml2::xml_text(title.node) <- paste0(scriptname, "()")

	# write the final html
	xml2::write_html(run_example.html, outputfile)

	# clean the tmp.directory
	unlink(tmpdir, recursive = TRUE)
	return(invisible(NULL))
}

