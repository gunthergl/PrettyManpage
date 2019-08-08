#' Pretty document scripts
#'
#' Document scripts in .html *with* running the examples and giving the output
#'
#' I understand that this is no wanted standard behaviour because this takes much
#' space.
#' @param path.R.script
#' 	Path to the R-script
#' @param path.Rd.file
#' path to the corresponding .Rd file
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
#' # From these examples it seems that
#' #	- currentpackage is unnecessary
#' #	- Not exporting seems no problem.
#'
#' pretty_document_script(
#' 	path.R.script = "R/hello2.R"
#' 	,path.Rd.file = "man/hello2.Rd"
#' 	,outputfile = "prettyhelp/hello2.html"
#' 	# ,currentpackage = "DocumentFunctions"
#' 	,check_package = FALSE)
#'
#' pretty_document_script(
#' 	path.R.script = "R/hello2.R"
#' 	,path.Rd.file = "man/hello2.Rd"
#' 	,outputfile = "prettyhelp/hello2_withCurrentPackage.html"
#' 	,currentpackage = "DocumentFunctions"
#' 	,check_package = FALSE)
#'
#'
#' # pretty_document_script(
#' #	path.R.script = "../Rvarious/R/applySignature.R"
#' #	,path.Rd.file = "../Rvarious/man/applySignature.Rd"
#' #	,outputfile = "../Rvarious/prettyhelp/applySignature.html"
#' #	,check_package = FALSE)
#'
#'
#'
pretty_document_script <- function(path.R.script
		 ,path.Rd.file
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
	dir.create(tmpdir, recursive = TRUE)
	# Building the R helppage
	# Package checking should be done during build process. It just takes time.
	documented.paths <- document::document(path.R.script, check_package = check_package
										   ,output_directory = tmpdir)

	# Extracting the examples
	examples <- utils::capture.output(tools::Rd2ex(path.Rd.file))
	if(length(examples) == 0){
		warning("No examples found by tools::Rd2ex() in ", path.Rd.file)
		return(NULL)
	}
	scriptname <- sub("### Name: ", "", examples[1])
	example.tmp_R <- file.path(tmpdir, "example_tmpfile.R")
	example.tmp_html <- file.path(tmpdir, "example_tmpfile.html")
	# make a new file really only including the examples and no further information
	sink(example.tmp_R)
	if(!missing(currentpackage)){
		cat("library(\"", currentpackage, "\")\n", sep="")
	}
	cat(examples[-c(1:which("### ** Examples" == examples))], sep="\n")
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
