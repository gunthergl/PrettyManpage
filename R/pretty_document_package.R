#' Pretty document a package
#'
#' Document scripts-manpages in .html *with* running the examples and giving the
#' output for a complete package.
#'
#' @param package.path
#' Where is the package located?
#'
#' @param output.dir
#' Where should the pretty manpages be saved? Usually in "package/inst/pretty_man"
#'
#' file.path("inst", "pretty_man") has gone deprecated because the package size goes
#' then quite up.
#' @param use.existing.rds
#' If TRUE, the function `pretty_document_script_rd` is called instead of `pretty_document_script`.
#' @param verbose
#' Should I give my current status?
#'
#'
#' @return
#' invisible(all_rd_files) # All rd-files which were completed
#' @export
#'
#' @examples
#' # Somehow I could improve here using this dontrun thingy
#' # https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
#' #  search "dontrun"
#' # pretty_document_package(package.path = ".")
pretty_document_package <- function(package.path
									,output.dir=file.path("pretty_man")
									,verbose=TRUE
									,use.existing.rds=FALSE){
	if(package.path != "."){
		if(grepl("/$", package.path)){
			package.path <-  paste0(package.path, ".")
		}else{
			package.path <- paste0(dirname(package.path), ".")
		}
	}

	if(output.dir == file.path("pretty_man")){
		output.dir <- file.path(package.path, output.dir)
	}
	all_r_files <- list.files(file.path(package.path, "R"))
	for(r.fileX in all_r_files){
		if(verbose)
			cat("Start ", r.fileX, "\n\n")
		rd.fileX <- sub("R$", "Rd", r.fileX)
		rd.fileX <- sub("/R/", "/man/", rd.fileX)
		out.file <- sub(".*/", "", r.fileX)
		out.file <- sub(".R$", ".html", out.file)
		tryCatch(expr = {
			if(use.existing.rds){
				pretty_document_script_rd(path.R.script = r.fileX
										  ,path.Rd.file = rd.fileX
										  ,outputfile = file.path(output.dir, out.file))
			}else{
				pretty_document_script(path.R.script = r.fileX
									   ,outputfile = file.path(output.dir, out.file))
			}
		}, error=function(e){
			if(grepl("failed: File does not exist:", e)){
				warning(
					paste0("Error when pretty-helping ", r.fileX
						   , "\n Tried to access a file which does not exist."
						   ,"\n This is probably because the .Rmd which calls the function"
						   ,"\n is not in the root-directory of the package."
						   ,"\n Maybe a simple change that the .Rmds are in this root-dir might solve this"))
			}else{
				stop(e)
			}

		})
		if(verbose)
			cat(r.fileX, "  done\n\n")
	}
	return(invisible(all_r_files))
}

