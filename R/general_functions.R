#' List files and return the entire local url including parent folders
#'
#' Function to list all files in a directory including full paths
#'
#' @param dir a directory
#' @return a character vector comprised of full paths to all files in the specified directory
#' @export
list_files <- function(dir){
	list.files(dir) %>%
		paste0(dir,'/',.)
}

#' Pull a column from a data frame or tibble and return it as a vector of unique values
#'
#' Function to pull a column from a data frame or tibble and return it as a vector of unique values
#'
#' @param ... a column
#' @return a character vector comprised of all unique values in a given column in a tibble
#' @export
pu <- function(df,...){

	df %>%
		dplyr::pull(...) %>%
		unique
}

#' Rounding function to round values of exactly n.5 up
#'
#' Function to round values of exactly n.5 up rather than round, which would round even numbers down and odd up:
#' with round 1.5 and 2.5 would both give 2. round_half_up would return 2 and 3.
#'
#' @param dbl a numeric value
#' @return a rounded numeric value
#' @export
round_half_up <- function(dbl){
	if_else(dbl %% 1 == .5, ceiling(dbl), round(dbl))
}


#' Check if your R version is up to date
#'
#' Function to check current R version against the latest release from CRAN
#'
#' @export
checkRVersion <- function(){

	current_version <- (version$major) %>% paste(version$minor,sep='.')

	url <- 'https://cran.r-project.org/bin/macosx/'

	page <- url %>%
		xml2::read_html(.)

	hrefs <- page %>%
		rvest::html_nodes('a') %>%
		rvest::html_attr('href') %>%
		.[grepl('^R-',.)]


	pkg_nums <- hrefs %>%
		stringr::str_extract('-[0-9]\\.[0-9]\\.[0-9]') %>%
		stringr::str_extract_all('[0-9]') %>%
		purrr::map_dbl(~stringr::str_c(unlist(.x),collapse = '') %>% as.numeric)

	latest_version <- hrefs[which(pkg_nums == max(pkg_nums, na.rm = T))]


	version_url <- paste0('https://cran.r-project.org/bin/macosx/',latest_version)


	latest_version_num <- latest_version %>%
		stringr::str_remove_all('R-|\\.pkg')

	if(current_version == latest_version_num){
		print("You're up to date! Cran-tastic.")
	} else {
		warning(paste0('Please update R to version ', latest_version,'. You can find the files at ',
									 url, ' or you can download ',latest_version, ' directly from ', version_url, '.'))
	}

}
