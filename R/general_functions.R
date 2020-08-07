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
#' with round 1.5 and 2.5 woukld both give 2. round_half_up would return 2 and 3.
#'
#' @dbl ... a numeric value
#' @return a rounded numeric value
#' @export
round_half_up <- function(dbl){
	if_else(val %% 1 == .5, ceiling(val), round(val))
}
