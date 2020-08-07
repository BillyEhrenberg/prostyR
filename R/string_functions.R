#' Function to return all characters in a vector that match a pattern

#' @param c a character vector
#' @param pattern a string or regex pattern to match
#' @return a character vector comprised of all elements of c that match pattern
#'
#' @export
str_get_matches <- function(c, pattern){

	c %>%
		.[grepl(pattern, .)]

}
