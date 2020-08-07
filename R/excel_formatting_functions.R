#' Function to return an excel date, which can read into R as seconds since 1899-12-30, as a date object

#' @param date_as_number an excel numeric date
#' @return a date object
#'
#' @export
fromExcelDate <- function(date_as_number){
	as.Date(as.numeric(date_as_number), origin="1899-12-30")
}







#' Function to set a given row of a data frame as column headers

#' @param df a data.frame or tibble
#' @param row the row to set as column headers
#' @param clean_col_names should the column names be cleaned using janitor::clean_names
#' @param add_index should columns be given a trailing unique integer to avoid duplicate names
#' @return a date object
#'
#' @export
row_to_col_names <- function(df, row = 1, clean_col_names = F){


	df <- df %>%
		purrr::set_names(.[row,]) %>%
		.[(row+1):nrow(.),]


	if(clean_col_names == T){

		df <- df %>%
			janitor::clean_names(.)

	}

	return(df)
}
