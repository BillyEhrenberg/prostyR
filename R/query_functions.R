#' Get the names of a BigQuery table
#'
#' Function to return a character vector containing all the names of a big query table filtered by pattern if specified
#'
#' @param tbl a BigQuery table
#' @param pattern a string or regex pattern used to match the column names in tbl
#' @return a character vector comprised of all column names OR if pattern is specified all column names that match pattern
#'
#' @export
get_tbl_names <- function (tbl, pattern = NULL)
{
	res <- tbl$ops %>% unlist %>% .[grepl("vars", names(.))] %>%
		as.character %>%
		.[!grepl("^\\~", .)] %>%
		unique

	if (!is.null(pattern)) {
		res <- res %>%
			str_get_matches(pattern)
	}
	return(res)
}


#' Function to return a character vector containing all the names of a big query table, filtered by pattern if specified
#' @param tbl a BigQuery table
#' @param n number of ids or rows to sample
#' @param distinct_on column on which to call dplyr::distinct. If unspecified function will return n rows.
#' @param keep_all should the function find n unique values for distinct_on and return all rows containing those values, T,
#'  or n rows, each with a distinct value for distinct_on.
#' @return Lazy query with with n rows or n unique values of distinct_on
#' @export
tbl_sample_n <- function(tbl, n, distinct_on = NULL, keep_all = T){

	if(!is.null(distinct_on)){

		distinct_on <- rlang::ensym(distinct_on)

		if(keep_all){

			ids <- tbl %>%
				dplyr::arrange(sql("RAND()")) %>%
				dplyr::distinct(!!distinct_on) %>%
				utils::head(n)

			res <- tbl %>%
				dplyr::filter(!!distinct_on %in% ids)

		} else {
			res <- tbl %>%
				dplyr::arrange(sql("RAND()")) %>%
				dplyr::group_by(!!distinct_on) %>%
				dplyr::filter(row_number() == 1) %>%
				dplyr::ungroup %>%
				utils::head(n)
		}

	} else {

		res <- tbl %>%
			dplyr::arrange(sql("RAND()")) %>%
			utils::head(n)

	}
	res
}


#' Render and copy to clipboard an sql query
#'
#' Function to render and copy to clipboard a sql query
#'
#' @param ... a column
#' @return a character vector comprised of all unique values in a given column in a tibble
#' @export
sql_copy <- function(query){
	query %>%
		dbplyr::sql_render(.) %>%
		clipr::write_clip(.)
}
