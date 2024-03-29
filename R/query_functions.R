#' Get the names of a BigQuery table
#'
#' Function to return a character vector containing all the names of a big query table filtered by pattern if specified
#'
#' @param x a BigQuery table
#' @return a character vector comprised of all column names
#'
#' @export
lazy_names <- function(x){

	nms <- x %>%
		unlist %>%
		.[grepl("vars",names(.))] %>%
		as.character()

	if(length(grep("~",nms) > 0)){
		nms <- nms[grep("~",nms)] %>%
			gsub("~","",.)
	}

	return(nms)

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

#' load a table from the SQL location
#'
#' Function to load a table from the SQL location
#'
#' @param connection a BQ connection
#' @return a lazy query
#' @export
source_bq_tbl2 <- function(connection, table_location){

	lookup <- strsplit(table_location,"\\.") %>%
		unlist %>%
		purrr::set_names(c("dataset","schema","table")) %>%
		as.list

	out <- source_bq_tbl(connection,
											 table = data$table,
											 schema = data$schema,
											 dataset = data$dataset)

	return(out)


}
