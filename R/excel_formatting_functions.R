#' Function to return an excel date, which can read into R as seconds since 1899-12-30, as a date object

#' @param date_as_number an excel numeric date
#' @return a date object
#'
#' @export
fromExcelDate <- function(date_as_number){
	as.Date(as.numeric(date_as_number), origin="1899-12-30")
}
