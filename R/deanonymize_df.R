

#' deanonymize dataframe
#'
#' @param anonymized_df Dataframe with values in specified columns that have been anonymized
#' @param keys List of dataframes generated from anonymize_df
#' @param cols_to_deanon Default uses names from `keys`, but can specify specific columns
#' @param key_col_anon The column position or name in the `keys` dataframes where the current anonymized value is
#' @param key_col_val The column position or name in the `keys` dataframes where the original value is
#'
#' @return A dataframe.
#' @export

deanonymize_df <- function(anonymized_df, keys, cols_to_deanon = "names(keys)", key_col_anon = 3, key_col_val = 1){

  #input an anonymized_df, and the keys to deanonymize, this is useful for
  #deanonymizing tables that perhaps have changed form since they were
  #deanonymized and wanting to inspect e.g. an aggregated form

  #current method is inconsistent with indiex match approach in anonymize_df

  if (cols_to_deanon == "names(keys)") {
    cols_to_deanon <- names(keys)
  }

  #Make sure input is valid
  if(!all(cols_to_deanon %in% names(anonymized_df))) stop("cols_to_anon are not all in df")

  length_cols <- length(cols_to_deanon)
  for (i in 1:length_cols){
    indexes <- match(anonymized_df[[cols_to_deanon[i]]], keys[[cols_to_deanon[i]]][[key_col_anon]])
    anonymized_df[[cols_to_deanon[i]]] <- keys[[cols_to_deanon[i]]][[key_col_val]][indexes]
  }
  return(anonymized_df)
}
