
#' anonymize_df
#'
#' @param df Dataframe that contains columns you want to anonymize
#' @param cols_to_anon Character vector with column names to anonymize
#' @param out What to output, i.e. original df, anonymized df, keys, seeds (note that seeds are currently randomy generated)
#' @param algo The type of algorithm to use
#' @param preserve_NA Change to FALSE if you want NA to be anonymized as well
#' @return A list, a dataframe, or a vector, depening on out
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @export


anonymize_df <- function(df, cols_to_anon, algo = "crc32", preserve_NA = TRUE){

  #Make sure input is valid
  if(!all(cols_to_anon %in% names(df))) stop("cols_to_anon are not all in df")

  #first match unique values to random nums and then these to hash functions
  length_cols <- length(cols_to_anon)
  keys <- vector("list", length_cols)
  names(keys) <- cols_to_anon
  seeds_id <- vector("numeric", length_cols)
  seeds_hash <- vector("numeric", length_cols)

  for (i in c(1:length_cols)){
    seeds_id[i] <- as.numeric(microbenchmark::get_nanotime() / 1000000)
    set.seed(seeds_id[i])
    val <- tibble::tibble(val = unique(df[[cols_to_anon[i]]])) %>%
      mutate(id = sample.int(n(), n()))

    seeds_hash[i] <- round(as.numeric(microbenchmark::get_nanotime() / 1000000))
    val <- mutate(val, hash = anonymizer::anonymize(id, algo, .seed = seeds_hash[i]))

    keys[[i]] <- val
  }

  #NA values will typically be replaced by anonymized character, this prevents that
  if(preserve_NA){
    preserve_NA <- function(df) mutate_at(df, vars("id", "hash"), funs(ifelse(is.na(val), NA, .)))
    keys <- purrr::map(keys, preserve_NA)
  }

  #Create the anonymized dataframe
  anon <- df

  #Recursively replace anonymized value and then reorder to original df order
  for (i in 1:length_cols){
    namei <- cols_to_anon[i]
    anon <- keys[[namei]][c(1,3)] %>%
      right_join(anon, by = c("val" = namei)) %>%
      rename(!!namei := hash) %>%
      select(-one_of("val"))
  }
  anon <- select(anon, one_of(names(df)))

  #output is tibble, not traditional dataframe, so any row names (or other attributes) will be stripped
  output <- list(anon, df, seeds_id, seeds_hash, keys)
  names(output) <- c("anon", "orig", "seeds_id", "seeds_hash", "keys")

  return(output)
}
