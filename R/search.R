
#' @export
soc_search_dataset <- function(text, socrata){
  if(!"soc_list" %in% class(socrata)){
    socrata <- soc_list(socrata)
  }
  sl <- dplyr::as_tibble(socrata)

  results <- filter_keywords(sl, text, c("title", "description"))
  results %>% dplyr::select(title, description, dataset_id, landingPage)

}

filter_keywords <- function (d, keywords, cols = NULL){
  if (is.null(cols))
    cols <- names(d)
  non_chr_cols <- map_lgl(d, ~!is.character(.)) %>% keep(~.) %>%
    names()
  ds <- d %>% modify_at(vars(one_of(non_chr_cols)), as.character)
  pattern <- paste0(keywords, collapse = "|")
  ds %>% filter_at(vars(one_of(cols)),
                   any_vars(grepl(pattern, ., ignore.case = TRUE)))
}





#'
#' #' @export
#' secop_search_name <- function(str, match = "equals", table = "secop1"){
#'
#'   l <- list(
#'     list(var = "nom_raz_social_contratista", val = str, match = match),
#'     list( var = "nombre_del_represen_legal", val = str, match = match)
#'   )
#'   search_url <- soc_query_list(l, logical = "or", table = "secop1")
#'   read.socrata(search_url)
#' }
#'
#' #' @export
#' secop_search_id <- function(id, table = "secop1"){
#'   if(cc_or_nit(id) == "nit"){
#'     mix <- mix_nit(id)
#'   }else if(cc_or_nit(id) == "cc"){
#'     mix <- mix_cc(id)
#'   }else{
#'     # other
#'     mix <- id
#'   }
#'   l <- list(
#'     list(var = "identificacion_del_contratista", val = mix, match = "in"),
#'     list(var = "identific_del_represen_legal", val = mix, match = "in")
#'   )
#'   search_url <- soc_query_list(l, logical = "or", table = "secop1")
#'   read.socrata(search_url)
#' }
#'
#'
#'
#' #' @export
#' save_contracts_zip <- function(contratos, zip_path){
#'   dir <- file.path(tempdir(), sample(1000000,1))
#'   dir.create(dir)
#'   on.exit(unlink(dir, recursive = TRUE))
#'   walk(seq_along(contratos), function(i){
#'     d <- contratos[[i]]
#'     write_csv(d, file.path(dir,gsub(" ","_",paste0(names(contratos[i]),".csv"))))
#'   })
#'   list.files(dir)
#'   zip::zipr(file.path(zip_path),
#'             list.files(dir, full.names = TRUE))
#'   zip_path
# }



