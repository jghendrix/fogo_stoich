unique_sites <- function(DT) {
    
  DT %<>% dplyr::filter(!is.na(x) & !is.na(y)) %>%
    filter(!duplicated(paste0(x,y))) %>%
    dplyr::select(c(x, y))
}
