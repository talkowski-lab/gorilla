skip_tabix <- function() {
    # tabix queries unconditionally use an OAuth2 token so we skip of either
    # tabix or gcloud is missing
    paths <- Sys.which(c("tabix", "gcloud"))
    if (!all(nzchar(paths))) {
        skip("not running without tabix or gcloud")
    } else {
        invisible()
    }
}
