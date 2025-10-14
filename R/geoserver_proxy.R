# R/geoserver_proxy.R
# -------------------
# Démarre/arrête un petit proxy WMS authentifié (Basic Auth) vers GeoServer.
# Lit la config dans l'environnement:
#   GEOSERVER_USER, GEOSERVER_PASS, GEOSERVER_OWS, GEOSERVER_PROXY_PORT
# Dépendances (installer une fois) : plumber, httr2, callr

geoserver_proxy_start <- function() {
  user <- Sys.getenv("GEOSERVER_USER", "")
  pass <- Sys.getenv("GEOSERVER_PASS", "")
  ows  <- Sys.getenv("GEOSERVER_OWS",  "")
  port <- as.integer(Sys.getenv("GEOSERVER_PROXY_PORT", "8888"))
  
  stopifnot(nzchar(user), nzchar(pass), nzchar(ows), is.finite(port), port > 0)
  
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Le package 'callr' est requis. Installez-le : install.packages('callr')")
  }
  
  proxy_proc <- callr::r_bg(
    func = function(port, user, pass, ows) {
      if (!requireNamespace("plumber", quietly = TRUE) ||
          !requireNamespace("httr2", quietly = TRUE)) {
        stop("Installer 'plumber' et 'httr2' dans l'environnement d'exécution du proxy.")
      }
      pr <- plumber::pr()
      pr$handle("GET", "/wms", function(req, res) {
        q <- req$argsQuery
        if (is.null(q$service) || !nzchar(q$service)) q$service <- "WMS"
        
        gs_req <- httr2::request(ows) |>
          httr2::req_url_query(!!!q) |>
          httr2::req_auth_basic(user, pass) |>
          httr2::req_headers(
            "Accept" = "image/png, application/xml;q=0.9, */*;q=0.8",
            "User-Agent" = "shiny-plumber-wms-proxy"
          )
        
        gs_resp <- tryCatch(httr2::req_perform(gs_req), error = function(e) e)
        if (inherits(gs_resp, "error")) {
          res$status <- 502
          res$setHeader("Content-Type", "text/plain; charset=utf-8")
          res$body <- paste("Proxy error:", gs_resp$message)
          return(res)
        }
        res$status <- httr2::resp_status(gs_resp)
        ctype <- httr2::resp_header(gs_resp, "content-type")
        if (is.null(ctype) || identical(ctype, "")) ctype <- "application/octet-stream"
        res$setHeader("Content-Type", ctype)
        res$body <- httr2::resp_body_raw(gs_resp)
        res
      })
      pr$run(port = port, host = "127.0.0.1")
    },
    args = list(port = port, user = user, pass = pass, ows = ows),
    supervise = TRUE
  )
  
  list(
    base_url = sprintf("http://127.0.0.1:%d/wms", port),
    process  = proxy_proc
  )
}

geoserver_proxy_stop <- function(proxy) {
  if (!is.null(proxy) && !is.null(proxy$process)) {
    p <- proxy$process
    if (inherits(p, "r_process") && p$is_alive()) p$kill()
  }
  invisible(TRUE)
}
