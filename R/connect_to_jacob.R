connect_to_jacob=function(){
  conn <- DBI::dbConnect(RPostgres::Postgres(),
                         host = Sys.getenv("JACOB_HOST"), 
                         port = Sys.getenv("JACOB_PORT"),
                         dbname = Sys.getenv("JACOB_NAME"), 
                         user = Sys.getenv("JACOB_USER_APP"),
                         password = Sys.getenv("JACOB_PASS_APP"))
  return(conn)
}
