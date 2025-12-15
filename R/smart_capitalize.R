smart_capitalize <- function(txt) {
  if (is.null(txt)) return(character(0))
  
  txt <- tryCatch(as.character(txt), error = function(e) rep("", length(txt)))
  txt <- unlist(txt)
  
  out <- txt
  
  for (i in seq_along(out)) {
    t <- out[i]
    
    if (is.na(t) || !nzchar(t)) {
      out[i] <- ""
      next
    }
    
    # tout en minuscule
    t <- tolower(t)
    
    # espace après un point collé à une lettre : "bonjour.Merci" -> "bonjour. Merci"
    t <- gsub("(\\.)([[:alpha:]])", "\\1 \\2", t, perl = TRUE)
    
    # ajoute ". " après une URL (si pas déjà suivi d'espace/punct)
    t <- gsub("(https?://\\S+)(?![\\s\\.,;:!?])", "\\1. ", t, perl = TRUE)
    
    # majuscule au début + après ponctuation . ! ? … (SANS fonction de remplacement)
    t <- gsub("(^|[.!?…]\\s+)([[:alpha:]])", "\\1\\U\\2", t, perl = TRUE)
    
    out[i] <- t
  }
  
  out
}