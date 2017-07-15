## copied from ggplot2
aesthetics <- function (x) {
   req_aes <- x$required_aes
   def_aes <- names(x$default_aes)
   def_aes <- setdiff(def_aes, req_aes)
   if (length(req_aes) == 0) {
      return(suppressWarnings(sort(names(x$default_aes))))
   }
   if (length(def_aes) == 0) {
      return(paste("\\strong{", sort(x$required_aes), "}", 
                   sep = ""))
   }
   return(c(paste("\\strong{", sort(x$required_aes), "}", sep = ""), 
            sort(def_aes)))
}

rd_aesthetics <- function(cname, fname) {
   aes <- aesthetics(get(cname))
   paste("\\code{", fname, "} ", "understands the following aesthetics (required aesthetics are in bold):\n\n",
         "\\itemize{\n", paste("  \\item \\code{", aes, "}", collapse = "\n",
                               sep = ""), "\n}\n", sep = "")
}
