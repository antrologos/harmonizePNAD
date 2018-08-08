#' Builds a synthetic variable for age - 1960
#' @param
#' @value data.frame
#' @export

vars_to_harmonize <- function(){

        existing_functions <- unclass(lsf.str(envir = asNamespace("harmonizePNAD"), all = T))
        existing_functions <- existing_functions[grep(pattern = "build", x = existing_functions)]

        f_parts <- strsplit(existing_functions, split = "_")
        f_parts <- f_parts[sapply(f_parts, function(x) length(x) == 3)]

        vars_to_harmonize <-  data.frame(t(data.frame(f_parts)))[, -1]
        rownames(vars_to_harmonize) <- NULL

        names(vars_to_harmonize) <- c("Theme", "Variable")

        vars_to_harmonize
}
