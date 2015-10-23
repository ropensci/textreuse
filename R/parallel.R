# Check if the option `mc.cores` has been set. If it has, return `mclapply`
# instead of `lapply`. But in no circumstances use `mclapply` on Windows.
using_parallel <- function() {
  cores_set <- !is.null(getOption("mc.cores"))
  windows <- .Platform$OS.type == "windows"
  cores_set && !windows
}

get_apply_function <- function() {
 if (using_parallel())
   return(parallel::mclapply)
 else
   return(lapply)
}
