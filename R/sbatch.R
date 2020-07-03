
# get the job ID from sbatch output
get_jobID <- function(sbatch_output) {
  last_line <- last(sbatch_output)
  last(unlist(strsplit(last_line, " ", fixed = TRUE))) # assumes the jobid is the last line
}

#' submit a slurm file to the queue, with dependencies
#' @param slurm_file name of the file to submit
#' @param dependencies jobid of dependencies if any
#' @param dep_type which type of dependency to use (see SLURM documentation)
#' @param extra additinoal SLURM arguments
#' @return the jobid of the submitted job
#' @export
#' @importFrom purrr map_chr
sbatch <- function(slurm_file, dep = NA, dep_type = c("ok", "notok", "any", ""),
                   .extra = "") {
  dep_type <- match.arg(dep_type) # correspond to after{dep_type}
  # dep is a jobid dependency; if NA, there's no dependency
  extras <- paste0(.extra, collapse = " ") # extra slurm arguments
  if (any(is.na(dep))) {
    arg <- slurm_file
    dep_str <- ""
  } else {
    dep <- glue_collapse(glue("after{dep_type}:{dep}"), sep = ",")
    dep_str <- glue("--dependency={dep}")
  }
  arg <- paste(dep_str, extras, slurm_file)
  map_chr(arg, function(.arg) {
    out <- system2("sbatch", .arg, stdout = TRUE)
    cat(out, sep = "\n")
    get_jobID(out)
  })
}

#' submit a sequence of slurm files, each depending on the previous
#' @param slurm_files a vector of slurm files to be submitted in order
#' @param ... arguments for `sbatch`
#' @return a vector of jobids
#' @export
#' @importFrom purrr accumulate
#' @rdname sbatch
sbatch_seq <- function(slurm_files, ...) {
  purrr::accumulate(slurm_files, ~ sbatch(.y, dep = .x, ...), ..., .init = NA)
}
