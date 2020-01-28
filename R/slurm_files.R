#' Create a SLURM configuration header
#' @param job_name the job's name
#' @param nodes number of nodes
#' @param wayness how many tasks per node to use
#' @param time max clock time in "HH:MM:SS" format
#' @param queue (current options are "normal" and "development")
#' @param account account to charge the jobs to
#' @param log_dir where to save the log files
#' @param email address to email
#' @param ... unused
#' @return the config header of a slurm job
#' @export
#' @importFrom glue glue glue_collapse
make_slurm_config <- function(job_name, nodes, wayness,
                              time, queue, account, log_dir,
                              email = "", ...) {
  # browser()
  glue(
    "#!/bin/bash
    #SBATCH -J {job_name}
    #SBATCH -N {nodes}
    #SBATCH -n {wayness * nodes}
    #SBATCH -p {queue}
    #SBATCH -o {log_dir}/{job_name}.o
    #SBATCH -e {log_dir}/{job_name}.e
    #SBATCH -t {time}
    #SBATCH -A {account}
    #SBATCH --mail-user={email}
    #SBATCH --mail-type=fail
    #------------------------------------------------------
    module load intel
    module load Rstats
  "
  )
}

#' Creates a SLURM file
#' @param slurm_config the configuration header, from `make_slurm_config()`
#' @param launcher_file the job file for launcher scripts; if empty the
#'    launcher isn't used
#' @param launcher_prefix,launcher_suffix code to run before and after the
#'    launcher code is run
#' @param ... unused
#' @return text of a slurm file
#' @export
make_slurm <- function(slurm_config, launcher_file = "",
                       launcher_prefix = "",
                       launcher_suffix = "", ...) {
  launcher <-
    ifelse(launcher_file == "", "", glue(
      "module load launcher
       export LAUNCHER_PLUGIN_DIR=$LAUNCHER_DIR/plugins
       export LAUNCHER_RMI=SLURM
       export LAUNCHER_JOB_FILE={launcher_file}
       $LAUNCHER_DIR/paramrun"
    ))
  glue("{slurm_config}
       {launcher_prefix}
       {launcher}
       {launcher_suffix}")
}

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
