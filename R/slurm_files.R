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
   module load Rstats")
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
