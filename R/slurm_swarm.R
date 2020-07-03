# Unified script for composite slurm files
# glue::glue, but if there's null values it returns glue("")
# instead of # a 0-length glue
glue_null = function(...) {
  out = glue(...)
  if(length(out) == 0) out = glue("")
  out
}

# Slurm Swarm Architecture
make_slurm_swarm_directories = function(job_names = "job", root_dir=getwd()) {
  # job_names: name of different jobs, each of which gets a sub dir
  # root dir: where to put it
  path(root_dir, "swarm", c(job_names, "logs")) %>% fs::dir_create()
}

#' Factory for making a slurm_swarm
#' @param swarm_name name of the swarm.
#' @param updater_script name of the command script used to update task lists.
#'                       This script should take `section_list` as the first
#'                       argument and the SLURM job ID as the second.
#' @param remaining_jobs_script name of command script used to determine how many
#'                              jobs are remaining.
#' @param section_list vector of job types.
#' @param ppn_list processes per node for `section_list`.
#' @param queue_list available slurm partitions (queues)
#' @param update_arg_list character vector of a section's extra `glue`-able
#'                        args for the `updater_script`.
#' @param jobid environment variable for the slurm job ID
#' @param other_args named list of other args w/ default values for; this can
#'                   used to set default values for any existing arguments other
#'                   than section or queue. Any new args included here will only
#'                   be referenced in `sbatch_extra`, `updater_script`,
#'                   `remaining_jobs_script`, or `update_arg_list`.
#' @param sbatch_extra extra #SBATCH directives
#' @return a function that sets up a slurm swarm
#' @importFrom fs path
#' @export
make_slurm_swarm = function(swarm_name, updater_script,
                            remaining_jobs_script,
                            section_list, ppn_list,
                            queue_list = c("normal", "skx-normal"),
                            jobid = "$SLURM_JOBID",
                            other_args = list(),
                            sbatch_extra = "") {
  ### Code scaffolds ####
  # Launcher code for each section

  section_tbl = tibble(section_name = section_list,
                       PPN = ppn_list) %>%
    mutate(update_head = glue("{updater_script} {section_name} {jobid}")) %>%
    mutate(launcher_code = glue(
    "# {section_list}
     export SLURM_TASKS_PER_NODE={ppn_list}
     {update_head} {lb}{max_tasks} * {PPN}{rb} {update_arg_list}
     export LAUNCHER_JOB_FILE=swarm/{section_list}/{jobid}
     $LAUNCHER_DIR/paramrun
     ",
    # The lb/rb stuff is to make sure that max_tasks is multiplied by PPN correclty
    lb = "{", rb = "}", max_tasks = "max_tasks")) %>%
    dplyr::select(-update_head, -PPN)
  # while loop scaffold
  set_remaining_jobs = glue("remaining_jobs={remaining_jobs_script}")
  while_loop = # Note: this requires a remaining_jobs script
    "{set_remaining_jobs}
        while [$remaining_jobs -ge 0]
        do
          {launcher_scripts}
          {set_remaining_jobs}
        done"

  # Not sure if a control file is necessary, but if so, this is what it will be
  # control_file = glue("{swarm_name}.control")

  slurm_scaffold = glue(
    "#!/bin/bash
    #SBATCH -J {swarm_name}
    #SBATCH -N {{nodes}}
    #SBATCH -n {{nodes}}
    #SBATCH -p {{queue}}
    #SBATCH -o swarm/logs/{swarm_name}.%j.o
    #SBATCH -e slurm_logs/{swarm_name}.%j.e
    #SBATCH -t {{time}}
    {{sbatch_account}}
    {{sbatch_email}}
    {sbatch_extra}
    #------------------------------------------------------
    module load intel
    module load Rstats
    module load launcher
    export LAUNCHER_PLUGIN_DIR=$LAUNCHER_DIR/plugins
    export LAUNCHER_RMI=SLURM
    {{script_body}}")
  # Initial return function ####
  #' Create a slurm script that handles multiple pipeline steps
  #'
  #' Each step (section) builds a joblist then runs
  #' Joblists will exclude already complete outputs.
  #' @param nodes number of nodes
  #' @param time time requested
  #' @param queue which queue to use
  #' @param sections which job types to run
  #' @param account TACC account
  #' @param email email for contact
  #' @param root_dir directory to create swarm folder in
  base_function = function(nodes, time, queue,
                           sections, account, email,
                           root_dir = getwd()) {
    queue = match.arg(queue, queue_list)
    sections = match.arg(sections, job_sections, several.ok = TRUE)
    # Allow for a node range
    if(length(nodes) == 2) {
      # Figure out how to make -n work with this
      stop("Figure this out")
    }
    make_slurm_swarm_directories(sections, root_dir)
    # Optional components ####
    sbatch_account = glue_null("#SBATCH -A {account}")
    sbatch_email = glue_null(
      "#SBATCH --mail-user={email}
      #SBATCH --mail-type=fail"
    )
    # Assemble the slurm script #####
    script_body = glue(while_loop,
      launcher_scripts = section_tbl %>%
        dplyr::filter(section_name %in% sections) %>%
        pull(launcher_code) %>%
        glue_collapse(sep = "\n") %>%
        glue() # fill in extra args
    )
    slurm_script = glue(slurm_scaffold)
    # Save files ####
    slurm_file = slurm_path(root_dir, swarm, glue("{swarm_name}.slurm"))
    readr::write_lines(slurm_script, slurm_file)
    # If there's a control file, output it here
  }
  # Modify the base function's arguments ####
  new_fmls = rlang::fn_fmls(base_function) %>%
    purrr::list_modify(
      !!!other_args,
      queue = queue_list,
      sections = section_list)
  rlang::fn_fmls(base_function) <- new_fmls
  # see if this works
  rm(new_fmls)
  base_function
}
