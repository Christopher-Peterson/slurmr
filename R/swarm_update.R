#' Swarm updater script
#' @param swarm_name name of the swarm script
#' @param section_list vector of section names
#' @param script_list vector of scripts for each name
#' @param filter_function_list list of functions or lambdas that filter out completed
#'                             tasks; arguments should be .x for input files and
#'                             .y for output files. return value should be the
#'                             first arguments for the task's script
#' @param script_arg_list extra arguments for the scripts
#' @param in_dir_list,out_dir_list directories to scan for input and output files
#' @param in_pat_list,out_pat_list regex pattern to filter input/output file names by
#' @importFrom rlang set_names
#' @export
#' @return a list with the updater function and remaining function needed by the swarm
make_swarm_task_helpers = function(swarm_name, section_list, script_list,
                              filter_function_list,
                              script_arg_list,
                              in_dir_list, in_pat_list,
                              out_dir_list, out_pat_list
) {
  # Create a named list of functions to get uncompleted outputs ####
    # Convert lambdas
  filter_function_list = lapply(script_list, filter_function_list)
  get_uncomplete = tibble(in_dir = in_dir_list,
                          out_dir = out_dir_list,
                          in_pat = in_pat_list,
                          out_pat = out_pat_list,
                          filter_fn = filter_function_list) %>%
    purrr::pmap(function(in_dir,  out_dir,  in_pat,
                         out_pat,  filter_fn) {
      function() {
        in_lst = dir(in_dir, pattern = in_pat)
        out_lst = dir(out_dir, pattern = out_pat)
        filter_fn(in_lst, out_lst)
      } # return value is an argument-less function
    }) %>% set_names(section_list)
  script = glue("{script_list} {{tasks}} {script_arg_list}") %>%
    as.list() %>% set_names(section_list)
  # Output
  list(
    update_function = function(section, job_id, max_tasks, ...) {

      out_job = path("swarm", section, job_id)
      # Check currently active tasks
      browser() # make sure that JOBNAME & STATUS are correct names
      active_queue = slurmr::squeue() %>%
        dplyr::filter(grepl(swarm_name, JOBNAME),
                      STATUS == "R") %>%
        dplyr::arrange(TIME) %>%
        dplyr::mutate(position == n(),
                      tasks = max_tasks * NODES) %>%
        dplyr::mutate(start_tasks = cumsum(tasks))
      n_active = nrow(active_queue)
      this_status = active_queue %>% filter(JOBID == job_id)
      other_jobs = active_queue %>% filter(JOBID != job_id)

      # Get list of uncompleted tasks
      uncomplete = get_uncomplete[[section]]()
      potential_scripts = glue(script, tasks = uncomplete, ...)
      claimed_by_job =
        glue("swarm/{section}/{other_jobs$JOBID}") %>%
          lapply(readr::read_lines)
      all_claimed = unlist(claimed_by_job)
      # Determine the output of the list
      available_scripts = potential_scripts[!potential_scripts %in% all_claimed]
      if(length(available_scripts) >= this_status$tasks) {
        # There are still plenty of tasks left; keep the top remaining
        out = available_scripts[1:this_status$tasks]
      } else {
        n_extra = this_status$tasks - length(available_scripts)
        n_per = ceiling(n_extra / (n_active - 1))
        # To select remaining files, pick the bottom {n_per} from
        # each list, then reverse their order
        remaining = claimed_by_job %>%
          purrr::map(~rev(tail(.x, n = n_per))) %>%
          purrr::transpose() %>% unlist()
        out = c(available_scripts, remaining)
      }
      readr::write_lines(out, out_job)
    },
    remaining_function = function(sections) {
      remaining = purrr::map_int(
        get_uncomplete[sections],
        ~length(rlang::exec(.x))
      )
      cat(sum(remaining))
      invisible(sum(remaining))
  })
}