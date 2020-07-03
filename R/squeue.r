
### Helper functions ####
# Ensures that missing columns at the end will still be included
# like with stringr::str_split()
strsplit_safe = function(x, split = "\t", ...) {
  strsplit(paste0(x, split), split, ...)
}
string_count = function(x, pattern, ...) {
# budget version of stringr::str_count()
  positions = gregexpr(pattern, x, ...)
  # -1 indicates nothing found
  purrr::map_int(positions,
     ~ifelse(.x[1] == -1L, 0L, length(.x)) )
}
# Convert SLURM output times to the diff times
to_difftime = function(x) {
  n_colon = string_count(x, ":")
  formats = c("%M:%S", "%H:%M:%S")
  as.difftime(x, formats[n_colon], units = "minutes")
}



### Main function ####
#' Return SLURM queue as a tibble
#' @param user username
#' @return a tibble of the SLURM queue
#' @export
squeue <- function(user = "$USER") {
  options = paste0("%", c( # -o options
    "i", # (Job ID)
    "P", # (partition; e.g., queue)
    "j", # (Job name)
    "u", # (User)
    "t", # (Job state)
    "M", # (Time used)
    "D", # (Nodes?)
    "R", # (Reason)
    "L", # time left
    "a", # (account)
    "E"  # (Job dependencies remaining)
    ), collapse = "\t")
  args = glue::glue("-u {user} -o='{options}'")
  unparsed = system2("squeue", args = args, stdout = TRUE)
  # Now let's parse it
  # Remove first equal sign and split the strings by tabs
  split = substring(unparsed, 2L) %>%
    strsplit_safe(split = "\t")
  # Create a tibble using tribble format
  headers = # give formula format
    glue::glue('~"{x}"', x = split[[1]]) %>%
    lapply(as.formula)
  remainder = unlist(split[-1])
  out_df = tibble::tribble(!!!headers, !!!remainder)
  # Format columns
  within(out_df, { # within is basically mutate()
    JOBID = as.integer(JOBID);
    NODES = as.integer(NODES);
    TIME = to_difftime(TIME);
    TIME_LEFT = to_difftime(TIME_LEFT)
  })
}
