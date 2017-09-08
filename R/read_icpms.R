
#' Read ICP-MS Data Files
#'
#' @param path The path where the files are kept
#' @param df The output of \code{read_icpms} or a filtered result
#' @param skip The number of lines to skip at the beginning of the file
#'   (blank lines are automatically dropped).
#' @param na Values that should be considered NA
#'
#' @return \code{read_icpms} returns a
#'   data frame with columns sample_id, datetime, run, and others in
#'   the form "27Al_ppb" (isotope, element, unit). \code{tidy_icpms} returns
#'   a parameter-long, summarised result.
#' @export
#'
#' @importFrom dplyr select filter mutate mutate_at vars starts_with everything
#' @importFrom dplyr summarise group_by ungroup if_else one_of matches
#' @importFrom magrittr %>%
#'
#' @examples
#' # file
#' read_icpms(system.file("test_icpms/test_icpms.xlsx", package = "dalcwrs"))
#' # directory
#' read_icpms(system.file("test_icpms", package = "dalcwrs"))
#'
#' # use tidy to get parameter-long, summarised output
#' raw <- read_icpms(system.file("test_icpms/test_icpms.xlsx", package = "dalcwrs"))
#' tidy_icpms(raw)
#'
read_icpms <- function(path, skip = 1, na = c("NA", "", "n/a")) {
  # if is a directory, read all excel files in the directory and rbind
  if(dir.exists(path)) {
    # list files in directory
    files <- list.files(path, pattern = '*.xlsx', full.names = TRUE)

    # return result of read_icpms_single rbinded together
    return(purrr::map_df(files, read_icpms, skip = skip, na = na))
  }

  # read in file with all columns as character
  data_file <- readxl::read_excel(path, col_types = "text", na = na, skip = skip) %>%
    select(-starts_with("X_"))

  # check headers to make sure all required headers are present
  required_cols <- c("Run", "Time")
  missing_cols <- required_cols[!(required_cols %in% names(data_file))]
  if(length(missing_cols) > 0) stop(path, " is missing required columns: ",
                                    paste(missing_cols, collapse = ", "))

  # Change column names to element plus unit
  data_file_names <- names(data_file)
  data_file_names[c(-1, -2)] <- paste(data_file_names[c(-1, -2)],
                                      data_file[1, c(-1, -2)],
                                      sep = "_")
  names(data_file) <- data_file_names
  data_columns <- data_file_names[c(-1, -2)]

  # sample id regular expression
  sample_id_re <- paste0("^(.*?)\\s+", # sample id
                         "([0-9]{1,2}/[0-9]{1,2}/[0-9]{4}\\s+", # date
                         "[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}\\s[AP]M)", # time
                         "\\s*(.*?)$")

  # make sample ID column
  # make things from the sample_id column that aren't sample ids NA
  data_file$sample_id <- data_file[[3]] %>%
    if_else(stringr::str_detect(., sample_id_re), ., NA_character_)

  # final format of data_file
  data_file %>%
    # fill sample ID down to apply to all replicates
    tidyr::fill(sample_id, .direction = "down") %>%
    # filter where run is not NA
    filter(!is.na(Run)) %>%
    # extract sample id, datetime from sample ID column
    tidyr::extract(sample_id, into = c("sample_id", "datetime", "extra"),
                   sample_id_re) %>%
    mutate(extra = if_else(extra == "", NA_character_, extra)) %>%
    # parse date_run
    mutate(datetime = lubridate::mdy_hms(datetime)) %>%
    # add source file as a column
    mutate(source = path) %>%
    # select final order
    select(source, sample_id, datetime, extra, run = Run, everything()) %>%
    select(-Time) %>%
    # make value columns numeric
    mutate_at(vars(one_of(data_columns)), as.numeric)
}

#' @rdname read_icpms
#' @export
tidy_icpms <- function(df) {
  # check input
  if(!is.data.frame(df)) stop("df is not a data frame")

  # check required columns
  required_cols <- c("sample_id", "datetime", "extra", "run")
  missing_cols <- required_cols[!(required_cols %in% names(df))]
  if(length(missing_cols) > 0) stop("df is missing required columns: ",
                                    paste(missing_cols, collapse = ", "))

  # define data column re
  data_column_re <- "^([0-9.]+)([A-Za-z]+)_(.*?)$"

  # gather, summarise data columns
  df %>%
    # filter only raw data, not summaries
    filter(stringr::str_detect(run, "^[0-9]+$")) %>%
    select(-run) %>%
    # gather data values
    tidyr::gather(matches(data_column_re),
                  key = "param", value = "value") %>%
    # summarise data values to value, sd, n
    group_by(source, sample_id, datetime, extra, param) %>%
    summarise(sd = stats::sd(value, na.rm = TRUE),
              n = n(),
              n_na = sum(is.na(value)),
              value = mean(value, na.rm = TRUE),
              rsd = sd / value * 100) %>%
    ungroup() %>%
    # extract element, isotope, unit from param
    tidyr::extract(param, into = c("isotope", "element", "unit"),
                   data_column_re) %>%
    mutate(isotope = as.numeric(isotope)) %>%
    # final column selection
    select(source, sample_id, datetime, extra, isotope,
           element, value, sd, unit, n, n_na, rsd)
}
