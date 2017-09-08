
context("test read_icpms() and tidy_icpms()")

# define the test icp directory
test_icp_dir <- system.file("test_icpms", package = "dalcwrs")

test_that("read_icpms() reads test_icpms.xlsx correctly", {
  # read file
  df <- read_icpms(file.path(test_icp_dir, "test_icpms.xlsx"))
  # check output columns
  expect_true(setequal(
    names(df),
    c("source", "sample_id", "datetime", "extra", "run", "4.5Bkg_ppb", "27Al_ppb", "45Sc_ppb",
      "56Fe_ppb", "65Cu_ppb", "115In_ppb", "159Tb_ppb", "208Pb_ppb",
      "220.5Bkg_ppb")
  ))
  # check number of samples (22)
  expect_length(unique(df$sample_id), 22)

  # check that tidy_icpms() doesn't change the number of parameters or number of samples
  tidy <- tidy_icpms(df)
  expect_true(setequal(
    with(tidy, paste0(isotope, element, "_", unit)),
    c("4.5Bkg_ppb", "27Al_ppb", "45Sc_ppb",
      "56Fe_ppb", "65Cu_ppb", "115In_ppb", "159Tb_ppb", "208Pb_ppb",
      "220.5Bkg_ppb")
  ))
  expect_true(setequal(
    df$sample_id,
    tidy$sample_id
  ))

  # check a few mean values calculated manually to ensure values didn't get mixed up
  expect_equal(
    tidy %>% dplyr::filter(sample_id == "DF1 June5", element == "Pb") %>% .$value,
    3.0736666667
  )
  expect_equal(
    tidy %>% dplyr::filter(sample_id == "Standard 1 - 25ug/L", element == "Fe") %>% .$value,
    22.8733333333
  )
})

test_that("using test_icpms with a directory works as expected", {
  test_icp_files <- list.files(test_icp_dir, full.names = TRUE)

  # read whole temp directory
  df_all <- read_icpms(test_icp_dir)

  # make sure all files are represented in the output
  expect_true(setequal(
    df_all$source,
    test_icp_files
  ))

  # make sure all files are included in the tidy output
  tidy_all <- tidy_icpms(df_all)
  expect_true(setequal(
    tidy_all$source,
    test_icp_files
  ))
})
