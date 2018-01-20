context("Test color console lists")

test_that("basic print", {
  litems <- list(
    "Sets key using surgID, procDate, and argument cohortVars",
    "Executes funs ApplySurgFilter, ApplyCohortFilter, IndexCohortCases",
    "Funs above are applied successively on the dataset",
    "Summarize cohort experience profile by surgeon",
    "Utilize Surg-Cohort stat table to filter (surg, cohort) obs",
    "return list containing data and surg-cohort stat table"
  )

  printlines(liheader = "Data Processing Steps:",
             litems = litems,
             likeyword = "DATA",
             lileadcol = "white",
             libgfill = "blue",
             likwtxtcol = "black",
             likwbgfill = "yellow",
             symcol = "blue",
             sym = "*",
             is_ordered = FALSE)

  printstamp("message")

  t <- Sys.time()
  print_rt(t)

})
