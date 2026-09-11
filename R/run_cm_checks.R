# Main function to run CM edit checks and generate the Excel issue report
run_cm_checks <- function(sas_file, output_file) {

  # Check SAS input path
  if (missing(sas_file) || sas_file == "") {
    stop("Please provide the SAS dataset location.")
  }

  # Check whether SAS file exists
  if (!file.exists(sas_file)) {
    stop("SAS dataset not found: ", sas_file)
  }

  # Check SAS file extension
  if (tolower(tools::file_ext(sas_file)) != "sas7bdat") {
    stop("Input file must be a .sas7bdat file.")
  }

  # Check output path
  if (missing(output_file) || output_file == "") {
    stop("Please provide the Excel output file location.")
  }

  # Check Excel extension
  if (tolower(tools::file_ext(output_file)) != "xlsx") {
    stop("Output file must be an .xlsx file.")
  }

  message("Reading SAS dataset...")

  # Read SAS dataset
  data <- haven::read_sas(sas_file)

  message("CM edit checks started.")
  message("Number of records received: ", nrow(data))
  message("Number of variables received: ", ncol(data))


  # ==========================================
  # Run CM001
  # ==========================================

  cm001_issues <- check_cm_medication(data)

  message(
    "CM001 issues found: ",
    nrow(cm001_issues)
  )


  # ==========================================
  # Run CM002, CM003 and CM004
  # ==========================================

  indication_issues <- check_cm_indication(data)

  message(
    "CM002 issues found: ",
    sum(indication_issues$CHECK_ID == "CM002")
  )

  message(
    "CM003 issues found: ",
    sum(indication_issues$CHECK_ID == "CM003")
  )

  message(
    "CM004 issues found: ",
    sum(indication_issues$CHECK_ID == "CM004")
  )


  # ==========================================
  # Combine all issues
  # ==========================================

  all_issues <- rbind(
    cm001_issues,
    indication_issues
  )

  total_issues <- nrow(all_issues)

  message(
    "Total issues found: ",
    total_issues
  )


  # ==========================================
  # Create summary sheet
  # ==========================================

  summary <- data.frame(
    DATASET = "CM",
    RECORDS_CHECKED = nrow(data),
    CHECKS_RUN = 4,
    TOTAL_ISSUES = total_issues,
    stringsAsFactors = FALSE
  )


  # ==========================================
  # Create check summary
  # ==========================================

  check_summary <- data.frame(
    CHECK_ID = c(
      "CM001",
      "CM002",
      "CM003",
      "CM004"
    ),

    DESCRIPTION = c(
      "Medication required when CMYN = YES",
      "AE term required for Adverse Event indication",
      "MH term required for Medical History indication",
      "Specify value required for Prophylaxis indication"
    ),

    ISSUES_FOUND = c(
      nrow(cm001_issues),
      sum(indication_issues$CHECK_ID == "CM002"),
      sum(indication_issues$CHECK_ID == "CM003"),
      sum(indication_issues$CHECK_ID == "CM004")
    ),

    stringsAsFactors = FALSE
  )


  # ==========================================
  # Create output directory if needed
  # ==========================================

  output_directory <- dirname(output_file)

  if (!dir.exists(output_directory)) {

    dir.create(
      output_directory,
      recursive = TRUE
    )

  }


  # ==========================================
  # Write Excel report
  # ==========================================

  writexl::write_xlsx(

    list(
      Summary = summary,
      Check_Summary = check_summary,
      Issues = all_issues
    ),

    path = output_file

  )


  message(
    "Excel report created successfully: ",
    output_file
  )

  return(all_issues)
}
