check_cm_medication <- function(data) {

  # Variables required for this edit check
  required_vars <- c(
    "USUBJID",
    "CMSEQ",
    "CMYN",
    "CMMED"
  )

  # Check whether all required variables exist
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "Required variable(s) missing: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Standardize values for checking
  cmyn <- toupper(trimws(as.character(data$CMYN)))
  cmmed <- trimws(as.character(data$CMMED))

  # Identify records where CMYN = YES
  # but Medication is missing
  issue_index <- which(
    cmyn == "YES" &
      (is.na(data$CMMED) | cmmed == "")
  )

  # If no issues are found, return an empty issue dataset
  if (length(issue_index) == 0) {

    return(
      data.frame(
        CHECK_ID = character(),
        USUBJID = character(),
        CMSEQ = character(),
        VARIABLE = character(),
        VALUE = character(),
        ISSUE = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  # Create issue dataset
  issues <- data.frame(
    CHECK_ID = "CM001",

    USUBJID =
      as.character(data$USUBJID[issue_index]),

    CMSEQ =
      as.character(data$CMSEQ[issue_index]),

    VARIABLE = "CMMED",

    VALUE =
      as.character(data$CMMED[issue_index]),

    ISSUE =
      "Medication is required when CMYN = YES.",

    stringsAsFactors = FALSE
  )

  return(issues)
}
