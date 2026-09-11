check_cm_indication <- function(data) {

  required_vars <- c(
    "USUBJID",
    "CMSEQ",
    "CMIND",
    "CMINDSPE",
    "CMAEIND",
    "CMMHIND"
  )

  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "Required variable(s) missing: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Helper function:
  # treat NA, blank, N\A and N/A as missing
  is_missing_value <- function(x) {

    x <- toupper(trimws(as.character(x)))

    is.na(x) |
      x == "" |
      x == "N\\A" |
      x == "N/A"
  }

  cmind <- trimws(as.character(data$CMIND))

  # -------------------------------
  # CM002 - Adverse Event
  # -------------------------------

  idx_cm002 <- which(
    cmind == "Adverse Event" &
      is_missing_value(data$CMAEIND)
  )

  cm002 <- data.frame(
    CHECK_ID = rep("CM002", length(idx_cm002)),
    USUBJID = as.character(data$USUBJID[idx_cm002]),
    CMSEQ = as.character(data$CMSEQ[idx_cm002]),
    VARIABLE = rep("CMAEIND", length(idx_cm002)),
    VALUE = as.character(data$CMAEIND[idx_cm002]),
    ISSUE = rep(
      "Adverse Event Term is required when CMIND = Adverse Event.",
      length(idx_cm002)
    ),
    stringsAsFactors = FALSE
  )

  # -------------------------------
  # CM003 - Medical History
  # -------------------------------

  idx_cm003 <- which(
    cmind == "Medical History Condition(s)" &
      is_missing_value(data$CMMHIND)
  )

  cm003 <- data.frame(
    CHECK_ID = rep("CM003", length(idx_cm003)),
    USUBJID = as.character(data$USUBJID[idx_cm003]),
    CMSEQ = as.character(data$CMSEQ[idx_cm003]),
    VARIABLE = rep("CMMHIND", length(idx_cm003)),
    VALUE = as.character(data$CMMHIND[idx_cm003]),
    ISSUE = rep(
      "Medical History Term is required when CMIND = Medical History Condition(s).",
      length(idx_cm003)
    ),
    stringsAsFactors = FALSE
  )

  # -------------------------------
  # CM004 - Prophylaxis Specify
  # -------------------------------

  idx_cm004 <- which(
    cmind == "Prophylaxis (specify)" &
      is_missing_value(data$CMINDSPE)
  )

  cm004 <- data.frame(
    CHECK_ID = rep("CM004", length(idx_cm004)),
    USUBJID = as.character(data$USUBJID[idx_cm004]),
    CMSEQ = as.character(data$CMSEQ[idx_cm004]),
    VARIABLE = rep("CMINDSPE", length(idx_cm004)),
    VALUE = as.character(data$CMINDSPE[idx_cm004]),
    ISSUE = rep(
      "Indication specify is required when CMIND = Prophylaxis (specify).",
      length(idx_cm004)
    ),
    stringsAsFactors = FALSE
  )

  # Combine all indication issues
  issues <- rbind(
    cm002,
    cm003,
    cm004
  )

  return(issues)
}
