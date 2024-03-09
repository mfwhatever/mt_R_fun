require(odbc)
require(tidyverse)

# ########################## #
# ### Connect to BOSEXAM ### #
# ########################## #

# Close any connection that is already open

if (exists("connect")) {
  if (odbc::dbIsValid(connect)) {
    DBI::dbDisconnect(connect)
  }
}

# Connect

connect <- dbConnect(odbc::odbc(), dsn = "BOSEXAM")

# ######################### #
# ### Declare Functions ### #
# ######################### #

# Function to remove whitespace from string fields in R dataframes
rmv_ws <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- trimws(df[[col]],
                          # \h is any horizontal whitespace
                          # \v is any vertical whitespace
                          # Unlike the default, these options will
                          # remove Unicode non-breaking space
                          whitespace = "[\\h\\v]")
    }
  }
  return(df)
}



# Function to fix duplicate dataframe column names
fix_dups <- function(df) {
  df <- setNames(df, make.unique(names(df)))
  return(df)
}

# ##################### #
# ### Example Query ### #
# ##################### #

query <- "SELECT * FROM PRDDTALIB.SCHOOL"

data <- DBI::dbGetQuery(connect, query) %>%
  rmv_ws() %>%
  fix_dups()

# ############################ #
# ### Close the Connection ### #
# ############################ #

DBI::dbDisconnect(connect)