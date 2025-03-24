# Increase the JAVA memory stack size to 8MB
options(java.parameters = "-Xmx8000m")

require(DBI)
require(odbc)
require(RJDBC)
require(tidyverse)
require(keyring)
require(rstudioapi)

# Set password function

set_BOSEXAM_password <- function(username){
  if(missing(username)) {
    user <- rstudioapi::showPrompt(
      title = "Change stored password",
      message = "Enter your BOSEXAM username"
    )
  } else {
    user <- deparse(substitute(username))
  }
  
  keyring::key_set(
    "BOSEXAM",
    username = user,
    prompt = paste0(
      "Enter your BOSEXAM password for ",
      user 
    )
  )
}

connect_bosexam <- function(
    username,
    connection_name,
    database = BOSEXAM,
    connection_method = "odbc" #"odbc" or "java"
) {
  if(!connection_method %in% c("odbc", "java")) {
    stop("connection_method must be either 'odbc' or 'java'")
  }
  
  if(missing(username)) {
    if(length(keyring::key_list(service = "BOSEXAM")$username) == 1) {
      user <- keyring::key_list(service = "BOSEXAM")$username
    } else {
      user <- rstudioapi::showPrompt(
        title = "Username",
        message = "Enter your BOSEXAM username"
      )
    }
  } else {
    user <- as.character(substitute(username))
  }
  
  if(missing(connection_name)) {
    connection <- "connect"
  } else {
    connection <- as.character(substitute(connection_name))
  }
  
  database <- deparse(substitute(database))
  
  connect_to_db <- function(
    db = database,
    uid = user) {
    
    e <- simpleError(
      paste0(
        "Connection Failed\n",
        "db = ", db, "\n",
        "uid = ", uid, "\n"
      )
    )
    
    # Check for password in keyring
    tryCatch(
      eval(
        parse(
          text = paste0(
            "keyring::key_get(",
            "service = \"",db,"\",",
            "username = \"",uid,"\")"
          )
        )
      ),
      error = function(e) {
        eval(
          parse(
            text = paste0(
              "set_BOSEXAM_password(",
              uid,
              ")"
            )
          )
        )
      }
    )
    
    # Try connection
    tryCatch(
      if(connection_method == "odbc"){
        dbConnect(
          odbc::odbc(), 
          #dsn = db,
          driver = "iSeries Access ODBC Driver",
          server = db,
          uid = uid,
          pwd =  eval(
            parse(
              text = paste0(
                "keyring::key_get(",
                "service = \"",db,"\",",
                "username = \"",uid,"\")"
              )
            )
          ),
          # Converts "for BIT DATA" types
          .connection_string = "SYSTEM=BOSEXAM;TRANSLATE=1"
        )} else if(connection_method == "java"){
          drv <- JDBC(
            "com.ibm.as400.access.AS400JDBCDriver",
            paste0(
              file.path(Sys.getenv('USERPROFILE')),
              "\\JTOpen-20.0.7\\jt400-20.0.7.jar",
              collapse = "")
          )
          dbConnect(
            drv,
            "jdbc:as400://BOSEXAM",
            user = uid,
            pwd =  eval(
              parse(
                text = paste0(
                  "keyring::key_get(",
                  "service = \"",db,"\",",
                  "username = \"",uid,"\")"
                )
              )
            )
          )
        },
      error = function(e) {
        stop("Connection Failed.\nTo change/set your keyring password, use set_BOSEXAM_password()")
        # eval(
        #   parse(
        #     text = paste0(
        #       "set_BOSEXAM_password(",
        #       uid,
        #       ")"
        #     )
        #   )
        # )
        # if(connection_method == "odbc"){
        #   dbConnect(
        #     odbc::odbc(), 
        #     #dsn = db,
        #     driver = "iSeries Access ODBC Driver",
        #     server = db,
        #     uid = uid,
        #     pwd =  eval(
        #       parse(
        #         text = paste0(
        #           "keyring::key_get(",
        #           "service = \"",db,"\",",
        #           "username = \"",uid,"\")"
        #         )
        #       )
        #     ),
        #     # Converts "for BIT DATA" types
        #     .connection_string = "SYSTEM=BOSEXAM;TRANSLATE=1"
        #   )} else if(connection_method == "java"){
        #     drv <- JDBC(
        #       "com.ibm.as400.access.AS400JDBCDriver",
        #       paste0(
        #         file.path(Sys.getenv('USERPROFILE')),
        #         "\\JTOpen-20.0.7\\jt400-20.0.7.jar",
        #         collapse = "")
        #     )
        #     dbConnect(
        #       drv,
        #       "jdbc:as400://BOSEXAM",
        #       user = uid,
        #       pwd =  eval(
        #         parse(
        #           text = paste0(
        #             "keyring::key_get(",
        #             "service = \"",db,"\",",
        #             "username = \"",uid,"\")"
        #           )
        #         )
        #       )
        #     )
        #   }
      }
    )
  }
  
  assign(
    connection,
    connect_to_db(),
    envir = .GlobalEnv
  )
  
  cat("Connection name: '", connection, "'\n", sep = "")
  print(get(connection))
  
}


# ############### #
# ### Connect ### #
# ############### #

#connect_bosexam()

# ######################### #
# ### Declare Functions ### #
# ######################### #

# Function to remove whitespace from string fields in R dataframes
# rmv_ws <- function(df) {
#   for (col in names(df)) {
#     if (is.character(df[[col]])) {
#       df[[col]] <- trimws(
#         df[[col]],
#         # \h is any horizontal whitespace
#         # \v is any vertical whitespace
#         # Unlike the default, these options will
#         # remove Unicode non-breaking space
#         whitespace = "[\\h\\v]"
#       )
#     }
#   }
#   return(df)
# }



# Function to fix duplicate dataframe column names
# fix_dups <- function(df) {
#   df <- setNames(df, make.unique(names(df)))
#   return(df)
# }

# ##################### #
# ### Example Query ### #
# ##################### #

#query <- "SELECT * FROM PRDDTALIB.SCHOOL"

#data <- DBI::dbGetQuery(connect, query) %>%
#  rmv_ws() %>%
#  fix_dups()

