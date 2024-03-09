require(RODBC)
require(keyring)
require(rstudioapi)

# Close any connection that is already open
if (exists("connect")) {
  # note that odbcValidChannel() is an internal function
  # (which is why ::: is needed to access it).
  # There is no guarantee that this function will be accessible in
  # future versions of RODBC (current version 1.3-23)
  if (RODBC:::odbcValidChannel("channel")) {
    odbcClose("connect")
  }
}

# Check if a username/password is already stored.
# On Windows, by default, the value will be stored
# in the Windows Credential Manager
# (Control Panel\User Accounts\Credential Manager).

if (!"BOSEXAM" %in% keyring::key_list()[["service"]]) {
  keyring::key_set(
    "BOSEXAM",
    username = rstudioapi::showPrompt(title = "Username",
                                      message = "Enter your BOSEXAM username"),
    prompt = "Enter your BOSEXAM password"
  )
}


# Make the connection
connect <- odbcDriverConnect(
  connection = paste0(
    "DRIVER=iSeries Access ODBC Driver;",
    "SYSTEM=BOSEXAM;",
    "UID=",
    (
      keyring::key_list() %>%
        filter(service == "BOSEXAM") %>%
        select(username)
    )[[1]],
    ";PWD=",
    keyring::key_get(
      service = "BOSEXAM",
      username = (
        keyring::key_list() %>%
          filter(service == "BOSEXAM") %>%
          select(username)
      )[[1]]
    ),
    ";",
    #uncomment the next line if database tables contain
    #"FOR BIT DATA" data types. It translates binary
    #by default
    #"TRANSLATE=1;",
    collapse = ""
  )
)