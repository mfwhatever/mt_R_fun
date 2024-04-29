require(RODBC)
require(keyring)
require(rstudioapi)


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