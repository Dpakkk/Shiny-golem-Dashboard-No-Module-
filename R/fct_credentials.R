# 
# ## Credential for the login system
# credentials <- data.frame(
#   user = c("diwash", "bikash","princi", "BIndash_manager"), 
#   password = c("WWwhyRRr", "AtERtzOM","ARbiCADr","entAntaNcO"), 
#   admin = c(TRUE,TRUE,FALSE,TRUE),
#   stringsAsFactors = FALSE
# )
# 
# 
# shinymanager::create_db(
#   credentials_data = credentials,
#   sqlite_path = "credentials/credentials.sqlite"
# )