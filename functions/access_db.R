
#Exportando base para Dashboard

source("export_DB.R", encoding = "UTF-8")

#Base de dados de acessos

users_db <- data.frame(
  user = c("anisio","julio","demo"),
  password = c("123","321","123"),
  admin = c("True","True","True")
)



db_path <- "users.sqlite"

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Create a table in the database
dbWriteTable(con, "credentials", users_db, overwrite = TRUE)

# Disconnect from the database
dbDisconnect(con)

save(users_db, file = paste0("data/DB_Dashboard/users_db.rda"))
