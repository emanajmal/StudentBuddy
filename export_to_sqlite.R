
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("RPostgres", quietly = TRUE)) install.packages("RPostgres")
                                                                     
library(DBI)
library(RPostgres)
library(RSQLite)

pg_con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "StudentBuddy",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "Erdfcv12@"   # Replace with your real password
)

sqlite_con <- dbConnect(RSQLite::SQLite(), "StudentBuddy.db")

list.files()
dbWriteTable(sqlite_con, "students", dbReadTable(pg_con, "students"), overwrite = TRUE)
dbWriteTable(sqlite_con, "buddies", dbReadTable(pg_con, "buddies"), overwrite = TRUE)
dbWriteTable(sqlite_con, "matches", dbReadTable(pg_con, "matches"), overwrite = TRUE)

dbDisconnect(pg_con)
dbDisconnect(sqlite_con)


