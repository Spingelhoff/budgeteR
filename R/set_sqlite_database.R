library(DBI)
library(RSQLite)


set_SQLite_database <- function() {
  db <- dbConnect(SQLite(), "database.sqlite")

  on.exit(dbDisconnect(db))

  revenue_table_creation_query <- "
  CREATE TABLE IF NOT EXISTS revenue (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    date TEXT NOT NULL,
    classification TEXT NOT NULL,
    total REAL NOT NULL
  ) STRICT
  "

  cost_table_creation_query <- "
  CREATE TABLE IF NOT EXISTS cost (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    date TEXT NOT NULL,
    classification TEXT NOT NULL,
    total REAL NOT NULL
  ) STRICT
  "

  goal_table_creation_query <- "
  CREATE TABLE IF NOT EXISTS goal (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    date TEXT NOT NULL,
    classification TEXT NOT NULL,
    total REAL NOT NULL
  ) STRICT
  "

  debt_table_creation_query <- "
  CREATE TABLE IF NOT EXISTS debt (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    date TEXT NOT NULL,
    classification TEXT NOT NULL,
    total REAL NOT NULL
  ) STRICT
  "

  rent_table_creation_query <- "
  CREATE TABLE IF NOT EXISTS rent (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    month INTEGER NOT NULL,
    year INTEGER NOT NULL,
    room TEXT NOT NULL,
    expected REAL NOT NULL,
    recieved REAL NOT NULL
  ) STRICT
  "

  rent_defaults_table_creation_query <- "
  CREATE TABLE IF NOT EXISTS rent_defaults (
    room TEXT NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    expected REAL NOT NULL
  ) STRICT
  "

  transaction_views_creation_query <- "
  CREATE VIEW IF NOT EXISTS transactions
  AS
  SELECT
    date,
    'revenue' AS type,
    classification,
    total
  FROM revenue
  UNION ALL
  SELECT
    date,
    'cost' AS type,
    classification,
    -(total) AS total
  FROM cost
  "

  budget_views_creation_query <- "
  CREATE VIEW IF NOT EXISTS budget
  AS
  SELECT
    date,
    'goal' AS type,
    classification,
    total
  FROM goal
  UNION ALL
  SELECT
    date,
    'debt' AS type,
    classification,
    -(total) AS total
  FROM debt
  "

  database_tables <- list(
    revenue_table_creation_query,
    cost_table_creation_query,
    goal_table_creation_query,
    debt_table_creation_query,
    rent_table_creation_query,
    rent_defaults_table_creation_query,
    transaction_views_creation_query,
    budget_views_creation_query
  )

  lapply(
    database_tables,
    function(x) dbExecute(db, x)
  )
}
