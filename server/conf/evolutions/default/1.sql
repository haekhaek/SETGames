# User schema

# --- !Ups
create table "User" (
  `id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `first_name` VARCHAR NOT NULL,
  `last_name` VARCHAR NOT NULL,
  `email` VARCHAR NOT NULL
)
