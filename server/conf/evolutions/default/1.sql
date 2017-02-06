# Users schema

# --- !Ups
create table "Users" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "first_name" VARCHAR NOT NULL,
  "last_name" VARCHAR NOT NULL,
  "user_name" VARCHAR NOT NULL UNIQUE,
  "email" VARCHAR NOT NULL,
  "password" VARCHAR NOT NULL,
  "score" INTEGER NOT NULL
);

Insert into "Users" VALUES (0, "Sabrina", "Friedl", "Sabrina", "sabrina_friedl@gmx.de", "test", 2000);

# --- !Downs
