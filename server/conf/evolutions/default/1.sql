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
Insert into "Users" VALUES (1, "Admin", "Istrator", "admin", "admin@istra.tor", "admin", 2000);
Insert into "Users" VALUES (2, "Test", "Test", "test", "test@test.you", "test", 2000);

# --- !Downs
