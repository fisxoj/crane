# Migrations

Your schema will change, and this is a fact. Most ORMs hope the users
will be happy running manual `ALTER TABLEs` or provide migration functionality
through an external plugin (Alembic for SQLAlchemy, South for the Django ORM).

Migrations are completely built into Crane, and are designed to be intrusive:
You redefine the schema, reload, and Crane takes care of everything. If your
migration plan is too complicated for Crane, then you write a simple function
that does some transformations and Crane puts that in its migration history,
all that without ever having to leave your Lisp environment or accessing the
shell.

## Example

```lisp
(deftable employees
  (name :type string :null nil)
  (age  :type integer)
  (address :type string :null nil))
```

Now, if you decide that addresses can be nullable, you just redefine
the class (Make the change, and either `C-c C-c` on Emacs or Quickload
your project):

```lisp
(deftable employees
  (name :type string :null nil)
  (age  :type integer)
  (address :type string))
```

And Crane will spot the difference and perform the migration automatically.

## Trivial Migrations

Things like adding and dropping contraints (Making a field `NOT NULL`able,
dropping the default value of a column, et cetera) will be handled automatically
by Crane.

A less-than-trivial migration is changing the type of a column: In simple cases,
like moving from a float to an integer, Crane will handle this change automatically.

## Manual Migrations

More complex cases of changing a column's type can't be handled automatically
because of the ambiguity in what the user may be trying to achieve. For example,
changing a column's type from a string to an integer could involve a simple
`PARSE-STRING`, but maybe the user wants to do something more complex. When
Crane can't handle a migration automatically, a migration plan has to be written.

Note: A migration is a set of reversible transformations that take place at a
particular point in the database's history. A migration plan is a blueprint for
a migration, and is not tied to the migration history. Migration plans can be
applied repeatedly to create many migrations.