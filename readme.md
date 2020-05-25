## Database Records

This modules implements a mapping between SRFI-9 or native guile records and SQL database.
Currently we support only guile-sqlite3 database module.

## Example

    (use-modules (srfi srfi-9) (database records))

    (define-record-type <user>
      (make-user username fullname email)
      user?
      (username   user-username)
      (fullname   user-fullname)
      (email      user-email))

    (define-database-record-mapping <user> #:primary-key 'username
      add-user update-user remove-user get-user list-users)

    (add-user "alice" "Alice Brown" "alice@example.com")
    (add-user (make-user "alice" "Alice Brown" "alice@example.com"))
    (add-user #:username "alice" #:fullname "Alice Brown" #:email "alice@example.com")
    ; => #<<user> username: "alice" fullname: "Alice Brown" email: "alice@example.com">

    (get-user "alice")
    (get-user (make-user "alice" "Alice Brown" "alice@example.com"))
    ; => #<<user> username: "alice" fullname: "Alice Brown" email: "alice@example.com">

    (update-user "alice" #:email "alice.brown@example.com")
    (update-user (make-user "alice" "Alice Brown" "alice@example.com") #:email "alice.brown@example.com")
    (get-user "alice")
    ; => #<<user> username: "alice" fullname: "Alice Brown" email: "alice.brown@example.com">

    (remove-user "alice")
    (remove-user (make-user "alice" "Alice Brown" "alice@example.com"))
    (get-user "alice")
    ; => #f

    (add-user "alice" "" "alice@example.com")
    (add-user "bob" "" "bob@example.com")
    (list-users #:fullname "")
    ; => (#<<user> username: "alice" fullname: "" email: "alice@example.com">
          #<<user> username: "bob" fullname: "" email: "bob@example.com">)

## Usage

- Macro
  **define-database-record-mapping *record-type* [#:primary-key *primary-key*]
    [*add-record*] [*update-record*] [*remove-record*] [*get-record*] [*list-records*] **

  Defines database mappings for the record type.
  Mapping procedures may be listed in any order, and any one can be skipped.
  The function is associated to the procedure by the procedure name,
  Any procedure starting with "add" would be adder, and so on.
  If you need a procedure with a different name assign it later, for example:

      (define-database-record-mapping <event> add-event)
      (define new-event add-event)

  - **add-record**

  Inserts a row into database. Returns the newly inserted record.

    - **add-record *record* **

      Specify the values with an instance of the record type.

    - **add-record *value1* *value2* ... **

      Specify the field values. Values for all fields must be specified.

    - **add-record *#:field* *value* ... **

      Specify the values as keyword-value pairs.
      Not all (but at least one) values may be specified.

  - **update-record *id* *#:field* *value* ... **

      Update a row with a given primary key.
      *id* is either the value of the primary key, or the record.
      When *id* is an instance of record type, only the value of primary key is considered,
      other fields are ignored.

  - **remove-records id**
    **remove-records [#:field *value* ...]**

     If one argument is specified, remove the row with this primary key.
     When *id* is an instance of record type, only the value of primary key is considered,
     other fields are ignored.

     If multiple arguments are specified, remove all rows that match all the specified values.
     In case no argument is specified, remove all rows from this table.

  - **get-record**

     Reads a single record from the database.
     In case no row matches the specification, *#f* is returned.
     In case more than one row matches, one of the rows is returned.

     - **get-record *value* **

       Gets the record with the primary key value.

     - **get-record *#:field* *value* ... ***

       Gets the record with all matching values.

     - **get-record record**

       Gets the record that has the same primary key as the given record.
       This may be used to reload a record from the database.

  - **list-records [*#:field* *value* ...] **

     Read a list of records from the database.
     Returns the records that match all the specified values,
     or all records if no values are specified.
     Returns empty list if not records match.

## Requirements

- guile-sqlite3

  Debian has 'guile-sqlite3' package.
  Web page: https://notabug.org/guile-sqlite3/guile-sqlite3

