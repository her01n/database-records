(define-module (test database-records))

(use-modules (srfi srfi-1) (srfi srfi-9))

(use-modules (hdt hdt))

(use-modules (database records))

(use-modules (sqlite3))

(define-record-type <user>
  (make-user username fullname email)
  user?
  (username   user-username)
  (fullname   user-fullname)
  (email      user-email))

(define-database-record-mapping <user> #:primary-key 'username
  add-user update-user remove-user get-user list-users)

(test
  ; All tests start a with a fresh database
  (system "rm -rf test.sqlite3")
  (set-database (sqlite-open "test.sqlite3" (logior SQLITE_OPEN_CREATE SQLITE_OPEN_READWRITE)))
  (define alice (make-user "alice" "Alice Brown" "alice@example.com"))
  (test add-user
    (assert (equal? #f (get-user "alice")))
    (test keys
      (assert 
        (equal? 
          alice 
          (add-user #:username "alice" #:fullname "Alice Brown" #:email "alice@example.com")))
      (assert (equal? alice (get-user "alice"))))
    (test values
      (assert (equal? alice (add-user "alice" "Alice Brown" "alice@example.com")))
      (assert (equal? alice (get-user "alice"))))
    (test record
      (assert (equal? alice (add-user alice)))
      (assert (equal? alice (get-user "alice"))))
    (test primary-key
      (add-user "alice" "Alice Brown" "alice@example.com")
      (assert (throws-exception (add-user "alice" "Alice Bianco" "alice@gmail.com")))))
  (test get-user
    (assert (equal? #f (get-user "alice")))
    (assert (equal? #f (get-user #:email "alice@example.com")))
    (assert (equal? #f (get-user #:email "alice@example.com" #:fullname "Alice Brown")))
    (assert (equal? #f (get-user alice)))
    (add-user "alice" "Alice Brown" "alice@example.com")
    (assert (equal? alice (get-user "alice")))
    (assert (equal? alice (get-user #:email "alice@example.com")))
    (assert (equal? alice (get-user #:email "alice@example.com" #:fullname "Alice Brown")))
    (assert (equal? #f (get-user #:email "alice@example.com" #:fullname "Bob Marley")))
    (assert (equal? alice (get-user alice))))
  (test add-get-null
    (add-user #:username "bob")
    (assert (equal? (make-user "bob" #f #f) (get-user #:fullname #f))))
  (test update-user
    (add-user "alice" "Alice Brown" "alice@example.com")
    (test id-keys
      (update-user "alice" #:email "alice@gmail.com")
      (assert (equal? "alice@gmail.com" (user-email (get-user "alice")))))
    (test record-keys
      (update-user alice #:email "alice@gmail.com")
      (assert (equal? "alice@gmail.com" (user-email (get-user "alice")))))
    (test id-record
      (update-user "alice" (make-user "alice" "Alice Brown" "alice@gmail.com"))
      (assert (equal? "alice@gmail.com" (user-email (get-user "alice")))))
    (test record-record
      (update-user alice (make-user "alice" "Alice Brown" "alice@gmail.com"))
      (assert (equal? "alice@gmail.com" (user-email (get-user "alice")))))
    (test null
      (update-user "alice" #:fullname #f)
      (assert (equal? (make-user "alice" #f "alice@example.com") (get-user "alice")))))
  (test remove-user
    (add-user alice)
    (test remove-id
      (remove-user "alice")
      (assert (equal? #f (get-user "alice"))))
    (test remove-record
      (remove-user alice)
      (assert (equal? #f (get-user "alice"))))
    (test remove-by-values
      (add-user "bob" "" "bob@asdf.com")
      (add-user "charlie" "" "charlie@cfactory.com")
      (remove-user #:fullname "")
      (assert (get-user "alice"))
      (assert (not (get-user "bob")))
      (assert (not (get-user "charlie")))))
  (test list-users
    (test list-users
      (define alice (add-user "alice" "Alice Brown" "alice@example.com"))
      (define bob (add-user "bob" "" "bob@example.com"))
      (define charlie (add-user "charlie" "" "charlie@example.com"))
      (assert (equal? (list) (list-users #:email "denis@example.com")))
      (assert (equal? (list bob) (list-users #:email "bob@example.com")))
      (assert (lset= equal? (list bob charlie) (list-users #:fullname "")))
      (assert (lset= equal? (list alice bob charlie) (list-users))))
    (test null
      (define bob (add-user "bob" #f #f))
      (define charlie (add-user "charlie" #f #f))
      (assert (lset= equal? (list bob charlie) (list-users #:fullname #f))))))

; test the mapping without a primary key

(define-record-type <user-role>
  (make-user-role user role)
  user-role?
  (user   user-role-user)
  (role   user-role-role))

; also test out of order procedures
(define-database-record-mapping <user-role>
  list-user-role add-user-role update-user-role remove-user-role get-user-role)

(test out-of-order
  (add-user-role "alice" "moderator")
  (assert (equal? (make-user-role "alice" "moderator") (get-user-role #:user "alice"))))

(test no-primary-key
  (assert (throws-exception (get-user-role "alice")))
  (assert (throws-exception (update-user-role "alice" #:role "editor"))))

