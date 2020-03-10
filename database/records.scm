(define-module (database records)
  #:export-syntax (define-database-record-mapping))

(use-modules 
  (ice-9 match)
  (srfi srfi-9))

(use-modules (sqlite3))

(define database 
  (sqlite-open "database.sqlite3" (logior SQLITE_OPEN_CREATE SQLITE_OPEN_READWRITE)))

(define-public (set-database database')
  ; TODO close current database?
  (set! database database'))

(define (symbol->sql-string symbol)
  (list->string
    (filter identity
      (map
        (lambda (char) 
          (cond 
            ((char-alphabetic? char) char) 
            ((char-numeric? char) char)
            ((equal? #\_ char) #\_)
            ((equal? #\- char) #\_)
            (else #f)))
        (string->list (symbol->string symbol))))))

(define-record-type <mapping>
  (make-mapping record-type primary-key)
  mapping?
  (record-type   mapping-record-type)
  (primary-key   mapping-primary-key))

(define (mapping-table mapping)
  (symbol->sql-string (record-type-name (mapping-record-type mapping))))

(define (mapping-columns mapping)
  (map symbol->sql-string (record-type-fields (mapping-record-type mapping))))

(define (create-table mapping)
  (sqlite-exec database
    (format #f 
      "CREATE TABLE IF NOT EXISTS ~a (~a);" 
      (mapping-table mapping) 
      (string-join (mapping-columns mapping) ", "))))

(define* (keys->alist keys #:optional (alist '()))
  (match keys
    (() alist)
    ((key value . tail)
     (keys->alist tail (cons (cons (keyword->symbol key) value) alist)))))

(define (record->alist record-type value)
  (map
    (lambda (key) (cons key ((record-accessor record-type key) value)))
    (record-type-fields record-type)))

(define (keys->values mapping keys)
  (define alist (keys->alist keys))
  (map
    (lambda (key) (assoc-ref alist key)) 
    (record-type-fields (mapping-record-type mapping))))

(define (record->values mapping record)
  (map
    (lambda (key) ((record-accessor (mapping-record-type mapping) key) record))
    (record-type-fields (mapping-record-type mapping))))

(define (add-values mapping . values)
  (define record-type (mapping-record-type mapping))
  (define insert-sql
    (format #f
      "INSERT INTO ~a (~a) VALUES (~a)"
      (mapping-table mapping)
      (string-join (mapping-columns mapping) ", ")
      (string-join (map (const "?") (mapping-columns mapping)) ", ")))
  (create-table mapping)
  (let ((insert (sqlite-prepare database insert-sql)))
    (map
      (lambda (n value) (sqlite-bind insert (+ 1 n) value))
      (iota (length values)) values)
    (sqlite-step insert)
    (sqlite-finalize insert))
  (apply (record-constructor record-type) values))

(define (create-add mapping)
  (define record-type (mapping-record-type mapping))
  (lambda args
    (match args
      (((? keyword? key) value . tail)
       (apply add-values mapping (keys->values mapping args)))
      (((? (record-predicate record-type) record))
       (apply add-values mapping (record->values mapping record)))
      ((? list? values) (apply add-values mapping values)))))

(define (args->alist mapping args)
  (define record-type (mapping-record-type mapping))
  (match args
    (((? keyword? key) value . tail) (keys->alist args))
    (((? (record-predicate record-type) record)) (record->alist record-type record))
    ((value) (list (cons (mapping-primary-key mapping) value)))))

(define (arg->id mapping arg)
  (define record? (record-predicate (mapping-record-type mapping)))
  (match arg
    ((? record? arg) ((record-accessor (mapping-record-type mapping) (mapping-primary-key mapping)) arg))
    (value value)))

(define (create-update mapping)
  (lambda (arg . args)
    (define id (arg->id mapping arg))
    (define values (args->alist mapping args))
    ; refuse to implicitly update primary key
    (if (and (equal? 1 (length args)) (equal? 1 (length values)))
        (throw 'misc-error "Unexpected argument: ~a" (car args)))
    (map
      (lambda (pair)
        (define update-sql
          (format #f
            "UPDATE ~a SET ~a = ? WHERE ~a = ?;"
            (mapping-table mapping)
            (symbol->sql-string (car pair))
            (symbol->sql-string (mapping-primary-key mapping))))
        (create-table mapping)
        (let ((insert (sqlite-prepare database update-sql)))
          (sqlite-bind insert 1 (cdr pair))
          (sqlite-bind insert 2 id)
          (sqlite-step insert)
          (sqlite-finalize insert)))
      values)))

(define (create-remove mapping)
  (lambda (arg)
    (define id (arg->id mapping arg))
    (define delete-sql
      (format #f "DELETE FROM ~a WHERE ~a = ?;" (mapping-table mapping) (mapping-primary-key mapping)))
    (create-table mapping)
    (let ((delete (sqlite-prepare database delete-sql)))
      (sqlite-bind delete 1 id)
      (sqlite-step delete)
      (sqlite-finalize delete))))

(define (create-list mapping)
  (lambda args
    (define alist (args->alist mapping args))
    (define record-type (mapping-record-type mapping))
    (define fields (record-type-fields record-type))
    (define constructor (record-constructor record-type))
    (define select-sql
      (format #f
        "SELECT ~a FROM ~a WHERE ~a;"
        (string-join (mapping-columns mapping) ", ")
        (mapping-table mapping)
        (string-join
          (map 
            (lambda (pair) (format #f "~a = ?" (symbol->sql-string (car pair))))
            alist)
          " AND ")))
    (create-table mapping)
    (let ((select (sqlite-prepare database select-sql)))
      (map
        (lambda (n pair) (sqlite-bind select (+ 1 n) (cdr pair)))
        (iota (length alist)) 
        alist)
      (let ((result (sqlite-map (lambda (row) (apply constructor (vector->list row))) select)))
        (sqlite-finalize select)
        result))))

(define (create-get mapping)
  (define list-records (create-list mapping))
  (lambda args
    (match (apply list-records args)
      (() #f)
      ((row) row)
      (else (throw 'get-many "get returned more than one row")))))

(define (create-mapping proc-symbol record-type primary)
  (define mapping (make-mapping record-type primary))
  (define proc-name (symbol->string proc-symbol))
  (cond
    ((string-prefix? "add" proc-name) (create-add mapping))
    ((string-prefix? "update" proc-name) (create-update mapping))
    ((string-prefix? "remove" proc-name) (create-remove mapping))
    ((string-prefix? "get" proc-name) (create-get mapping))
    ((string-prefix? "list" proc-name) (create-list mapping))))

(define-syntax define-database-record-mapping
  (syntax-rules ()
    ((define-database-record-mapping record-type #:primary-key primary) #f)
    ((define-database-record-mapping record-type #:primary-key primary
       proc proc* ...)
     (begin
       (define proc (create-mapping 'proc record-type primary))
       (define-database-record-mapping record-type #:primary-key primary proc* ...)))
    ((define-database-record-mapping record-type proc* ...)
     (define-database-record-mapping
       record-type #:primary-key #f proc* ...))))
