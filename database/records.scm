(define-module (database records)
  #:export-syntax (define-database-record-mapping))

(use-modules
  (ice-9 match)
  (srfi srfi-1) (srfi srfi-9))

(use-modules (sqlite3))

(define database 
  (sqlite-open "database.sqlite3" (logior SQLITE_OPEN_CREATE SQLITE_OPEN_READWRITE)))

(define-public (set-database database')
  ; TODO close current database?
  (set! database database'))

(define (symbol->sql symbol)
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
  (make-mapping record-type primary-key types)
  mapping?
  (record-type mapping-record-type)
  (primary-key mapping-primary-key)
  (types mapping-types))

(define mappings '())

(define (map-record-type record-type primary-key field-types)
  (define types
    (map
      (lambda (field)
        (define type (assoc-ref field-types field))
        (define mapping (assoc-ref mappings type))
        (cond
          ((equal? 'rowid type) 'rowid)
          (mapping mapping)
          ((equal? #f type) #f)
          (else (throw 'invalid-field-argument "Invalid argument for field ~a: ~a" field type))))
      (record-type-fields record-type)))
  (define actual-primary-key
    (or
      primary-key
      (find identity
        (map
          (lambda (field type) (if (equal? type 'rowid) field #f))
          (record-type-fields record-type)
          types))))
  (define mapping (make-mapping record-type actual-primary-key types))
  (set! mappings (cons (cons record-type mapping) mappings))
  mapping)

(define (fields mapping) (record-type-fields (mapping-record-type mapping)))

(define (table mapping)
  (symbol->sql (record-type-name (mapping-record-type mapping))))

(define (columns mapping) (map symbol->sql (fields mapping)))

(define (rowid mapping)
  (find identity
    (map
      (lambda (field type) (and (equal? 'rowid type) field))
      (fields mapping)
      (mapping-types mapping))))

(define (create-table mapping)
  (define (constrained-column field type)
    (string-append
      (symbol->sql field)
      (cond
        ((equal? type 'rowid) " INTEGER PRIMARY KEY")
        ((equal? field (mapping-primary-key mapping)) " PRIMARY KEY")
        (else ""))))
  (sqlite-exec database
    (format #f 
      "CREATE TABLE IF NOT EXISTS ~a (~a);" 
      (table mapping) 
      (string-join
        (map constrained-column (fields mapping) (mapping-types mapping))
        ", "))))

(define (record->full-values mapping record)
  (map
    (lambda (field) (cons field ((record-accessor (mapping-record-type mapping) field) record)))
    (fields mapping)))

(define (record->primary-values mapping record)
  (define primary (mapping-primary-key mapping))
  (list
    (cons primary ((record-accessor (mapping-record-type mapping) primary) record))))

(define (keywords->values mapping args)
  (match args
    (() '())
    ((keyword value . tail)
     (cons (cons (keyword->symbol keyword) value) (keywords->values mapping tail)))))

(define (list->values mapping list)
  (map
    (lambda (key value) (cons key value))
    (fields mapping) list))

(define (arguments->values record->values mapping args)
  (match args
    (() '())
    (((? (record-predicate (mapping-record-type mapping)) record))
     (record->values mapping record))
    ((id) (list (cons (mapping-primary-key mapping) id)))
    (((? keyword? key) value . tail) (keywords->values mapping args))
    (else (list->values mapping args))))

(define (arguments->full-values mapping args) (arguments->values record->full-values mapping args))

(define (arguments->primary-values mapping args)
  (arguments->values record->primary-values mapping args))

(define (value->primitive type value)
  (if (and (mapping? type) ((record-predicate (mapping-record-type type)) value))
      ((record-accessor (mapping-record-type type) (mapping-primary-key type)) value)
      value))

(define (values->primitives mapping values)
  (define types (map cons (fields mapping) (mapping-types mapping)))
  (map
    (lambda (pair) (cons (car pair) (value->primitive (assoc-ref types (car pair)) (cdr pair))))
    values))

(define (arguments->full-primitives mapping args)
  (values->primitives mapping (arguments->full-values mapping args)))

(define (arguments->primary-primitives mapping args)
  (values->primitives mapping (arguments->primary-values mapping args)))

(define (get-last-inserted-rowid mapping)
  (define sql
    (format #f
      "SELECT ~a FROM ~a WHERE rowid = last_insert_rowid()"
      (symbol->sql (rowid mapping)) (table mapping)))
  (define select (sqlite-prepare database sql))
  (define value
    (first
      (sqlite-map (lambda (row) (vector-ref row 0)) select)))
  (sqlite-finalize select)
  value)

(define (fill-in-rowid mapping values)
  (define id (rowid mapping))
  (if (and id (not (assoc-ref values id)))
    (cons (cons id (get-last-inserted-rowid mapping)) values)
    values))

(define (create-add mapping)
  (lambda args
    (define values (arguments->full-values mapping args))
    (define primitives (values->primitives mapping values))
    (define insert-sql
      (format #f
        "INSERT INTO ~a (~a) VALUES (~a)"
        (table mapping)
        (string-join (map symbol->sql (map car primitives)) ", ")
        (string-join (map (const "?") primitives) ", ")))
    (create-table mapping)
    (let ((insert (sqlite-prepare database insert-sql)))
      (map
        (lambda (n pair) (sqlite-bind insert (+ 1 n) (cdr pair)))
        (iota (length primitives)) primitives)
      (sqlite-step insert)
      (sqlite-finalize insert))
    (let* ((values+id (fill-in-rowid mapping values))
           (constructor (record-constructor (mapping-record-type mapping))))
      (apply constructor (map (lambda (field) (assoc-ref values+id field)) (fields mapping))))))

(define (arg->id mapping arg)
  (define record? (record-predicate (mapping-record-type mapping)))
  (match arg
    ((? record? arg) ((record-accessor (mapping-record-type mapping) (mapping-primary-key mapping)) arg))
    (value value)))

; TODO can i do this in a single statement?
(define (create-update mapping)
  (lambda (arg . args)
    (define id (arg->id mapping arg))
    (define primitives (arguments->full-primitives mapping args))
    ; refuse to implicitly update primary key
    (if (and (equal? 1 (length args)) (equal? 1 (length primitives)))
        (throw 'misc-error "Unexpected argument: ~a" (car args)))
    (map
      (lambda (pair)
        (define update-sql
          (format #f
            "UPDATE ~a SET ~a = ? WHERE ~a = ?;"
            (table mapping)
            (symbol->sql (car pair))
            (symbol->sql (mapping-primary-key mapping))))
        (create-table mapping)
        (let ((insert (sqlite-prepare database update-sql)))
          (sqlite-bind insert 1 (cdr pair))
          (sqlite-bind insert 2 id)
          (sqlite-step insert)
          (sqlite-finalize insert)))
      primitives)))

(define (generate-where alist)
  (if
    (null? alist)
    ""
    (format #f "WHERE ~a"
      (string-join
        (map 
          (lambda (pair) 
            (if (cdr pair)
                (format #f "~a = ?" (symbol->sql (car pair)))
                (format #f "~a IS NULL" (symbol->sql (car pair)))))
          alist)
        " AND "))))
    
(define (bind-values select n values)
  (match values
    (((key . #f) . tail) (bind-values select n tail))
    (((key . value) . tail) (sqlite-bind select n value) (bind-values select (+ 1 n) tail))
    (() #f)))

(define (create-remove mapping)
  (lambda args
    (define primitives (arguments->primary-primitives mapping args))
    (define delete-sql
      (format #f "DELETE FROM ~a ~a;"
        (table mapping)
        (generate-where primitives)))
    (create-table mapping)
    (let ((delete (sqlite-prepare database delete-sql)))
      (bind-values delete 1 primitives)
      (sqlite-step delete)
      (sqlite-finalize delete))))

; TODO use JOIN query instead of multiple queries in case of linked table
(define (create-list mapping)
  (define record-type (mapping-record-type mapping))
  (define fields (record-type-fields record-type))
  (define constructor (record-constructor record-type))
  (define converters
    (map
      (lambda (type) (if (mapping? type) (lambda (value) ((create-get type) value)) identity))
      (mapping-types mapping)))
  (lambda args
    (define primitives (arguments->primary-primitives mapping args))
    (define select-sql
      (format #f "SELECT ~a FROM ~a ~a;"
        (string-join (columns mapping) ", ")
        (table mapping)
        (generate-where primitives)))
    (define (read-row row)
      (apply constructor
        (map (lambda (converter value) (converter value)) converters (vector->list row))))
    (create-table mapping)
    (let ((select (sqlite-prepare database select-sql)))
      (bind-values select 1 primitives)
      (let ((result (sqlite-map read-row select)))
        (sqlite-finalize select)
        result))))

(define (create-get mapping)
  (define list-records (create-list mapping))
  (lambda args
    (match (apply list-records args)
      (() #f)
      ((row) row)
      (else (throw 'get-many "get returned more than one row")))))

(define (map-procedure mapping proc-symbol)
  (define proc-name (symbol->string proc-symbol))
  (cond
    ((string-prefix? "add" proc-name) (create-add mapping))
    ((string-prefix? "update" proc-name) (create-update mapping))
    ((string-prefix? "remove" proc-name) (create-remove mapping))
    ((string-prefix? "get" proc-name) (create-get mapping))
    ((string-prefix? "list" proc-name) (create-list mapping))))

; TODO at least make this a fluid
(define mapping #f)
  
(define-syntax define-database-record-mapping
  (syntax-rules ()
    ((define-database-record-mapping #:mapping mapping) #f)
    ((define-database-record-mapping #:mapping mapping proc procs* ...)
     (begin
       (define proc (map-procedure mapping (quote proc)))
       (define-database-record-mapping #:mapping mapping procs* ...)))
    ((define-database-record-mapping record-type #:primary-key primary #:fields (fields* ...)
       (field type) args* ...)
     (define-database-record-mapping record-type #:primary-key primary
       #:fields (fields* ... (field type)) args* ...))
    ((define-database-record-mapping record-type #:primary-key primary #:fields ((field type) ...) 
       proc* ...)
     (begin
       (set! mapping (map-record-type record-type primary (list (cons (quote field) type) ...)))
       (define-database-record-mapping #:mapping mapping proc* ...)))
    ((define-database-record-mapping record-type #:primary-key primary args* ...)
     (define-database-record-mapping record-type #:primary-key primary #:fields () args* ...))
    ((define-database-record-mapping record-type args* ...)
     (define-database-record-mapping record-type #:primary-key #f args* ...))))

