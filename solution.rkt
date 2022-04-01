#lang racket

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table                       
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Warsaw"  "Poland"  517 #t)
         (list "Wrocław" "Poland"  293 #f)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))
(define cities2
  (table                       
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Warsaw"  517 #t))))
         

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

;(table-insert '("Gdynia" "Poland" 244 #f) cities)
; Wstawianie
(define (type-question elem x)
    (cond [(equal? 'string elem) 
            (string? x)]
          [(equal? 'number elem) 
            (number? x)]
          [(equal? 'boolean elem) 
            (boolean? x)]
          [(equal? 'symbol elem) 
            (symbol? x)]))

(define (check-type row tab)
  (define (for tab row)
    (cond [(and (null? row) (not (null? tab))) #f]
          [(and (null? tab) (not (null? row))) #f]
          [(and (null? tab)      (null? row))  #t]
          [(type-question (column-info-type (car tab)) (car row)) (for (cdr tab) (cdr row))]
          [else #f]))
  (for tab row)
   )


(define (table-insert row tab)
  (if (check-type row (table-schema tab))
      (table
         (table-schema tab)
         (append (table-rows tab) (list row)))
      (error "Bledny typ danych\n"))
  )

;(table-project '(city area) cities)
; Projekcja
(define (scheme cols tab)
  (define (for cols tab xs)
    (cond [(null? cols) xs]
          [(null? tab) xs]
          [(equal? (column-info-name (car tab)) (car cols)) (for (cdr cols) (cdr tab) (append xs (list (car tab))))]
          [else (for cols (cdr tab) xs)]
  ))
  (for cols tab '()))

(define (for1 all cols tab xs ys)
  (cond [(null? tab) ys]
        [else (for2 all all cols cols (car tab) tab xs ys)]))

(define (for2 all1 all cols1 cols tab1 tab xs ys)
  (cond [(null? all1)  (for1 all cols (cdr tab) '() (append ys (list xs)))]
        [(null? cols1) (for1 all cols (cdr tab) '() (append ys (list xs)))]
        [(null? tab1)  (for1 all cols (cdr tab) '() (append ys (list xs)))]
        [(equal? (column-info-name (car all1)) (column-info-name (car cols1)))
         (for2 (cdr all1) all (cdr cols1) cols (cdr tab1) tab (append xs (list (car tab1))) ys)]
        [else (for2 (cdr all1) all cols1 cols (cdr tab1) tab xs ys)])
  )

(define (info all cols tab)
  (for1 all cols tab '() '())
  )

;(table-project '( city country ) cities)
(define (table-project cols tab)
  (table
   (scheme cols (table-schema tab))
   (info (table-schema tab) (scheme cols (table-schema tab)) (table-rows tab)))
  )
;(table-rename 'country 'kraj cities)
;Zmiana nazwy
(define (change tab col ncol xs)
  (cond [(null? tab) xs]
        [(equal?  (column-info-name (car tab)) col) (change (cdr tab) col ncol (append xs (list (column-info ncol (column-info-type (car tab))))))]
        [else     (change (cdr tab) col ncol (append xs (list (car tab))))]))
                          
(define (table-rename col ncol tab)
  (table
  (change (table-schema tab) col ncol '())
  (table-rows tab))
  )

;(table-select ( or-f ( eq-f 'capital #t )( not-f ( lt-f 'area 300) ) )cities)
; Selekcja
(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))


(define (go f col row x y)
  (cond [(or (null? row) (null? col)) #f]
        [(equal? (column-info-name (car col)) x) (if (f (car row) y) #t #f)]
        [else (go f (cdr col) (cdr row) x y)]
  ))

(define (go2 f col cols row rows x y)
  (cond [(or (null? row) (null? col)) #f]
        [(equal? (column-info-name (car col)) x) (if (go equal? cols rows y (car row)) #t #f)]
        [else (go2 f (cdr col) cols (cdr row) rows x y)]
  ))

(define (type-select form col row xs)
    (cond [(and-f? form) 
            (if (and (type-select (and-f-l form) col row xs) (type-select (and-f-r form) col row xs))#t #f)]
          [(or-f? form) 
            (if (or (type-select (or-f-l form) col row xs) (type-select (or-f-r form) col row xs))#t #f)]
          [(not-f? form) 
            (if (not (type-select (not-f-e form) col row xs)) #t #f)]
          [(eq-f? form)
           (if (go equal? col row (eq-f-name form) (eq-f-val form)) #t #f)]
          [(eq2-f? form) 
            (if (go2 equal? col col row row (eq2-f-name form) (eq2-f-name2 form)) #t #f)]
          [(lt-f? form) 
            (if (go < col row (lt-f-name form) (lt-f-val form)) #t #f)]
          ))

(define (help-select form col row xs)
  (if (null? row)
      xs
      (if (type-select form col (car row) xs)
          (help-select form col (cdr row) (append xs (list (car row))))
          (help-select form col (cdr row) xs)
         )))

(define (table-select form tab) 
  (table
   (table-schema tab)
  (help-select form (table-schema tab) (table-rows tab) '()))
  )

;( table-cross-join cities( table-rename 'country 'country2 countries ) )
; Złączenie kartezjańskie
(define (gotab2 rows1 rows2 arows2 xs)
  (if (null? rows2)
      (gotab1 (cdr rows1) arows2 xs)
      (gotab2 rows1 (cdr rows2) arows2 (append xs (list (append (car rows1) (car rows2)))))))

(define (gotab1 rows1 rows2 xs)
  (if (null? rows1)
      xs
      (gotab2 rows1 rows2 rows2 xs)))
      

(define (table-cross-join tab1 tab2)
  (table
   (append (table-schema tab1) (table-schema tab2))
   (gotab1 (table-rows tab1) (table-rows tab2) '())
  ))

; Sortowanie
(define (build cols tab xs ys)
  (if (null? ys) (help cols (cdr tab) (list xs))  (append ys (list xs)))
  )

(define (help cols tab xs)
  (if (null? tab) xs (build cols tab (car tab) xs))) 

(define (table-sort cols tab)
  null
  ;(table
  ; (table-schema tab)
   ;(help cols (table-rows tab) '()))
  )
; Złączenie
(define (table-natural-join tab1 tab2)
  null;; uzupełnij
  )