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
         (column-info 'capital 'boolean)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f #t)
         (list "Warsaw"  "Warsaw"  517 #t #t))))
         
(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number)
         (column-info 'city    'string))
   (list (list "Poland" 38 "Wrocław")
         (list "Germany" 83 "Berlin")
         (list "France" 67 "Paris")
         (list "Spain" 47 "Madrit"))))

(define countries2
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
  (for tab row))

(define (table-insert row tab)
  (if (check-type row (table-schema tab))
      (table
         (table-schema tab)
         (append (table-rows tab) (list row)))
      (error "Bledny typ danych\n"))
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
           (if (go (ask (lt-f-name form) col) col row (lt-f-name form) (lt-f-val form)) #t #f)]
  ))

#|
(define (ask key cols)
  (if (eq? key (column-info-name (car cols)))
      (answer (column-info-type (car cols)))
      (ask key (cdr cols))))
      
(define (answer key)
   (cond [(eq? key 'string) string<? ]
         [(eq? key 'number) <]
         [(eq? key 'boolean) compare-bool]
         [(eq? key 'symbol) compare-symbol]
  ))      
|#


;lt-f nie śmiga problem ze wszystkim co nie jest number? bo nie może porównywać typów
;zamiast ask było <

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


;(table-project '( city country ) cities)
; Projekcja
(define (schemeloop tab taba cols xs)
  (if (null? tab)
      xs
      (if (equal? (column-info-name (car tab)) (car cols))
          (makescheme (cdr cols) taba (append xs (list (car tab))))
          (schemeloop (cdr tab) taba cols xs))))

(define (makescheme cols tab xs)
    (if (null? cols)
      xs
      (schemeloop tab tab cols xs)))

(define (rowloop cols colsa row rowa sch scha xs ys)
  (if (null? cols)
      (makerows colsa scha (cdr rowa) (append ys (list xs)))
      (if (equal? (column-info-name (car sch)) (car cols))
         (rowloop (cdr cols) colsa (car rowa) rowa scha scha (cons (car row) xs) ys)
         (rowloop cols colsa (cdr row) rowa (cdr sch) scha xs ys))))

(define (makerows cols sch rows xs)
  (if (null? rows)
      xs
      (rowloop cols cols (car rows) rows sch sch  '() xs))
  )

(define (table-project cols tab)
  (table
  (makescheme cols (table-schema tab) '())
   (makerows (reverse cols) (table-schema tab) (table-rows tab) '())
   ))

; Sortowanie //niemalejace
(define (compare-bool x y)
   (implies x y))

(define (compare-symbol x y)
  (string<? (symbol->string x) (symbol->string y)))

(define (ask key cols)
  (if (eq? key (column-info-name (car cols)))
      (answer (column-info-type (car cols)))
      (ask key (cdr cols))))
      
(define (answer key)
   (cond [(eq? key 'string) string<? ]
         [(eq? key 'number) <]
         [(eq? key 'boolean) compare-bool]
         [(eq? key 'symbol) compare-symbol]
  ))      

(define (find-val elem key cols)
  (if (eq? key (column-info-name (car cols)))
      (car elem)
      (find-val (cdr elem) key (cdr cols))))


(define test
  ( list
( column-info 'city 'string )
( column-info 'country 'string )
( column-info 'area 'number )
( column-info 'capital 'boolean ) ))

(define (compare keys new old schema)
  (if (null? keys)
      #f
      (if (eq? (find-val new (car keys) schema) (find-val old (car keys) schema))
          (compare (cdr keys) new old schema)
          ((ask (car keys) schema) (find-val new (car keys) schema) (find-val old (car keys) schema)))))
(define (add keys new  rows schema)
  (if (null? rows)
      (list new)
      (if (compare keys new (car rows) schema)
          (cons new rows)
          (cons (car rows) (add keys new (cdr rows) schema)))))

(define (loop ys schema sort rows)
  (if (null? rows)
      ys
      (loop (table (table-schema ys) (add sort (car rows) (table-rows ys) (table-schema ys))) schema sort (cdr rows)))) 

(define (table-sort cols tab)
  (loop (empty-table (table-schema tab)) (table-schema tab) cols (table-rows tab))) 






























#|
(define (insert-in-place key n xs)
  (if (null? xs)
     (list n)
     (if (key n (car xs))
         (cons n xs)
         (cons (car xs) (insert-in-place key n (cdr xs))))))

(define (loop xs ys key)
  (if (null? xs)
      ys
      (loop (cdr xs) (insert-in-place key (car xs) ys)))) 

(define (insert-sort key xs)
 (loop xs null key))



(define (table-sort cols tab)
  null)
|#




















































; Złączenie
;(help-join (table-schema tab1) (table-schema tab2) (table-schema tab1) '()  0)) -> 0 daje tablice powtorzonych
#|



(define (schemetolist xs ys)
  (if (null? xs)
    ys
    (schemetolist (cdr xs) (append ys (list (column-info-name (car xs)))))))


(define (loop tab xs)
  (if (equal? (car (column-info-name (table-schema tab))) (car xs))
      (zmiana (table-rename (column-info-name (car (table-schema tab))) (string-append (symbol->string (column-info-name (car (table-schema tab)))) "2") tab) (cdr xs))
      (loop (cdr (table-schema tab)) xs)))
      
(define (zmiana tab xs)
  (if (null? xs)
      tab
      (loop tab xs)
      ))


(define (fixed tab1 tab2 unique reps)
  (table
   unique
   ;(table-rows (table-project (schemetolist unique '()) (table-cross-join tab1 tab2)))))
   ;(cdr reps)))
   (table-schema (table-cross-join tab1 (zmiana tab2 reps)))))
   |#





#|
(define (help-join tab1 tab2 xs ys zm)
  (if (null? tab2)
      (if (= zm 1)
      xs
      ys)
      (if (goh xs (column-info-name (car tab2)))
          (help-join tab1 (cdr tab2) (append xs (list (car tab2))) ys zm)
          (help-join tab1 (cdr tab2) xs (append ys (list (column-info-name (car tab2))))zm)))
  )

(define (goh col x)
  (cond [(null? col) #t]
        [(equal? (column-info-name (car col)) x) #f]
        [else (goh (cdr col) x)]
  ))
|#

;(define (help-join 


(define (table-natural-join tab1 tab2)
  null)
;  (help-join (table-schema tab1) (table-schema tab2) '() '() 1))
 



















































