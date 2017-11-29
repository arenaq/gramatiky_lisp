;--------------------------------------
;CVICENI 1
;--------------------------------------

;definice funkce a prime spusteni
((lambda (x y) (+ x y)) 1 2)

;definice funkce plus
(define plus (lambda (x y) (+ x y)))

;definice funkce pluskratke
(define (pluskratke x y) (+ x y))

;volani funkce plus
(plus 2 3)

;volani funkce pluskratke
(pluskratke 2 4)

;definice funkce faktorial
(define (factorial n)
    (if (< n 2)
      1
      (* n (factorial (- n 1)))))

;volani funkce faktorial
(factorial 5)

;definice funkce absolut
(define (absolut x)
  (if (>= x 0)
      x
      (- x)))

;volani funkce absolut
(absolut 35575)

;dot pair
(cons 1 2)

;list vytvoreny pomoci apostrofu
'(1 2 3 4)

;list vytvoreny pomoci funkce list
(list 5 6 7 8)

;cons - prida prvek na zacatek listu, car - vrati prvni prvek, cdr - vrati vse krom prvnihoi prvku, append - spojeni dvou listu
(cons 1 '(2 3 4))
(car '(1 2 3 4))
(cdr '(1 2 3 4))
(append '(1 2) '(3 4))

;definice funkce fibonaciho posloupnosti
(define (fibonaci x)
  (if (<= x 2)
      1
      (+ (fibonaci (- x 1)) (fibonaci (- x 2)))))

(fibonaci 0)

;definice funkce faktorial pomoci koncove rekurze
(define (factorial_k n r)
    (if (<= n 0)
      r
      (factorial_k (- n 1) (* r n))))

;volani funkce faktorial
(factorial 21)
(factorial_k 21 1)

;definice funkce fibonaciho posloupnosti pomoci koncove rekurze
(define (fibonaci_k x x-1 x-2)
  (cond
    ((= x 0) x-2)
    ((= x 1) x-1)
    (else (fibonaci_k (- x 1) (+ x-1 x-2) x-1))))

(fibonaci 21)
(fibonaci_k 21 1 0)

;--------------------------------------
;CVICENI 3
;--------------------------------------
;velikost listu
;konecna rekurze
(define (size_k list result)
  (if (empty? list)
      result
      (size_k (cdr list) (+ 1 result))
  )
)

(size_k (list 1 2 3 4 5 6) 0)

;normalni rekurze
(define (size list)
  (if (empty? list)
      0
      (+ 1 (size (cdr list)))
  )
)

(size '(1 2 (3 4) 5 6))

;obraceni listu
;koncova rekurze
(define (reverse_k list result)
  (if (empty? list)
      result
      (reverse_k (cdr list)
               cons (car list result))
  )
)

;normalni rekurze
(define (reverse_n alist)
  (if (empty? alist)
      alist
      (append (reverse_n (cdr alist)) (list (car alist)))
  )
)

(reverse_n '(1 2 3 4 5 6))

;append
;normální rekurze
(define (app aList1 aList2)
  (if (empty? aList1)
      aList2
      (cons (car aList1) (app (cdr aList1) aList2))
  )
)

(app '(1 2) '(2 3))

;maximum z listu
;normalni rekurze
(define (maxim aList)
  (if (empty? (cdr aList))
     (car aList)
     (max (car aList) (maxim (cdr aList))))
  )

(maxim '(1 2 3 7 8 9 54 2 14 5 36))

;kalkulacka
(define (pocitej vyraz)
  (case (cadr vyraz)
    ((+) (+ (car vyraz) (caddr vyraz)))
    ((-) (- (car vyraz) (caddr vyraz)))
    ((*) (* (car vyraz) (caddr vyraz)))
    ((/) (/ (car vyraz) (caddr vyraz)))
  ))

(pocitej '(3 / 2))

(define (ionorder vyraz)
  (cond
    ((list? (car vyraz)) (inorder (car vyraz)))
    ((list? (caddr vyraz)) (inorder (caddr vyraz)))
    (else (pocitej vyraz))))

(inorder '(1 + 1))
(inorder '(1 * (1 * 2)))
(inorder '(((1 + 1) - 1 + (1 * 1)))