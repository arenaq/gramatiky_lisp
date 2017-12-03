(define (_length aList)
  (if (empty? aList)
      0
      (+ 1 (_length (cdr aList)))
  )
)

(define (_listEquals? aObject1 aObject2)
  (cond
    ((not (list? aObject1)) (eq? aObject1 aObject2))
    ((not (list? aObject2)) (eq? aObject1 aObject2))
    ((not (= (_length aObject1) (_length aObject2))) #f)
    ((= (_length aObject1) 0) #t)
    (else
      (if (_listEquals? (car aObject1) (car aObject2))
          (_listEquals? (cdr aObject1) (cdr aObject2))
          #f
      )
    )
  )
)

(define (_listContainsObject aList aObject)
  (cond
    ((not (list? aList)) (error "Illegal arguments: _listContainsObject list object"))
    ((empty? aList) #f)
    ((_listEquals? aObject (car aList)))    
    (else (_listContainsObject (cdr aList) aObject))
  )
)

(define (_listToSet aList)
  (cond
    ((empty? aList) '())
    ((_listContainsObject (cdr aList) (car aList)) (_listToSet (cdr aList)))
    (else (cons (car aList) (_listToSet (cdr aList))))
  )
)

(define (_listsToSets aList)
  (cond
    ((empty? aList) '())
    (else (cons (_listToSet (car aList)) (_listsToSets (cdr aList))))
  )
)

(define (_sjednoceni_rekurze aList1 aList2)
  (cond
    ((empty? aList1) aList2)
    ((_listContainsObject aList2 (car aList1)) (_sjednoceni_rekurze (cdr aList1) aList2))
    (else (cons (car aList1) (_sjednoceni_rekurze (cdr aList1) aList2)))
  )
)

(define (sjednoceni aList1 aList2)
  (_sjednoceni_rekurze (_listToSet aList1) (_listToSet aList2))
)

(define (_sjednoceniHromadne_rekurze aListL)
  (cond
    ((not (list? aListL)) (error "Illegal arguments: sjednoceni_m list_of_lists"))
    ((not (list? (car aListL))) (error "Illegal arguments: sjednoceni_m list_of_lists"))
    ((empty? aListL) (_sjednoceni_rekurze '() '()))
    ((= (_length aListL) 1) (car aListL))
    ((= (_length aListL) 2) (_sjednoceni_rekurze (car aListL) (cadr aListL)))
    (else (_sjednoceniHromadne_rekurze (sjednoceni (list (sjednoceni (car aListL) (cadr aListL))) (cddr aListL))))
  )
)

(define (sjednoceniHromadne aListL)
  (_sjednoceniHromadne_rekurze (_listsToSets aListL))
)

(define (prunik_r aList1 aList2)
  (cond
    ((empty? aList1) '())
    ((empty? aList2) '())
    ((_listContainsObject aList2 (car aList1)) (sjednoceni (list (car aList1)) (prunik_r (cdr aList1) aList2)))
    (else (prunik_r (cdr aList1) aList2))
  )
)

(define (prunik aList1 aList2)
  (prunik_r (_listToSet aList1) (_listToSet aList2))
)

(define (prunik_m aListL)
  (cond
    ((not (list? aListL)) (error "Illegal arguments: prunik_m list_of_lists"))
    ((not (list? (car aListL))) (error "Illegal arguments: prunik_m list_of_lists"))
    ((empty? aListL) (prunik_r '() '()))
    ((= (_length aListL) 1) (prunik_r '() (car aListL)))
    ((= (_length aListL) 2) (prunik_r (car aListL) (cadr aListL)))
    (else (prunik_m (sjednoceni (list (prunik (car aListL) (cadr aListL))) (cddr aListL))))
  )
)

(_length '(A 2 B 4 5))
;5
(_listEquals? 'A 'B)
;f
(_listEquals? '(A B) '(A B))
;t
(_listContainsObject '(A 1 2 3 B C D E) 'E)
;t
(_listContainsObject '((A B)(B C)(C D)(F 4)(D F)(A G)) '(A G))
;t
(_listToSet '(H A 1 2 G A B C A 1 2 B G C C H))
;(A 1 2 B G C H)
(sjednoceni '(A 1 2 A B C) '(A 2 1 B C C H))
;(A 2 1 B C H)
(sjednoceniHromadne '( ((A)(B)(C)) ((D)(E)(C)) (A) (A 1)(B 2 2)(C 3 3) ))
;((A) (B) (D) (E) (C) A 1 B 2 C 3)
(sjednoceniHromadne '( (A B) (A B C D E E E E)))
;(A B C D E)
(prunik '(6 h 4 g s 1) '(g 5 f 7 s 1))
;(g s 1)
(prunik_m '((A B C D)(D C D A)(C D E F G)(D C E F)(A B C E F G)))
;(C)