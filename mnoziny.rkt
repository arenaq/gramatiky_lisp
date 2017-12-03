(define (delkaListu aList)
  (if (empty? aList)
      0
      (+ 1 (delkaListu (cdr aList)))
  )
)

(define (rovnostObjektu aObject1 aObject2)
  (cond
    ((not (list? aObject1)) (eq? aObject1 aObject2))
    ((not (list? aObject2)) (eq? aObject1 aObject2))
    ((not (= (delkaListu aObject1) (delkaListu aObject2))) #f)
    ((= (delkaListu aObject1) 0) #t)
    (else
      (if (rovnostObjektu (car aObject1) (car aObject2))
          (rovnostObjektu (cdr aObject1) (cdr aObject2))
          #f
      )
    )
  )
)

(define (isObjectInList a aList)
  (cond
    ((not (list? aList)) (error "Illegal arguments: isObjectInList object list"))
    ((empty? aList) #f)
    ((rovnostObjektu a (car aList)))    
    (else (isObjectInList a (cdr aList)))
  )
)

(define (mnozina aList)
  (cond
    ((empty? aList) '())
    ((isObjectInList (car aList) (cdr aList)) (mnozina (cdr aList)))
    (else (cons (car aList) (mnozina (cdr aList))))
  )
)

(define (listListuNaListMnozin aList)
  (cond
    ((empty? aList) '())
    (else (cons (mnozina (car aList)) (listListuNaListMnozin (cdr aList))))
  )
)

(define (sjednoceni_r aList1 aList2)
  (cond
    ((empty? aList1) aList2)
    ((isObjectInList (car aList1) aList2) (sjednoceni_r (cdr aList1) aList2))
    (else (cons (car aList1) (sjednoceni_r (cdr aList1) aList2)))
  )
)

(define (sjednoceni aList1 aList2)
  (sjednoceni_r aList1 (mnozina aList2))
)

(define (sjednoceni_m aListL)
  (cond
    ((not (list? aListL)) (error "Illegal arguments: sjednoceni_m list_of_lists"))
    ((not (list? (car aListL))) (error "Illegal arguments: sjednoceni_m list_of_lists"))
    ((empty? aListL) (sjednoceni_r '() '()))
    ((= (delkaListu aListL) 1) (car aListL))
    ((= (delkaListu aListL) 2) (sjednoceni_r (car aListL) (cadr aListL)))
    (else (sjednoceni_m (sjednoceni (list (sjednoceni (car aListL) (cadr aListL))) (cddr aListL))))
  )
)

(define (sjednoceni_mm aListL)
  (sjednoceni_m (listListuNaListMnozin aListL))
)

(define (prunik_r aList1 aList2)
  (cond
    ((empty? aList1) '())
    ((empty? aList2) '())
    ((isObjectInList (car aList1) aList2) (sjednoceni (list (car aList1)) (prunik_r (cdr aList1) aList2)))
    (else (prunik_r (cdr aList1) aList2))
  )
)

(define (prunik aList1 aList2)
  (prunik_r aList1 (mnozina aList2))
)

(define (prunik_m aListL)
  (cond
    ((not (list? aListL)) (error "Illegal arguments: prunik_m list_of_lists"))
    ((not (list? (car aListL))) (error "Illegal arguments: prunik_m list_of_lists"))
    ((empty? aListL) (prunik_r '() '()))
    ((= (delkaListu aListL) 1) (prunik_r '() (car aListL)))
    ((= (delkaListu aListL) 2) (prunik_r (car aListL) (cadr aListL)))
    (else (prunik_m (sjednoceni (list (prunik (car aListL) (cadr aListL))) (cddr aListL))))
  )
)

(delkaListu '(A 2 B 4 5))
;5
(rovnostObjektu 'A 'B)
;f
(rovnostObjektu '(A B) '(A B))
;t
(isObjectInList 'E '(A 1 2 3 B C D E))
;t
(isObjectInList '(A G) '((A B)(B C)(C D)(F 4)(D F)(A G)))
;t
(mnozina '(H A 1 2 G A B C A 1 2 B G C C H))
;(A 1 2 B G C H)
(sjednoceni '(A 1 2 A B C) '(A 2 1 B C C H))
;(A 2 1 B C H)
(sjednoceni_mm '( ((A)(B)(C)) ((D)(E)(C)) (A) (A 1)(B 2 2)(C 3 3) ))
;((A) (B) (D) (E) (C) A 1 B 2 C 3)
(sjednoceni_mm '( (A B) (A B C D E E E E)))
;(A B C D E)
(prunik '(6 h 4 g s 1) '(g 5 f 7 s 1))
;(g s 1)
(prunik_m '((A B C D)(D C D A)(C D E F G)(D C E F)(A B C E F G)))
;(C)