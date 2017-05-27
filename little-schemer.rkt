#lang racket

(define atom?
  (λ (x)
    "(-> S-exp Bool)"
    "returns true if the S-exp is an atom"
    (and (not (pair? x)) (not (empty? x)))))
;(atom? '())

(define lat?
  (λ (l)
    "(-> (Listof S-exp) Bool"
    "returns true if every element in list is an atom"
    (cond
      [(empty? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))
;(and (eq? (lat? '(Jack Sprat could eat no chicken fat))
;          #t)
;     (eq? (lat? '((Jack) Sprat could eat no chicken fat))
;          #f)
;     (eq? (lat? '(Jack (Sprat could) eat no chicken fat))
;          #f)
;     (eq? (lat? '())
;          #t)
;     "lat? works")


(define member?
  (λ (a lat)
    "(-> atom (Listof atom) Bool)"
    "returns true if a is in the list"
    (cond
      [(empty? lat) #f]
      [else (or (eq? a (car lat))
                (member? a (cdr lat)))])))
;(and (eq? (member? 'poached '(fried eggs and scrambled eggs))
;          #f)
;     (eq? (member? 'meat '(mashed potatoes and meat gravy))
;          #t)
;     (eq? (member? 'liver '(bagels and lox))
;          #f)
;     "member? works")


(define rember
  (λ (a lat)
    "(-> atom (Listof atom) (Listof atom))"
    "removes the first instance of a if it exists in list"
    (cond
      [(empty? lat) '()]
      ;[(eq? a (car lat)) (rember a (cdr lat))] ; remove all
      [(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat)
                  (rember a (cdr lat)))])))
;(and (equal? (rember 'mint '(lamb chops and mint jelly))
;             '(lamb chops and jelly))
;     (equal? (rember 'mint '(lamb chops and mint flavored mint jelly))
;             '(lamb chops and flavored mint jelly))
;     (equal? (rember 'toast '(bacon lettuce and tomato))
;             '(bacon lettuce and tomato))
;     (equal? (rember 'cup '(coffee cup tea cup and hick cup))
;             '(coffee tea cup and hick cup))
;     (equal? (rember 'and '(bacon lettuce and tomato))
;             '(bacon lettuce tomato))
;     "rember works")


(define firsts
  (λ (lol)
    "(-> (Listof (List atoms)) (Listof atoms))"
    "returns a list of the car's of the argument"
    (cond
      [(empty? lol) '()]
      [else (cons (car (car lol))
                  (firsts (cdr lol)))])))
;(and (equal? (firsts '((apple peach pumpkin)
;                       (plum pear cherry)
;                       (grape raisin pea)
;                       (bean carrot eggplant)))
;             '(apple plum grape bean))
;     (equal? (firsts '((a b) (c d) (e f)))
;             '(a c e))
;     (equal? (firsts '())
;             '())
;     (equal? (firsts '((five plums)
;                       (four)
;                       (eleven green oranges)))
;             '(five four eleven))
;     (equal? (firsts '(((five plums) four)
;                       (eleven green oranges)
;                       ((no) more)))
;             '((five plums) eleven (no)))
;     "firsts works")


(define insertR
  (λ (new old lat)
    "(-> atom atom (Listof atom) (Listof atom))"
    "returns a list of atoms with new inserted to the right of old"
    (cond
      [(empty? lat) '()]
      [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))])))

(define insertL
  (λ (new old lat)
    "(-> atom atom (Listof atom) (Listof atom))"
    "returns a list of atoms with new inserted to the left of old"
    (cond
      [(empty? lat) '()]
      [(eq? (car lat) old) (cons new lat)]
      [else (cons (car lat) (insertL new old (cdr lat)))])))

(define subset
  (λ (new old lat)
    (cond
      [(empty? lat) '()]
      [(eq? (car lat) old) (cons new (cdr lat))]
      [else (cons (car lat) (subset new old (cdr lat)))])))

(define subst2
  (λ (new o1 o2 lat)
    (cond
      [(empty? lat) '()]
      [(eq? (car lat) o1) (cons new (cdr lat))]
      [(eq? (car lat) o2) (cons new (cdr lat))]
;      [(or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))]
      [else (cons (car lat) (subst2 new o1 (cdr lat)))])))

(define multirember
  (λ (a lat)
    (cond
      [(empty? lat) '()]
      [(eq? a (car lat)) (multirember a (cdr lat))] ; remove all
      ;[(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat)
                  (multirember a (cdr lat)))])))

;(multirember 'a '(a b c d a e f))
;(multirember 'cup '(coffee cup tea cup and hick cup))

(define multiinsertR
  (λ (new old lat)
    (cond
      [(empty? lat) '()]
      [(eq? (car lat) old)
       (cons old (cons new (multiinsertR new old (cdr lat))))]
      [else (cons (car lat) (multiinsertR new old (cdr lat)))])))

;(multiinsertR 'b 'a '(a c a c))

;(insertR 'topping 'fudge '(ice cream with fudge for desert))
;(insertR 'jalapeno 'and '(tacos tamales and salsa))
;(insertR 'e 'd '(a b c d f g d h))
;
;(subset 'topping 'fudge '(ice cream with fudge for desert))
;(subset 'jalapeno 'and '(tacos tamales and salsa))
;(subset 'e 'd '(a b c d f g d h))
;
;(insertL 'topping 'fudge '(ice cream with fudge for desert))
;(insertL 'jalapeno 'and '(tacos tamales and salsa))
;(insertL 'e 'd '(a b c d f g d h))
;
;(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate toppings))

;; Chapter 4
;; add1, sub1, zero? already defined in racket
(define o+
  (λ (n m)
    "(-> Integer Integer Integer)"
    "returns n + m"
    (cond
      [(zero? m) n]
      [else (o+ (add1 n) (sub1 m))]))) ; used an iterative version
;(and (= (o+ 46 12)
;        58)
;     "o+ works")


(define o-
  (λ (n m)
    "(-> Integer Integer Integer)"
    "returns n - m"
    (cond
      [(zero? m) n]
      [else (o- (sub1 n) (sub1 m))]))) ; used an iterative version
;(and (= (o- 14 3)
;        11)
;     (= (o- 17 9)
;        8)
;     (= (o- 18 25)
;        -7)
;     "o- works")


(define tup?
  (λ (tup)
    "(-> (Listof S-exp) Bool)"
    "returns true if all elements are numbers"
    (cond
      [(empty? tup) #t]
;      [(not (number? (car tup))) #f] ; replaced with "and" version
      [else (and (number? (car tup))
                 (tup? (cdr tup)))])))
;(and (eq? (tup? '(2 11 3 79 47 6))
;          #t)
;     (eq? (tup? '(8 55 5 555))
;          #t)
;     (eq? (tup? '(1 2 8 apple 4 3))
;          #f)
;     (eq? (tup? '(3 (7 4) 13 9))
;          #f)
;     (eq? (tup? '())
;          #t)
;     "tup? works")


(define addtup
  (λ (tup)
    "(-> (Listof Integer) Integer)"
    "returns the sum of the list of numbers"
    (cond
      [(empty? tup) 0]
      [else (o+ (car tup) (addtup (cdr tup)))])))
(eq? (addtup '(3 5 2 8))
     18)
(eq? (addtup '(15 6 7 12 3))
     43)