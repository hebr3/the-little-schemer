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
      [else (or (equal? a (car lat))
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
;(and (eq? (addtup '(3 5 2 8))
;          18)
;     (eq? (addtup '(15 6 7 12 3))
;          43)
;     "addtup works")

(define o*
  (λ (n m)
    "(-> Integer Integer Integer)"
    "returns n*m"
    (cond
      [(zero? m) 0]
      [else (o+ n (o* n (sub1 m)))])))
;(and (= (o* 5 3)
;        15)
;     (= (o* 13 4)
;        52)
;     (= (o* 12 3)
;        36)
;     "o* works")

(define tup+
  (λ (tup1 tup2)
    "(-> (Listof Integer) (Listof Integer) (Listof Integer))"
    "returns a list of the sum of pairwise elements in two lists"
    (cond
      ;[(and (empty? tup1) (empty? tup2)) '()] ; not needed
      [(empty? tup1) tup2]
      [(empty? tup2) tup1]
      [else (cons (o+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))])))
;(and (equal? (tup+ '(3 6 9 11 4)
;                   '(8 5 2 0 7))
;             '(11 11 11 11 11))
;     (equal? (tup+ '(2 3)
;                   '(4 6))
;             '(6 9))
;     (equal? (tup+ '(3 7)
;                   '(4 6))
;             '(7 13))
;     (equal? (tup+ '(3 7)
;                   '(4 6 8 1))
;             '(7 13 8 1))
;     "tup+ works")


(define o>
  (λ (n m)
    "(-> Integer Integer Boolean)"
    "returns true if n>m"
    (cond
      [(zero? n) #f]
      [(zero? m) #t]
      [else (o> (sub1 n) (sub1 m))])))
;(and (eq? (o> 12 133)
;          #f)
;     (eq? (o> 120 11)
;          #t)
;     "o> works")


(define o<
  (λ (n m)
    "(-> Integer Integer Boolean)"
    "returns true if n<m"
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (o< (sub1 n) (sub1 m))])))
;(and (eq? (o< 4 6)
;          #t)
;     (eq? (o< 8 3)
;          #f)
;     (eq? (o< 6 6)
;          #f)
;     "o< works")


(define o=
  (λ (n m)
    ""
    ""
;    (cond
;      [(zero? m) (zero? n)]
;      [(zero? n) #f]
;      [else (o= (sub1 n) (sub1 m))])))
    (cond
;      [(o> n m) #f]
;      [(o< n m) #f]
      [(or (o> n m) (o< n m)) #f]
      [else #t])))
;(and (eq? (o= 3 3)
;          #t)
;     (eq? (o= 5 4)
;          #f)
;     "o= works")


(define o^
  (λ (n m)
    "(-> Integer Integer Integer)"
    "returns n^m"
    (cond
      [(zero? m) 1]
      [else (o* n (o^ n (sub1 m)))])))
;(and (= (o^ 1 1)
;        1)
;     (= (o^ 2 3)
;        8)
;     (= (o^ 5 3)
;        125)
;     "o^ works")


(define quot
  (λ (n m)
    "(-> Integer Integer Integer)"
    "returns the quotient of n m"
    (cond
      [(o< n m) 0]
      [else (add1 (quot (o- n m) m))])))
;(and (= (quot 15 4)
;        3)
;     (= (quot 9 3)
;        3)
;     "quot works")


(define olength
  (λ (lat)
    "(-> (Listof atoms) Integer)"
    "returns the length of the argument"
    (cond
      [(empty? lat) 0]
      [else (add1 (olength (cdr lat)))])))
;(and (= (olength '(hotdogs with mustard sauerkraut and pickles))
;        6)
;     (= (olength '(ham and cheese on rye))
;        5)
;     "olength works")


(define pick
  (λ (n lat)
    "(-> Integer (Listof atom) atom)"
    "returns the nth element of lat"
    (cond
      [(empty? lat) '()]
      [(zero? n) '()]
      [(o= n 1) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))
;(and (eq? (pick 4 '(lasagna spaghetti ravioli
;                            macaroni meatball))
;          'macaroni)
;     (eq? (pick 0 '(a))
;          '())
;     "pick works")


(define rempick
  (λ (n lat)
    "(-> Integer (Listof atom) (Listof atom))"
    "returns the list without the nth element"
    (cond
      [(empty? lat) '()]
      [(zero? (sub1 n)) (cdr lat)]
      ;[(one? n) (cdr lat)] ; cannot reference before def
      [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))
;(and (equal? (rempick 3 '(hotdogs with hot mustard))
;             '(hotdogs with mustard))
;     "rempick works")


(define no-nums
  (λ (lat)
    "(-> (Listof atom) (Listof atom))"
    "returns the list with all numbers removed"
    (cond
      [(empty? lat) '()]
      [(number? (car lat)) (no-nums (cdr lat))]
      [else (cons (car lat) (no-nums (cdr lat)))])))
;(and (equal? (no-nums '(5 pears 6 prunes 9 dates))
;             '(pears prunes dates))
;     "no-nums works")


(define all-nums
  (λ (lat)
    "(-> (Listof atom) (Listof atom))"
    "returns the list with all non-numbers removed"
    (cond
      [(empty? lat) '()]
      [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
      [else (all-nums (cdr lat))])))
;(and (equal? (all-nums '(5 pears 6 prunes 9 dates))
;             '(5 6 9))
;     "all-nums works")


(define eqan?
  (λ (a1 a2)
    "(-> atom atom Boolean)"
    "returns true if a1 and a2 are the same atom"
    (cond
      [(and (number? a1) (number? a2)) (o= a1 a2)]
      [(or (number? a1) (number? a2)) #f]
      [else (eq? a1 a2)])))
;(and (eq? (eqan? 2 2)
;          #t)
;     (eq? (eqan? 2 'b)
;          #f)
;     (eq? (eqan? 'b 'b)
;          #t)
;     (eq? (eqan? 'a 'b)
;          #f)
;     "eqan? works")


(define occur
  (λ (a lat)
    "(-> atom (Listof atom) Integer)"
    (cond
      [(empty? lat) 0]
      [(eqan? a (car lat)) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))])))
;(and (o= (occur 'a '(1 2 a 4 a 5 b))
;         2)
;     (o= (occur 2 '(a b 2 d e 2 g 2 i))
;         3)
;     "occur works")


(define one?
  (λ (n)
    "(-> Integer Boolean)"
    "returns true if n = 1"
;    (cond
;      [(zero? n) #f]
;      [else (zero? (sub1 n))])))
    (o= n 1)))                     ; much cleaner 
;(and (eq? (one? 1)
;          #t)
;     (eq? (one? 2)
;          #f)
;     "one? works")

;; Chapter 5
(define rember*
  (λ (a l)
    "(-> atom (Listof sexp) (Listof sexp))"
    "returns the list with all atoms removed"
    (cond
      [(empty? l) '()]
      [(atom? (car l))
       (cond
         [(eqan? a (car l)) (rember* a (cdr l))]
         [else (cons (car l) (rember* a (cdr l)))])]
      [else (cons (rember* a (car l))
                  (rember* a (cdr l)))])))
;(and (equal? (rember* 'cup '((coffee)
;                             cup
;                             ((tea) cup)
;                             (and (hick))
;                             cup))
;             '((coffee) ((tea)) (and (hick))))
;     (equal? (rember* 'sauce '(((tomato sauce))
;                               ((bean) sauce)
;                               (and ((flying)) sauce)))
;             '(((tomato))
;               ((bean))
;               (and ((flying)))))
;     "rember* works")


(define insertR*
  (λ (new old l)
    "(-> sexp sexp (Listof sexp) (listof sexp))"
    "returns the list with new inserted after each old"
    (cond
      [(empty? l) '()]
;      [(atom? (car l))
;       (cond
;         [(eqan? old (car l))
;          (cons old (cons new (insertR* new old (cdr l))))]
;         [else (cons (car l) (insertR* new old (cdr l)))])]
;      [else (cons (insertR* new old (car l))
;                  (insertR* new old (cdr l)))]))
      [(atom? (car l))
       (cons (car l)
             (if (eq? old (car l))
                 (cons new (insertR* new old (cdr l)))
                 (insertR* new old (cdr l))))]
      [(list? (car l))                       ; a little better
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))])))
;(and (equal? (insertR* 'roast 'chuck '((how much (wood))
;                                       could
;                                       ((a (wood) chuck))
;                                       (((chuck)))
;                                       (if (a) ((wood chuck)))
;                                       could chuck wood))
;             '((how much (wood))
;               could
;               ((a (wood) chuck roast))
;               (((chuck roast)))
;               (if (a) ((wood chuck roast)))
;               could chuck roast wood))
;     "insertR* works")


(define occur*
  (λ (a l)
    "(-> atom (Listof sexp) Integer)"
    "returns the recursive count of atom in (Listof sexp)"
    (cond
      [(empty? l) 0]
;      [(atom? (car l))
;       (cond
;         [(eq? a (car l))
;          (add1 (occur* a (cdr l)))]
;         [else (occur* a (cdr l))])]
      [(atom? (car l))
       (if (eq? a (car l))
           (add1 (occur* a (cdr l)))
           (occur* a (cdr l)))]
      [else (+ (occur* a (car l))
               (occur* a (cdr l)))])))
;(and (= (occur* 'banana '((banana)
;                          (split ((((banana ice)))
;                                  (cream (banana))
;                                  sherbet))
;                          (banana)
;                          (bread)
;                          (banana brandy)))
;        5)
;     "occur* works")


(define subst*
  (λ (new old l)
    "(-> atom atom (Listof sexp) (Listof sexp))"
    "replace all old with new in the argument"
    (cond
      [(empty? l) '()]
;      [(atom? (car l))
;       (cond
;         [(eq? old (car l))
;          (cons new (subst* new old (cdr l)))]
;         [else
;          (cons (car l) (subst* new old (cdr l)))])]
      [(atom? (car l))
       (cons (if (eq? (car l) old) new (car l))
             (subst* new old (cdr l)))]
      [else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))])))
;(and (equal? (subst* 'orange 'banana '((banana)
;                                       (split ((((banana ice)))
;                                               (cream (banana))
;                                               sherbet))
;                                       (banana)
;                                       (bread)
;                                       (banana brandy)))
;             '((orange)
;               (split ((((orange ice)))
;                       (cream (orange))
;                       sherbet))
;               (orange)
;               (bread)
;               (orange brandy)))
;     "subst* works")


(define insertL*
  (λ (new old l)
    "(-> atom atom (Listof sexp) (Listof sexp))"
    "returns a (Listof sexp) with new to the left of every old"
    (cond
      [(empty? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l))
          (cons new (cons old (insertL* new old (cdr l))))]
         [else (cons (car l) (insertL* new old (cdr l)))])]
      [else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))])))
;(and (equal? (insertL* 'pecker 'chuck '((how much (wood))
;                                        could
;                                        ((a (wood) chuck))
;                                        (((chuck)))
;                                        (if (a) ((wood chuck)))
;                                        could chuck wood))
;             '((how much (wood))
;               could
;               ((a (wood) pecker chuck))
;               (((pecker chuck)))
;               (if (a) ((wood pecker chuck)))
;               could pecker chuck wood))
;     "insertL* works")


(define member*
  (λ (a l)
    "(-> atom (Listof sexp) Boolean)"
    "returns true if atom is in the (Listof sexp)"
    (cond
      [(empty? l) #f]
      [(atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l)))]
      [else (or (member* a (car l))
                (member* a (cdr l)))])))
;(and (eq? (member* 'chips '((potato) (chips ((with) fish) (chips))))
;          #t)
;     "member* works")


(define leftmost
  (λ (l)
    (cond
      [(atom? (car l)) (car l)]
      [else (leftmost (car l))])))
;(and (eq? (leftmost '((a b) c d))
;          'a)
;     (eq? (leftmost '(b))
;          'b)
;     "leftmost works")


(define eqlist?
  (λ (l1 l2)
    "(-> (Listof sexp) (Listof sexp) Boolean)"
    "returns true if both list are the same"
    (cond
      [(and (empty? l1) (empty? l2)) #t]
      [(or (empty? l1) (empty? l2)) #f]
      [(and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))]
      [(or (atom? (car l1)) (atom? (car l2)))
       #f]
      [else (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))])))
;    (cond
;      [(and (empty? l1) (empty? l2)) #t]
;      [(and (empty? l1) (atom? (car l2))) #f]
;      [(empty? l1) #f]
;      [(and (atom? (car l1)) (empty? l2)) #f]
;      [(and (atom? (car l1)) (atom? (car l2)))
;       (and (eqan? (car l1) (car l2))
;            (eqlist? (cdr l1) (cdr l2)))]
;      [(atom? (car l1)) #f]
;      [(empty? l2) #f]
;      [(atom? (car l2)) #f]
;      [else (and (eqlist? (car l1) (car l2))
;                 (eqlist? (cdr l1) (cdr l2)))])))
;(and (eq? (eqlist? '(strawberry ice cream)
;                   '(strawberry ice cream))
;          #t)
;     (eq? (eqlist? '(strawberry ice cream)
;                   '(strawberry cream ice))
;          #f)
;     (eq? (eqlist? '(banana ((split)))
;                   '((banana) (split)))
;          #f)
;     (eq? (eqlist? '(beef ((sausage)) (and (soda)))
;                   '(beef ((salami)) (and (soda))))
;          #f)
;     (eq? (eqlist? '(beef ((sausage)) (and (soda)))
;                   '(beef ((sausage)) (and (soda))))
;          #t)
;     "eqlist? works")


(define oequal?
  (λ (s1 s2)
    "(-> sexp sexp Bool)"
    "returns if s1 and s2 are equal"
    (cond
      [(and (atom? s1) (atom? s2))
       (eqan? s1 s2)]
      [(or (atom? s1) (atom? s2)) #f]
      [else (eqlist? s1 s2)])))

;; Chapter 6

(define numbered?
  (λ (aexp)
    "(-> sexp Bool)"
    "returns true if the sexp is a number"
    "assumes the cadr is one of o+ o* o^"
    (cond
      [(atom? aexp)
       (number? aexp)]
;      [(eq? (car (cdr aexp)) 'o+)
;       (and (numbered? (car aexp))
;            (numbered? (caddr aexp)))]
;      [(eq? (car (cdr aexp)) 'o*)
;       (and (numbered? (car aexp))
;            (numbered? (caddr aexp)))]
;      [(eq? (car (cdr aexp)) 'o^)
;       (and (numbered? (car aexp))
;            (numbered? (caddr aexp)))])))
      [else (and (numbered? (car aexp))
                 (numbered? (caddr aexp)))])))
;(and (eq? (numbered? '(3 o+ (4 o^ 5)))
;          #t)
;     "numbered? works")

(define value
  (λ (nexp)
  "(-> nexp Integer)"
  "calculates the value of an arithmetic expression"
    (cond
      [(atom? nexp) nexp]
      [(eq? (cadr nexp) 'o+)
       (o+ (value (car nexp))
          (value (caddr nexp)))]
      [(eq? (cadr nexp) 'o*)
       (o* (value (car nexp))
          (value (caddr nexp)))]
      [else
       (o^ (value (car nexp))
           (value (caddr nexp)))])))
;(and (eq? (value 13)
;          13)
;     (eq? (value '(1 o+ 3))
;          4)
;     (eq? (value '(1 o+ (3 o^ 4)))
;          82)
;     "value works")

(define 1st-sub-exp
  (λ (aexp)
    "(-> aexp aexp)"
    "returns the first sub exp"
    (car (cdr aexp)))) ; cadr
(define 2nd-sub-exp
  (λ (aexp)
    "(-> aexp aexp)"
    "returns the second sub exp"
    (car (cdr (cdr aexp))))) ; caddr
(define operator ; car)
  (λ (aexp)
    (car aexp)))
(define value2
  (λ (nexp)
    "(-> nexp Integer)"
    "calculates the value of an arithmetic expression"
    (cond
      [(atom? nexp) nexp]
      [(eq? (operator nexp) 'o+)
       (o+ (value2 (1st-sub-exp nexp))
           (value2 (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) 'o*)
       (o* (value2 (1st-sub-exp nexp))
           (value2 (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) 'o^)
       (o^ (value2 (1st-sub-exp nexp))
           (value2 (2nd-sub-exp nexp)))])))
;(and (eq? (value2 '(o+ 3 4))
;          7)
;     (eq? (value2 '(o+ 1 3))
;          4)
;     "value2 works")

(define sero?
  (λ (n)
    "(-> rep Bool)"
    "0 = '()"
    (empty? n)))
;(and (eq? (sero? '())
;          #t)
;     (eq? (sero? '(()))
;          #f)
;     "sero? works")

(define edd1
  (λ (n)
    "(-> (Listof empty) (Listof empty))"
    "returns a list with '() cons to the front"
    (cons '() n)))
;(and (equal? (edd1 '())
;             '(()))
;     (equal? (edd1 '(()))
;             '(() ()))
;     "edd1 works")

(define zub1
  (λ (n)
    "(-> (Listof empty) (Listof empty))"
    "returns the cdr of a list"
    (cdr n)))
;(and (equal? (zub1 '(()))
;             '())
;     (equal? (zub1 '(() ()))
;             '(()))
;     "zub1 works")

(define u+
  (λ (n m)
    (cond
      [(sero? m) n]
      [else (edd1 (u+ n (zub1 m)))])))
;(and (equal? (u+ '() '(()))
;             '(()))
;     (equal? (u+ '(()) '(()))
;             '(() ()))
;     (equal? (u+ '(() ()) '(() ()))
;             '(() () () ()))
;     "u+ works")

;; Chapter 7

(define oset?
  (λ (lat)
    "(-> (Listof atom) Bool)"
    "returns true if no element occurs twice"
    (cond
      [(empty? lat) #t]
      [(member? (car lat) (cdr lat)) #f]
      [else (oset? (cdr lat))]))) ; ])))
;(and (eq? (oset? '(apple peaches apple plum))
;          #f)
;     (eq? (oset? '(apples peaches pears plums))
;          #t)
;     (eq? (oset? '())
;          #t)
;     (eq? (oset? '(apple 3 pear 4 9 apple 3 4))
;          #f)
;     "oset? works")

(define makeset
  (λ (lat)
    "(-> (Listof atoms) (Listof atoms))"
    "returns a set from two lists of atoms"
    (cond
      [(empty? lat) '()]
;      [(member? (car lat) (cdr lat))
;       (makeset (cdr lat))]
;      [else (cons (car lat)
;                  (makeset (cdr lat)))])))
      [else (cons (car lat)
                  (makeset (multirember (car lat) (cdr lat))))])))
;(and (equal? (makeset '(apple peach pear peach plum apple lemon peach))
;             '(apple peach pear plum lemon))
;     (equal? (makeset '(apple 3 pear 4 9 apple 3 4))
;             '(apple 3 pear 4 9))
;     "makeset works")

(define subset?
  (λ (set1 set2)
    "(-> (Listof atoms) (Listof atoms) Boolean)"
    "returns true if set1 is a subset of set2"
    (cond
      [(empty? set1) #t]
      [else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))])))
;(and (eq? (subset? '(5 chicken wings)
;                   '(5 hamburgers
;                     2 pieces fried chicken and
;                     light duckling wings))
;          #t)
;     (eq? (subset? '(4 pounds of horseradish)
;                   '(four pounds of chickend and
;                     5 ounces horseradish))
;          #f)
;     "subset? works")

(define eqset?
  (λ (set1 set2)
    "(-> (Listof atoms) (Listof atoms) Boolean)"
    "returns true if set1 = set2"
    (and (subset? set1 set2)
         (subset? set2 set1))))
;    (cond
;      [(and (empty? set1) (empty? set2)) #t]
;      [(or (empty? set1) (empty? set2)) #f]
;      [else (and (member? (car set1) set2)
;                 (eqset? (rember (car set1) set1)
;                         (rember (car set1) set2)))])))
;(and (eq? (eqset? '(6 large chickens with wings)
;                  '(6 chickens with large wings))
;          #t)
;     "eqset? works")

(define intersect?
  (λ (set1 set2)
    "(-> (Listof atoms) (Listof atoms) Boolean)"
    "return true if at least one element in set1 is in set2"
    (cond
      [(empty? set1) #t]
      [else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))])))
;(and (eq? (intersect? '(stewed tomatoes and macaroni)
;                     '(macaroni and cheese))
;          #t)
;     "intersect? works")

(define intersect
  (λ (set1 set2)
    "(-> (Listof atoms) (Listof atoms) (Listof atoms))"
    "returns a list of atoms that are in both sets"
    (cond
      [(empty? set1) '()]
      [(member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))
;(and (equal? (intersect '(stewed tomatoes and macaroni)
;                        '(macaroni and cheese))
;             '(and macaroni))
;     "intersect works")

(define union
  (λ (set1 set2)
    "(-> (Listof atoms) (Listof atoms) (Listof atoms))"
    "returns the union of the two lists"
    (cond
      [(empty? set1) set2]
      [(member? (car set1) set2) (union (cdr set1) set2)]
      [else (cons (car set1)
                  (union (cdr set1) set2))])))
;(and (equal? (union '(stewed tomatoes and macaroni casserole)
;                    '(macaroni and cheese))
;             '(stewed tomatoes casserole macaroni and cheese))
;     "union works")

(define set-diff ;xxx
  (λ (set1 set2)
    "(-> (Listof atoms) (Listof atoms) (Listof atoms))"
    "returns set1 without the elements that are in set2"
    (cond
      [(empty? set1) '()]
      [(member? (car set1) set2) (set-diff (cdr set1) set2)]
      [else (cons (car set1)
                  (set-diff (cdr set1) set2))])))
;(and (equal? (set-diff '(a b c d e)
;                       '(c d))
;             '(a b e))
;     "set-diff works")

(define intersectall
  (λ (l-set)
    "(-> (Listof (Listof atoms)) (Listof atoms))"
    "returns the intersection of all sub-lists in l-set"
    (cond
      [(empty? (cdr l-set)) (car l-set)]
      [else (intersect (car l-set)
                       (intersectall (cdr l-set)))])))
;(and (equal? (intersectall '((a b c) (c a d e) (e f g h a b)))
;             '(a))
;     (equal? (intersectall '((6 pears and)
;                             (3 peaches and 6 peppers)
;                             (8 pears and 6 plums)
;                             (and 6 prunes with some apples)))
;             '(6 and))
;     "intersectall works")


(define opair?
  (λ (x)
    "(-> (Listof atoms) Boolean)"
    "returns true if the list of atoms is a pair"
    (cond
      [(empty? x) #f]
      [(atom? x) #f]
      [(empty? (cdr x)) #f]
      [(empty? (cddr x)) #t]
      [else #f])))
;(and (eq? (opair? '(pear pear))
;          #t)
;     (eq? (opair? '(3 7))
;          #t)
;     (eq? (opair? '((2) (pair)))
;          #t)
;     (eq? (opair? '(full (house)))
;          #t)
;     "opair? works")


(define fst car)
;  (λ (p)
;    "(-> (Listof atom) atom)"
;    "returns the car of a list"
;    (car p)))
;(and (eq? (fst '(1 2))
;          1)
;     (eq? (fst '(apple pear))
;          'apple)
;     (equal? (fst '((1 2) (3 4)))
;             '(1 2))
;     "fst works")


(define snd cadr)
;  (λ (p)
;    "(-> (Listof atom) atom)"
;    "returns the cadr of a list"
;    (cadr p)))
;(and (eq? (snd '(1 2))
;          2)
;     (eq? (snd '(apple pear))
;          'pear)
;     (equal? (snd '((1 2) (3 4)))
;          '(3 4))
;     "snd works")


(define build
  (λ (s1 s2)
    "(-> atom atom (Pairof atoms)"
    "returns a pair made of the two atoms"
    (cons s1 (cons s2 '()))))
;(and (equal? (build 1 2)
;             '(1 2))
;     (equal? (build 'apple 'pear)
;             '(apple pear))
;     (equal? (build '(1 2) '(3 4))
;             '((1 2) (3 4)))
;     "build works")


(define thd caddr)
;  (λ (l)
;  "(-> (Listof atom) atom)"
;  "returns the third element"
;    (car (cdr (cdr l)))))


(define rel?
  (λ (l)
    "(-> (Listof sexp) Boolean)"
    "returns true if all elements are pairs"
    (cond
      [(empty? l) #t]
      [else (and (opair? (car l))
                 (oset? l)
                 (rel? (cdr l)))])))
;(and (eq? (rel? '(apples peaches pumpkin pie))
;          #f)
;     (eq? (rel? '((apples peaches)
;                  (pumpkin pie)
;                  (apples peaches)))
;          #f)
;     (eq? (rel? '((apples peaches) (pumpkin pie)))
;          #t)
;     (eq? (rel? '((4 3) (4 2) (7 6) (6 2) (3 4)))
;          #t)
;     "rel? works")


(define fun?
  (λ (rel)
    "(-> rel Boolean)"
    "returns true if the pairs discribe a function"
    "horizontal line rule"
    (oset? (firsts rel))))
;(and (eq? (fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))
;          #f)
;     (eq? (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
;          #t)
;     "fun? works")

(define revpair
  (λ (pair)
    (build (snd pair) (fst pair))))
;(and (equal? (revpair '(1 2))
;             '(2 1))
;     (equal? (revpair '(apple pear))
;             '(pear apple))
;     (equal? (revpair '((1 2) (3 4)))
;             '((3 4) (1 2)))
;     "revpair works")


(define revrel
  (λ (rel)
    "(-> rel rel)"
    "returns a rel with the pairs reversed"
    (cond
      [(empty? rel) '()]
;      [else (cons (build (snd (car rel))
;                         (fst (car rel)))
;                  (revrel (cdr rel)))])))
      [else (cons (revpair (car rel))
                  (revrel (cdr rel)))])))
;(and (equal? (revrel '((8 a) (pumpkin pie) (got sick)))
;             '((a 8) (pie pumpkin) (sick got)))
;     "revrel works")


(define seconds
  (λ (l)
    "(-> (Listof pairs) (Listof atoms)"
    "returns a list of the snd's of the argument"
    (cond
      [(empty? l) '()]
      [else (cons (snd (fst l))
                  (seconds (cdr l)))])))
;(and (equal? (seconds '((1 2) (3 4) (5 6)))
;             '(2 4 6))
;     (equal? (seconds '((grape raisin)
;                        (plum prune)
;                        (stewed grape)))
;             '(raisin prune grape))
;     "seconds works")


(define fullfun?
  (λ (fun)
    "(-> fun Boolean)"
    "returns true if no snd appears twice"
    ;(oset? (firsts (revrel fun)))))
    (oset? (seconds fun))))
;(and (eq? (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
;          #f)
;     (eq? (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
;          #t)
;     (eq? (fullfun? '((grape raisin)
;                      (plum prune)
;                      (stewed prune)))
;          #f)
;     (eq? (fullfun? '((grape raisin)
;                     (plum prune)
;                     (stewed grape)))
;          #t)
;     "fullfun? works")


(define one-to-one?
  (λ (fun)
    (and (fun? fun)
         (fun? (revrel fun)))))
;(and (eq? (one-to-one? '((1 2) (3 4) (5 6)))
;          #t)
;     (eq? (one-to-one? '((1 2) (1 3)))
;          #f)
;     (eq? (one-to-one? '((1 2) (3 2)))
;          #f)
;     (eq? (one-to-one? '((chocolate chip) (doughy cookie)))
;          #t)
;     "one-to-one? works")


;; Chapter 8
;; Lambda the Ultimate

(define rember-f
;  (λ (test? a l)
  (λ (test?)
    (λ (a l)
      "(-> (-> sexp Bool) atom (Listof atoms))"
      "return a list where every (test? atom) is removed if true"
      (cond
        [(empty? l) '()]
        [(test? a (car l)) (cdr l)]
        [else
         (cons (car l)
               ((rember-f test?) a (cdr l)))]))))
;(and (equal? ((rember-f o=) 5 '(6 2 5 3))
;             '(6 2 3))
;     (equal? ((rember-f eq?) 'jelly '(jelly beans are good))
;             '(beans are good))
;     (equal? ((rember-f equal?) '(pop corn) '(lemonade
;                                            (pop corn)
;                                            and
;                                            (cake)))
;             '(lemonade and (cake)))
;     "rember-f works")

(define rember-= (rember-f o=))
;  (λ (a l)
;    ((rember-f o=) a l)))

(define rember-eq? (rember-f eq?))
;  (λ (a l)
;    ((rember-f eq?) a l)))
;(and (equal? (rember-eq? 'tuna '(tuna salid is good))
;             '(salid is good))
;     (equal? (rember-eq? 'tuna '(shrimp salad and tuna salad))
;             '(shrimp salad and salad))
;     (equal? (rember-eq? 'eq? '(equal? eq? eqan? eqlist? eqpair?))
;             '(equal? eqan? eqlist? eqpair?))
;     "rember-eq? works")

(define rember-equal? (rember-f equal?))
;  (λ (a l)
;    ((rember-f equal?) a l)))


(define eq?-c
  (λ (a)
    (λ (x)
      (eq? x a))))
;(and (eq? ((eq?-c 2) 2)
;          #t)
;     (eq? ((eq?-c 3) 2)
;          #f)
;     "eq?-c works")


(define eq?-salad (eq?-c 'salad))
;(and (eq? (eq?-salad 'salad)
;          #t)
;     (eq? (eq?-salad 'tuna)
;          #f)
;     (eq? ((eq?-c 'salad) 'tuna)
;          #f)
;     "eq?-salad works")

(define seqL
  (λ (new old l)
    (cons new (cons old l))))
(define seqR
  (λ (new old l)
    (cons old (cons new l))))

(define insert-g
  (λ (seq)
    (λ (new old l)
      (cond
        [(empty? l) '()]
        [(eq? old (car l))
         (seq new old (cdr l))]
        [else (cons (car l)
                    ((insert-g seq) new old cdr l))]))))

(define insertL-f
  (λ (test?)
    (λ (new old l)
      (cond
        [(empty? l) '()]
        [(test? old (car l))
         (cons new (cons old (cdr l)))]
        [else (cons (car l)
                    ((insertL-f test?) new old (cdr l)))]))))

(define insertR-f
  (λ (test?)
    (λ (new old l)
      (cond
        [(empty? l) '()]
        [(test? old (car l))
         (cons old (cons new (cdr l)))]
        [else (cons (car l)
                    ((insertL-f test? new old (cdr l))))]))))

(define insertL2 ;(insert-g seqL))
  (insert-g
   (λ (new old l)
     (cons new (cons old l)))))
;(and (equal? (insertL 'a 'b '(b c d b))
;             (insertL2 'a 'b '(b c d b)))
;     "insertL2 works")
(define insertR2 ;(insert-g seqR))
  (insert-g
   (λ (new old l)
     (cons old (cons new l)))))
;(and (equal? (insertR 'a 'b '(b c d b))
;             (insertR2 'a 'b '(b c d b)))
;     "insertR2 works")