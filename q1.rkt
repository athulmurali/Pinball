;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; q1.rkt;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")
(check-location "06" "q1.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide
 inner-product
 permutation-of?
 shortlex-less-than?
 permutations)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; inner-product : RealList RealList -> Real
;;; GIVEN: two lists of real numbers
;;; WHERE: the two lists have the same length
;;; RETURNS: the inner product of those lists
;;           i.e the sum of each pair of same index


;; STRATEGY : Using HOF foldr on list1 and list2 with function +

;;; EXAMPLES:
;;;     (inner-product (list 2.5) (list 3.0))  =>  7.5
;;;     (inner-product (list 1 2 3 4) (list 5 6 7 8))  =>  70
;;;     (inner-product (list) (list))  =>  0


;; **************** Function definition*************************

(define (inner-product real-list-1 real-list-2)
  (  foldr + 0
           (mapped-product-of-lists real-list-1 real-list-2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mapped-product-of-lists:  Real Real => Real 

;; GIVEN   :  li1, RealList, represents a list of real numbers
;;            li2, RealList, represents a list of real numbers,
;;            of same length as li1

;; RETURNS : a list containing a product of each pair of same index
;;           consecutively from the two list given

;; STRATEGY : using HOF map  on lists li1 li2 with function  *

;; EXAMPLES;
;;(mapped-product-of-lists (list 0 1 2) (list 100 10 10)) => (list 0 10 20)

;; ***************************FUNCTION DEFINITION*************


(define ( mapped-product-of-lists li1 li2)
  (map * li1 li2))

;;****************************TESTS***************************
(begin-for-test
  (check-equal?
   (mapped-product-of-lists (list 0 1 2) (list 100 10 10))
   (list 0 10 20))
  "It must be (list 0 10 20")
;; *********************TESTS*******************
(begin-for-test
  (check-equal?
   (inner-product null null)
   0 "It has to be 0 even if 1 of the list is empty")
  (check-equal?
   (inner-product (list 1 1 1) (list 2 2 2))
   6 "It has to be 6 ")
  (check-equal?
   (inner-product  (list 1 1 1) (list 3 4 5))
   12 "It has to be 6 ")
  ;; Test cases from examples below this---
  (check-equal?
   (inner-product  (list 2.5) (list 3.0))
   7.5 "It has to be 7.5 or 15/2 ")

  (check-equal?
   (inner-product  (list 1 2 3 4) (list 5 6 7 8))
   70 "It has to be 70")

  (check-equal?
   (inner-product  (list ) (list))
   0 "It has to be 0 for empty lists"))


;; ==================================================================

;;; permutation-of? : IntList IntList -> Boolean
;;; GIVEN: two lists of integers
;;; WHERE: neither list contains duplicate elements
;;; RETURNS: true if and only if one of the lists
;;;     is a permutation of the other
;;; EXAMPLES:
;;;     (permutation-of? (list 1 2 3) (list 1 2 3)) => true
;;;     (permutation-of? (list 3 1 2) (list 1 2 3)) => true
;;;     (permutation-of? (list 3 1 2) (list 1 2 4)) => false
;;;     (permutation-of? (list 1 2 3) (list 1 2)) => false
;;;     (permutation-of? (list) (list)) => true


;; ***********************FUNCTION DEFINITION ************************
(define (permutation-of? list1 list2)
  (if(not
      ( = (length list1)   (length list2))) 
     false
     ;;else
     (or
      ( equal? list1 (sort list2 < ))
      ( equal? list2 (sort list1 < )))))
;; *************TESTS***********************************************
(begin-for-test
  ;; checking for different lengths
  (check-equal?
   (permutation-of? (list 1 2) (list 1 2 3))
   false "Must be false, list lengths are different")
  (check-equal?
   (permutation-of? (list 1 2 3) (list 1 2 3))
   true "The list are same hence permuation of each other")
  (check-equal?
   (permutation-of? (list 3 1 2) (list 1 2 3))
   true "The list are same hence permuation of each other"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
          
;;; shortlex-less-than? : IntList IntList -> Boolean
;;; GIVEN: two lists of integers
;;; RETURNS: true if and only either
;;;     the first list is shorter than the second
;;;  or
;;       both are non-empty,
;;       have the same length,
;;      and either
;;         or
;;;         the first element of the first list is less than
;;;                  the first element of the second list
;;;       the first elements are equal, and the rest of
;;;             the first list is less than the rest of the
;;;             second list according to shortlex-less-than?
;;; EXAMPLES:
;;;     (shortlex-less-than? (list) (list)) => false
;;;     (shortlex-less-than? (list) (list 3)) => true
;;;     (shortlex-less-than? (list 3) (list)) => false
;;;     (shortlex-less-than? (list 3) (list 3)) => false
;;;     (shortlex-less-than? (list 3) (list 1 2)) => true
;;;     (shortlex-less-than? (list 3 0) (list 1 2)) => false
;;;     (shortlex-less-than? (list 0 3) (list 1 2)) => true

;; ********************FUNCTION DEFINITION ****************************
(define  (shortlex-less-than? list1 list2)
  (cond
    ((< (length list1) (length list2))  true)
    ((= (length list1) (length list2))
     (cond
       (( = (length list1) 0 )              false)
       (( > ( first list1)(first list2))    false)
       (( < (first list1)(first list2))     true)
       (else (shortlex-less-than? (rest list1)(rest list2)))))
    (else false)))

;; *************************TESTS*************************************
(begin-for-test
  (check-equal?
   (shortlex-less-than? (list) (list ))
   false "If both are empty, it must be false")

  (check-equal?
   (shortlex-less-than? (list) (list 3))
   true "It must be true since length of list1 is lesser than list2")

  (check-equal?
   (shortlex-less-than? (list 3) (list))
   false "It must be false since length of list1 is greater than list2")

  (check-equal?
   (shortlex-less-than? (list 3) (list 3))
   false "It must be false since all the elements are same in both lists")

  (check-equal?
   (shortlex-less-than? (list 3) (list 1 2))
   true "It must be true since length of list1 is lesser than list2")

  (check-equal?
   (shortlex-less-than? (list 3 0) (list 1 2))
   false "It must be false since first element of list1 itself is greater
than the 1st element of list2 ")

  (check-equal?
   (shortlex-less-than? (list 0 3) (list 1 2))
   true "It must be false since first element of list1 itself is lesser
than the 1st element of list2 ")

  (check-equal?
   (shortlex-less-than? (list 3 1) (list 3 2))
   true "It must be true since the 1st non-same element in list1 is greater
than the 1st non-same element in list 2"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
;;; permutations : IntList -> IntListList
;;; GIVEN: a list of integers
;;; WHERE: the list contains no duplicates
;;; RETURNS: a list of all permutations of that list,
;;;     in shortlex order
;;; EXAMPLES:
;;;     (permutations (list))  =>  (list (list))
;;;     (permutations (list 9))  =>  (list (list 9))
;;;     (permutations (list 3 1 2))
;;;         =>  (list (list 1 2 3)
;;;                   (list 1 3 2)
;;;                   (list 2 1 3)
;;;                   (list 2 3 1)
;;;                   (list 3 1 2)
;;;                   (list 3 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; last-index: List => Integer
;; GIVEN       : l, List
;; RETURNS     : the last index of the list,
;;               which can be any value from
;;               0 to  (length of the list given -1)
;;               but returns -1 if an empty list is given as l
;;             
;; STRATEGY   : Transcribing the formula
;; ****************** FUNCTION DEFINITION ********************

(define (last-index l)
  (- (length l) 1))

;;************************************************************

;; set-val-at-ind List NonNegInteger NonNegInteger => List
;; GIVEN       : l, List
;;               ind, NonNegInteger
;;               val  NonNegInteger
;;               
;; RETURNS     : the new list ,
;;               which can be any value from
;;               0 to  (length of the list given -1)
;;               but returns -1 if an empty list is given as l
;;             
;; STRATEGY   : Transcribing the formula

;; Examples:
;;(set-val-at-ind   (list 0 1 2 3 4 5 6 7 8 9 )  5  555) =>
;;   (list 0 1 2 3 4 555 6 7 8 9)
;; ****************** FUNCTION DEFINITION ********************
(define (set-val-at-ind l ind val)
  (append
   (first-n-in-list ind  l (list))
   (list val)
   (last-n-in-list (- (sub1 (length l))ind )  l )))
  

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first-n-in-list : NonNegInteger List =>List
;; GIVEN: n, NonNegInteger , first n elements to be obtained from the list
;;        l, List          , the list from which elements are to be obatined
;; RETURNS: a new list with first n elements

;; STRATEGY : Using cosntructor template of List
(define (first-n-in-list  n l temp-list)
  (if (or (zero? n) (null? l))
      temp-list
      ;;    else
      (first-n-in-list
       (- n 1)
       (rest l)
       (append temp-list (list (first l))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; last-n-in-list : NonNegInteger List =>List
;; GIVEN: n, NonNegInteger , first n elements to be obtained from the list
;;        l, List          , the list from which elements are to be obatined
;;        temp-list, List , the list from which elements are to be obatined

;; RETURNS: a new list with last n elements

;; STRATEGY : Using cosntructor template of List
(define (last-n-in-list  n l  )
  (cond
    ((empty? l) empty)
    ((=   n    0)  empty )
    ((= (length l ) n)  l )
    (else (last-n-in-list n (rest l) )) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (check-equal?
   (last-index (list))
   -1   "Empty list")

  (check-equal?
   (empty? (last-n-in-list  0 empty ))
   true   "Empty list")

  (check-equal?
   (last-index (list 1))
   0   "0 should be the result, as last index is (length of list -1")

  (check-equal?
   (last-index (list 1 2))
   1   "1 should be the result, as last index is (length of list-1)")

  (check-equal?
   (last-index (list 1 2 3 4 5 6 7 8 9 10))
   9   "9 should be the result, as last index is (length of list-1)")

  (check-equal?
   (first-n-in-list 5 (list 1 2 3 4 5 6 7 8 9 10) (list))
   (list 1 2 3 4 5)
   "The first 5 elements in the list must be generated as a new list")

  (check-equal?
   (last-n-in-list  5 (list 1 2 3 4 5 6 7 8 9 10))
   (list 6 7 8 9 10)
   "The last elements 5 in the list must be generated as a new list")


  (check-equal?
   (set-val-at-ind   (list 0 1 2 3 4 5 6 7 8 9 )  5  555)
   (list 0 1 2 3 4 555 6 7 8 9)
   "new value must be updated at the 5th index")

  (check-equal?
   (set-val-at-ind   (list 0 1 2 3 4 5 6 7 8 9)  9  999)
   (list 0 1 2 3 4 5 6 7 8 999)
   "new value must be updated at the 9th index"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; duplicate-exists? : IntegerList Integer Integer Boolean
;; GIVEN: li, List of Integers
;;          i, Integer from -1 to last index of list
;;         j,, Integer from -1 to last index of list
;;RETURNS: IF there are any duplicates

;; STRATEGY : Combining simpler functions
;; EXAMPLES:
;; PLease refer the test cases below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (duplicate-exists?  li i j status)
  (
   cond
    ((= i  (length li))  status)
    ((= j  (length li))  (duplicate-exists?  li (add1 i) 0 status))

    ((and
      (= (list-ref li i)
         (list-ref li j))
      (not(= i j )))      true)

    (else (duplicate-exists?  li i (add1 j) status))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(begin-for-test
  (check-equal?
   (duplicate-exists?  (list 0 1 0) 0 0 true)
   true " 0 exists twice, hence it must be true" )
  (check-equal?
   (duplicate-exists?  (list 0 1 2) 0 0 false)
   false " none of the elements exists twice, hence it must be false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; next-set :IntList Int  Int  Int => IntList

;; GIVEN:       
 
;;index-list   : IntList, the given index list (current index set) 
;;change-index : Int, the index to be changed in the list
;;min-index    : Int, The lower end of the index
;;max-index    : Int, the upper end of the index


;; RETURNS: A next set of index list  (Intlist)

;; STRATEGY: Combining simpler functions

;; Examples:
;;(next-set  (list 0 1 2 )   2    0    2 ) =>
;;    (list 0 2 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define(next-set    index-list change-index min-index  max-index )
  (if (and
       ( >= change-index min-index )
       ( < (list-ref  index-list change-index ) max-index))

      (set-val-at-ind
       index-list
       change-index
       (add1 (list-ref  index-list change-index )))


      ;;      else
      (next-set
       (set-val-at-ind index-list change-index min-index)
       ( sub1  change-index)
       min-index
       max-index)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;next-acceptable-state :IntList Int  Int  Int => IntList

;; GIVEN   :
;;li           : IntList, the given index list (current index set) 
;;change-index : Int, the index to be changed in the list
;;min-index    : Int, The lower end of the index
;;max-index    : Int, the upper end of the index

;; RETURNS : the next acceptable state, (list of index which is accepted)
;; acc. to requirements: like no repetition 

;; STRATEGY : Combining simpler functions 

;; EXAMPLES;
;;   (next-acceptable-state  (list 0 1 2 )   2    0    2 )=>   (list 0 2 1 )


;; ***************************FUNCTION DEFINITION*************

(define  (next-acceptable-state  li  change-ind min-ind max-ind)
  (if (duplicate-exists?
       ( next-set li   change-ind min-ind max-ind )
       0 0 false )

      ( next-acceptable-state
        (next-set  li   change-ind min-ind max-ind )
        change-ind
        min-ind
        max-ind )

      ;;else
      (next-set li   change-ind min-ind max-ind )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gen-ind-lstlst
(define (gen-ind-lstlst  temp-set end-set  list-of-lists)
  (if (equal? end-set temp-set )
      ;;(append  list-of-lists  (list end-set))
      list-of-lists
  
      ;;else
      (gen-ind-lstlst
       ( next-acceptable-state
         temp-set
         ( sub1 (length temp-set) ) 0 ( sub1 (length temp-set )))
       end-set
       (append list-of-lists
               ( list (next-acceptable-state
                       temp-set
                       ( sub1 (length temp-set ))
                       0
                       ( sub1 (length temp-set ))))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (check-equal?
   (next-set
    (list 0 1 2 )   2    0    2 )
   (list 0 2 0)
   "0 2 0 should be the next state")
  (check-equal?
   (next-set
    (list 2 0 0 )   2    0    2 )
   (list  2 0 1)
   "if a set with index above max-index given,
the same list must be returned")
  (check-equal?
   (next-acceptable-state
    (list 0 1 2 )   2    0    2 )
   (list 0 2 1 )
   "(0 2 1) should be the next acceptable state since
(list of 0 2 0 has two zeroes)")
  (check-equal?
   (next-acceptable-state
    (list 2 0 1 )   2   0     2)
   (list 2 1 0 )
   "(0 2 1) should be the next acceptable state since
(list of 0 2 0 has two zeroes)")

  (check-equal?
   (gen-ind-lstlst (list 0 1 2 )
                   (list 2 1 0)
                   (list(list 0 1 2 )))
   (list(list 0 1 2 )(list 0 2 1 )(list 1 0 2 )
        (list 1 2 0)(list 2 0 1 )(list 2 1 0))
   "(0 2 1) should be the next acceptable state since
(list of 0 2 0 has two zeroes)"))
;; ===============================================
;; gen-index-list : Int NonNegInt IntList
;; GIVEN   : i, Int, from -1 to n
;;           n, Int, number of elements to be there in the output list
;;           li,  to be initialized as empty list 

;; RETURNS : li, which is recursively appended to give the output list

;; STRATEGY : Combining simpler functions

;; EXAMPLES;
;; (gen-index-list -1 1 (list)) =>  (list 0)
;; (gen-index-list -1 5 (list)) =>  (list 0 1 2 3 4 )

;; TESTS: After three functions below,(Grouped by relevance)


;; ***************************FUNCTION DEFINITION*************

(define (gen-index-list i n li)
  (if (< (length li) n )
      (gen-index-list
       (add1 i)
       n
       (append li (list (add1 i))))
      ;; else
      li))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; re-ordered-set : IntList IntList List Int => IntList
;;GIVEN:
;; ind-li      : List of integers , based on which the list is to be ordered
;; actual-li   : List of integers, these are the actual numbers in new order
;; given-li    : List of integers, this is the list of given numbers
;;                to be ordered
;; i           : Int, One of the temp variables used inside, but it requires
;;               initialization  -1 

;; Returns     : actual-li, the reorder list of actual integers

;; STRATEGY    : Combining simpler functions

;; EXAMPLES     :
;;   (re-ordered-set ( list  2 1 0) (list) (list 1 2 3)  -1) =>
;;   (list 3 2 1) 

;; ************ FUNCTION DEFINITION
(define (re-ordered-set  ind-li actual-li given-li i)
  (if
   ( = (length given-li) (length actual-li))
   actual-li

   ;; else
   (re-ordered-set
    ind-li
    (append actual-li
            (list( list-ref  given-li (list-ref  ind-li (add1 i)))))
    given-li
    (add1 i) )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ind-to-actual-lstlst  List(IntegerList) List(IntegerList) IntegerList
;;                      => 
;; GIVEN:   ind-lstlst : the reorder list of list of integers (indexes)
;;          actual-lstlst: initiailly this is initialized to empty
;;          input-lst : The list of given integers from the user
;;                       The list to be  in various orders 
;;

;; RETURNS: actual-lstlst, The reorder list of lists of integers
;;                          based on  ind-lstlst

;; STRATEGY: Combining simpler functions

;; Examples:
;;(ind-to-actual-lstlst   ( list (list 0 1 2)(list 1 2 0))
;;                        ( list )
;;                        ( list 11 22 33))
;;                         =>
;;                        (list ( list  11 22 33 )(list  22 33 11 ))


;; **********FUNCTION DEFINITION *********************

(define (ind-to-actual-lstlst  ind-lstlst actual-lstlst input-lst)  
  (if
   ( = (length ind-lstlst) 0 )
   actual-lstlst
   
   ;; else
   (ind-to-actual-lstlst
    ( rest ind-lstlst )
    (append actual-lstlst (list
                           (re-ordered-set
                            (first ind-lstlst)
                            (list)
                            input-lst
                            -1)))
    input-lst)))


;; ========================================

(begin-for-test
  (check-equal?
   (gen-index-list -1 1 (list))
   (list 0)
   "(list 0) should be the output")

  (check-equal?
   (gen-index-list -1 5 (list))
   (list 0 1 2 3 4 )
   "(list 0) should be the output")
  
  (check-equal?
   (re-ordered-set
    ( list  2 1 0)
    (list)
    (list 1 2 3)
    -1)
   (list 3 2 1) "(list 3 2 1 ) must be the answer")

  (check-equal?
   (re-ordered-set
    ( list  2 1 0)
    (list)
    (list  11 22 33)
    -1)
   (list 33 22 11) "(list 33 22 11 ) must be the answer")

  (check-equal?
   (re-ordered-set
    ( list  1   2  0)
    (list)
    (list   11  22 33)
    -1)
   (list  22 33 11)
   "(list  22 33 11 ) must be the answer")

  (check-equal?
   (ind-to-actual-lstlst
    ( list (list 0 1 2)(list 1 2 0))
    ( list )
    ( list 11 22 33))
   (list ( list  11 22 33 )(list  22 33 11 ))
   "The resultant list of list must be based on 1st parameter")

  (check-equal?
   (ind-to-actual-lstlst
    ( list (list 0 1 2)(list 2 1 0))
    ( list )
    ( list 11 22 33))
   (list ( list  11 22 33 )(list  33 22 11 ))
   "The resultant list of list must be based on 1st parameter"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gen-indlst-for-input : IntList=> ListListInt

;; GIVEN   : in-list, the list given
;;               for which the various possible index sequences has to be
;;              generated

;; RETURNS : a list of list of integers, representing the
;;              list of indexes in each list, from which the actual
;;              numbers has to be mirrored in the later functions

;; STRATEGY : COmbining simpler functions

;; EXAMPLES;
;; (gen-indlst-for-input  (list (list 0 1 )) =>
;; ((list 0 1)(list 1 0 ))

;; TEST: Please Scroll down for tests
;; ***************************FUNCTION DEFINITION*************
(define (gen-indlst-for-input in-list)
  (gen-index-list -1 (length in-list) (list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indlstlst-for-input : NonNegativeIntList => NonNegativeIntList

;; GIVEN   :in-list, the list of integers (a list of index) 

;; RETURNS : :in-list, the list of all possible index combinations
;;            applicable to the requirement

;; STRATEGY : Combining simpler functions

;; EXAMPLES;

;;(indlstlst-for-input  (list 0 1 2))
;;   (list (list 0 1 2) (list 0 2 1) (list 1 0 2)
 ;;        (list 1 2 0) (list 2 0 1) (list 2 1 0))

;; TESTS:
;; Please refer the test cases below 
;; ***************************FUNCTION DEFINITION*************

(define (indlstlst-for-input in-list)
  (gen-ind-lstlst
   (gen-indlst-for-input in-list)
   (reverse (gen-indlst-for-input in-list))
   (list ( gen-indlst-for-input  in-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; permutations : IntList -> IntListList
;;; GIVEN: a list of integers
;;; WHERE: the list contains no duplicates
;;; RETURNS: a list of all permutations of that list,
;;;     in shortlex order
;;; EXAMPLES:
;;;     (permutations (list))  =>  (list (list))
;;;     (permutations (list 9))  =>  (list (list 9))
;;;     (permutations (list 3 1 2))
;;;         =>  (list (list 1 2 3)
;;;                   (list 1 3 2)
;;;                   (list 2 1 3)
;;;                   (list 2 3 1)
;;;                   (list 3 1 2)
;;;                   (list 3 2 1))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define  (permutations input-lst )
  (
   ind-to-actual-lstlst
   (indlstlst-for-input input-lst)
   ( list )
   (sort input-lst < )))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (check-equal?
   (indlstlst-for-input  (list 0 1 2))
   (list (list 0 1 2) (list 0 2 1) (list 1 0 2)
         (list 1 2 0) (list 2 0 1) (list 2 1 0))
   "THe output must be as same as the given")
  (check-equal?
   (permutations (list 3 1 2) )
   (list (list 1 2 3) (list 1 3 2) (list 2 1 3)
         (list 2 3 1) (list 3 1 2) (list 3 2 1))))
