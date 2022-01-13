#! /usr/bin/env racket

#lang racket

(define (extract str)
  (substring str 4 7))

(extract "the cat out of the bag")

(define (halfbake flavor)
  (string-append flavor " creme brulee")
  )

(define x 3)

(+ x 2)


(define (reply s)
  (
   if (string-prefix? s "hello ")
      "hi!"
      
   ;else
      "huh?")
  )

(define (reply-more s)
  (
   cond
   [(string-prefix? s "hello ")
    "hi!"]
   [(string-prefix? s "goodbye ")
    "bye!"]
   [(string-suffix? s "?")
    "I don't know"]
  )
)



(define (double v)
  (
   (if (string? v)
       string-append +) v v)
)


(define (twice f v)
  (f (f v)))

(twice (lambda (s) (string-append s "!"))"hello")





(define (my-map f lst)
  (define (iter lst backward-result)
    (cond
     [(empty? lst) (reverse backward-result)]
     [else (iter (rest lst)
                 (cons (f (first lst))
                       backward-result))]))
  (iter lst empty))



(define (remove-dups l)
  (cond
   [(empty? l) empty]
   [(empty? (rest l)) l]
   [else
    (let ([i (first l)])
      (if (equal? i (first (rest l)))
          (remove-dups (rest l))
          (cons i (remove-dups (rest l)))))]))

(define (P1 lst)
  (if (= (length lst) 1) (car lst)  (P1 (cdr lst)) )
  )


(define (P2 lst)
  (if (= (length lst) 2) lst  (P2 (cdr lst)) )
  )

(define (P3 lst k)
  (if (= k 1) (car lst)  (P3 (cdr lst) (- k 1)) )
  )

(define (P4 lst)
  (if (empty? lst) 0 (+ 1 (P4 (cdr lst))))
  )

(define (P5 lst)
  (if (empty? lst) lst (append (P5 (cdr lst))  (list (car lst))))
  )

(define (P6 lst)
  (equal? lst (P5 lst))
  )


(define (P7 lst)
  (if (empty? lst) empty
  (if (list? (car lst)) (append (P7 (car lst)) (P7 (cdr lst))) (cons (car lst) (P7 (cdr lst))) ))
  )

(define (P8 lst)
  (cond
    [(empty? lst) lst]
    [(empty? (cdr lst)) lst]
    [(equal? (car lst) (car (cdr lst))) (P8 (cdr lst))]
    [else (cons (car lst) (P8 (cdr lst)))] 
    )
  )

(define (factorial i fact)
  (if (= i 1)
      fact
      (factorial (- i 1) (* i fact))))




;----------------------------REWRITE BEGINS--------------------------------


;------------------PROBLEM 1----------------------------

(display "\nPROBLEM 1\n")

(define my-last
  (lambda (given_list)
         (match given_list
           [(list) error "Given List is empty"]
           [(list x) x]
           [_ (my-last (cdr given_list))])))

(my-last '())
(my-last '(1))
(my-last '(1 2))
(my-last '(1 2 3))
(my-last '(1 2 3 4))

;--------------------END--------------------------------



;------------------PROBLEM 2----------------------------

(display "\nPROBLEM 2\n")

(define my-but-last
  (lambda (given_list)
    (match given_list
      [(or (list) (list _)) error "Not enough element in the list"]
      [(list x y) given_list]
      [_ (my-but-last (cdr given_list))])))


(my-but-last '())
(my-but-last '(1))
(my-but-last '(1 2))
(my-but-last '(1 2 3))
(my-but-last '(1 2 3 4))

;--------------------END--------------------------------




;------------------PROBLEM 3----------------------------

(display "\nPROBLEM 3\n")

(define recur-element-in-list
  (lambda (lst k)
    (cond
      [(= k 1) (car lst)]
      [else (recur-element-in-list (cdr lst) (- k 1))])))

(define element-at
  (lambda (given_list index)
    (cond
      [(empty? given_list) error "List is empty"]
      [(<= index 0)  error "index k is negative or zero"]
      [(> index (length given_list)) error "index greater than list size"]
      [else (recur-element-in-list given_list index)])))
        

(element-at '() 0)
(element-at '(1) 0)
(element-at '(1) 1)
(element-at '(1) 2)
(element-at '(1 2 3) 0)
(element-at '(1 2 3) 1)
(element-at '(1 2 3) 2)
(element-at '(1 2 3) 3)
(element-at '(1 2 3) 4)
(element-at '(1 2 3 4) 2)

;--------------------END--------------------------------




;------------------PROBLEM 4----------------------------

;p04 my-length
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TOOO SAME
; READ AND ALTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "\nPROBLEM 4\n")

(define my-length
  (lambda (given_list)
    (define len 0)
    
    (define recur-length
      (lambda (lst length)
        (match lst
          [(list) length]
          [_ (recur-length (cdr lst) (+ length 1))])))
    
    (recur-length given_list len)))

(my-length '())
(my-length '(1))
(my-length '(1 2))
(my-length '(1 2 3))
(my-length '(1 2 3 4))

;--------------------END--------------------------------




;------------------PROBLEM 5----------------------------

;p05 my-reverse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TOOO SAME
; READ AND ALTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "\nPROBLEM 5\n")

(define my-reverse
  (lambda (given_list)
    
    (define reversed_list '())
    
    (define recur-reverse
      (lambda (lst rev-lst)
        (match lst
          [(list) rev-lst]
          [_ (recur-reverse (cdr lst) (cons (car lst) rev-lst))])))
    
    (recur-reverse given_list reversed_list)))
    

(my-reverse '())
(my-reverse '(1))
(my-reverse '(1 2))
(my-reverse '(1 2 3))
(my-reverse '(1 2 3 4))

;--------------------END--------------------------------





;------------------PROBLEM 6----------------------------

;p06 palindrome?

(display "\nPROBLEM 6\n")

(define palindrome?
  (lambda (given_list)
    (equal? given_list (my-reverse given_list))))

(palindrome? '())
(palindrome? '(1))
(palindrome? '(1 2))
(palindrome? '(1 1))
(palindrome? '(1 2 1))
(palindrome? '(1 2 2 1))
(palindrome? '(1 2 3 4))

;--------------------END--------------------------------



;------------------PROBLEM 7----------------------------

(display "\nPROBLEM 7\n")

;; IS there a tail recursive solution??

(define my-flatten
  (lambda (given_list)
    (cond
      [(empty? given_list) given_list]
      [(list? (car given_list)) (append (my-flatten (car given_list)) (my-flatten (cdr given_list)))]
      [else (cons (car given_list) (my-flatten (cdr given_list)))])))

(my-flatten '())
(my-flatten '(1))
(my-flatten '((((1)))))
(my-flatten '(1 2 3))
(my-flatten '((1 2) (3) (4 5 (6 7 (8)))))


;--------------------END--------------------------------



;------------------PROBLEM 8----------------------------

(display "\nPROBLEM 8\n")

(define compress
  (lambda (given_list)
    
    
    (define compressed_list '())
    
    (define recur-compress
      (lambda (lst cmp-lst) 
        (match lst
          [(list) cmp-lst]
          [(list ele) (append cmp-lst (list ele))]
          [(list a a b ...) (recur-compress (cdr lst) cmp-lst)]
          [_ (recur-compress (cdr lst) (append cmp-lst (list (car lst))))])))
    
    (recur-compress given_list compressed_list)))


(compress '(a a a a b c c a a d e e e e))

;--------------------END--------------------------------


;------------------PROBLEM 9----------------------------

(display "\nPROBLEM 9\n")

(define pack
  (lambda (given_list)
    
    
    (define packed_list '())
    (define tmp-lst '())
    
    (define recur-pack
      (lambda (lst pack-lst tmp-lst) 
        (match lst
          [(list) pack-lst]
          [(list ele) (append pack-lst (list (cons ele tmp-lst)))]
          [(list a a b ...) (recur-pack (cdr lst) pack-lst (cons a tmp-lst))]
          [_ (recur-pack (cdr lst) (append pack-lst (list (cons (car lst) tmp-lst))) '())])))
    
    (recur-pack given_list packed_list tmp-lst)))

(pack '(a a a a b c c a a d e e e e))

;--------------------END--------------------------------


;------------------PROBLEM 10----------------------------

(display "\nPROBLEM 10\n")

(define add-length
  (lambda (lst)
    (list (length lst) (car lst))))

(define encode
  (lambda (given_list)
    (define packed_list (pack given_list))
    (map add-length packed_list)))

(encode '(a a a a b c c a a d e e e e))

;--------------------END--------------------------------


;------------------PROBLEM 11----------------------------

(display "\nPROBLEM 11\n")

(define add-length-modified
  (lambda (lst)
    (match lst
      [(list ele) ele]
      [_ (list (length lst) (car lst))])))

(define encode-modified
  (lambda (given_list)
    (define packed_list (pack given_list))
    (map add-length-modified packed_list)))

(encode-modified '(a a a a b c c a a d e e e e))

;--------------------END--------------------------------


;------------------PROBLEM 12----------------------------

(display "\nPROBLEM 12\n")

;p12 decode

(define create-repeat-list
  (lambda (repeat ele [lst '()])
    (if (= repeat 0)
        lst
        (create-repeat-list (- repeat 1) ele (cons ele lst)))))

(define remove-length
  (lambda (ele)
    (if (list? ele)
        (create-repeat-list (car ele) (cadr ele))
        (list ele))))

(define decode
  (lambda (given_list)
    (foldl
     (lambda (element decoded_lst) (append decoded_lst (remove-length element)))
     '()
     given_list)))

(decode '((4 a) b (2 c) (2 a) d (4 e)))

;--------------------END--------------------------------




;------------------PROBLEM 13----------------------------

(display "\nPROBLEM 13\n")

(define format-entry
  (lambda (repeat ele)
    (if (= 1 repeat)
        ele
        (cons repeat (list ele)))))

(define encode-direct
  (lambda (given_list)
    
    (define recur-encode
      (lambda (lst encoded-lst tmp-len) 
        (match lst
          [(list) encoded-lst]
          [(list ele) (cons (format-entry (+ 1 tmp-len) ele) encoded-lst)]
          [(list a a b ...) (recur-encode (cdr lst) encoded-lst (+ 1 tmp-len))]
          [_ (recur-encode (cdr lst) (cons (format-entry (+ 1 tmp-len) (car lst)) encoded-lst) 0)])))
    
    (my-reverse(recur-encode given_list '() 0))))

(encode-direct '(a a a a b c c a a d e e e e))

;--------------------END--------------------------------


;------------------PROBLEM 14----------------------------

(display "\nPROBLEM 14\n")

(define dupli
  (lambda (given_list)
    
    (define recur-dupli
      (lambda (lst dup-lst) 
        (match lst
          [(list) dup-lst]
          [_ (recur-dupli (cdr lst) (cons (car lst) (cons (car lst) dup-lst)))])))
    
    (my-reverse(recur-dupli given_list '()))))
    
(dupli '(a b c c d))

;--------------------END--------------------------------




;------------------PROBLEM 15----------------------------

(display "\nPROBLEM 15\n")

(define repli
  (lambda (given_list repeat)
    
    (define recur-repli
      (lambda (lst rep-lst rep) 
        (match lst
          [(list) rep-lst]
          [_ (recur-repli (cdr lst) (create-repeat-list rep (car lst) rep-lst) rep)])))
    
    (my-reverse(recur-repli given_list '() repeat))))
    
(repli '(a b b c) 3)

;--------------------END--------------------------------




;------------------PROBLEM 16----------------------------

(display "\nPROBLEM 16\n")

(define drop
  (lambda (given_list n)

    (cond
      [(<= n 0) error "Invalid n, n should be greater than 0"]
      [else
       
       (define recur-drop
         (lambda (lst res-lst count) 
           (cond
             [(empty? lst) res-lst]
             [(= count (- n 1)) (recur-drop (cdr lst) res-lst 0)]
             [else (recur-drop (cdr lst) (cons (car lst) res-lst) ( + 1 count))])))

       (my-reverse(recur-drop given_list '() 0))])))
    
    
(drop '(a b c d e f g h i k) 3)

;--------------------END--------------------------------




;------------------PROBLEM 17----------------------------

(display "\nPROBLEM 17\n")

(define split
  (lambda (given_list split_index)

    (cond
      [(< split_index 0) error "Invalid n, n should be greater than or equal to 0"]
      [else
       
       (define recur-split
         (lambda (lst first second count) 
           (cond
             [(empty? lst) (cons (my-reverse first) (list (my-reverse second)))]
             [(> count 0) (recur-split (cdr lst) (cons (car lst) first) second (- count 1))]
             [else (recur-split (cdr lst) first (cons (car lst) second) (- count 1))])))

       (recur-split given_list '() '() split_index)])))
    
    
(split '(a b c d e f g h i j k) 3)

;--------------------END--------------------------------




;------------------PROBLEM 18----------------------------

(display "\nPROBLEM 18\n")

(define slice
  (lambda (given_list start end)

    (cond
      [(< end start) error "Invalid start and end values"]
      [else
       
       (define recur-slice
         (lambda (lst res-lst count) 
           (cond
             [(or (empty? lst) (> count end)) (my-reverse res-lst)]
             [(< count start) (recur-slice (cdr lst) res-lst (+ 1 count))]
             [else (recur-slice (cdr lst) (cons (car lst) res-lst) (+ count 1))])))

       (recur-slice given_list '() 1)])))
    
    
(slice '(a b c d e f g h i j k) 3 6)
(slice '(a b c d e f g h i k) 3 7)

;--------------------END--------------------------------



;------------------PROBLEM 19----------------------------

(display "\nPROBLEM 19\n")

(define rotate
  (lambda (given_list rotation)
    
    (define len (length given_list))
    (define rem (remainder rotation len))
    (define right-rot (remainder (- len rem) len))
    (define splt-lst (split (my-reverse given_list) right-rot))
    (append (my-reverse (car splt-lst))(my-reverse (cadr splt-lst)))))
    
(rotate '(a b c d e f g h) 3)
;'(D E F G H A B C)
(rotate '(a b c d e f g h) -2)
;'(G H A B C D E F)

;--------------------END--------------------------------



;------------------PROBLEM 20----------------------------

(display "\nPROBLEM 20\n")

(define remove-at
  (lambda (given_list k)

    (cond
      [(<= k 0) error "Invalid n, n should be greater than 0"]
      [else
       
       (define recur-remove-at
         (lambda (lst res-lst count) 
           (cond
             [(empty? lst) res-lst]
             [(= count k) (recur-remove-at (cdr lst) res-lst (+ 1  count))]
             [else (recur-remove-at (cdr lst) (cons (car lst) res-lst) ( + 1 count))])))

       (my-reverse(recur-remove-at given_list '() 1))])))
    
    
(remove-at '(a b c d) 2)

;--------------------END--------------------------------





;------------------PROBLEM 21----------------------------

(display "\nPROBLEM 21\n")

(define insert-at
  (lambda (ele given_list k)

    (cond
      [(<= k 0) error "Invalid n, n should be greater than 0"]
      [else
       
       (define recur-insert-at
         (lambda (lst res-lst count) 
           (cond
             [(empty? lst) res-lst]
             [(= count k) (recur-insert-at lst (cons ele res-lst) (+ 1  count))]
             [else (recur-insert-at (cdr lst) (cons (car lst) res-lst) ( + 1 count))])))

       (my-reverse(recur-insert-at given_list '() 1))])))
    
    
(insert-at 'alfa '(a b c d) 2)

;--------------------END--------------------------------




;------------------PROBLEM 22----------------------------

(display "\nPROBLEM 22\n")

(define range
  (lambda (start end)

    (cond
      [(< end start) error "Invalid start and end values"]
      [else
       
       (define recur-range
         (lambda (res-lst count) 
           (cond
             [(or (> count end)) (my-reverse res-lst)]
             [else (recur-range (cons count res-lst) (+ count 1))])))

       (recur-range '() start)])))

(range 4 9)

;--------------------END--------------------------------



;------------------PROBLEM 23----------------------------

(display "\nPROBLEM 23\n")

(define rnd-select
  (lambda (given_list n)

    (define len (my-length given_list))

    (cond
      [(< n 0) error "Invalid n, n should be greater than or equal to 0"]
      [else
       
       (define recur-rnd-select
         (lambda (lst lst-len res-lst count) 
           (cond
             [(= count 0) (my-reverse res-lst)]
             [else (define index (random 1 ( + 1 lst-len)))
                   (recur-rnd-select (remove-at lst index) (- lst-len 1) (cons (element-at lst index) res-lst) (- count 1))])))

       (my-reverse(recur-rnd-select given_list len '() n))])))
    

(rnd-select '(a b c d e f g h) 3)

;--------------------END--------------------------------




;------------------PROBLEM 24----------------------------

(display "\nPROBLEM 24\n")

(define lotto-select
  (lambda (n m)
    (cond
      [(< n 0) error "Invalid n, n should be greater than or equal to 0"]
      [(> n m) error "Invalid n and m. Should be n <= m"]
      [else (rnd-select (range 1 m) n)])))
    
(lotto-select 6 49)

;--------------------END--------------------------------



;------------------PROBLEM 25----------------------------

(display "\nPROBLEM 25\n")

(define rnd-permu
  (lambda (given_list)
    (rnd-select given_list (my-length given_list))))

(rnd-permu '(a b c d e f))

;--------------------END--------------------------------




;------------------PROBLEM 26----------------------------

(display "\nPROBLEM 26\n")

(define combination
  (lambda (selection given_list)

    (define backtrack
      (lambda (k lst lst-len cur-set result)
        (cond
          [(> k lst-len) result]
          [(or (= 0 k) (empty? lst)) (cons (my-reverse cur-set) result)]
          [else (define new_res (backtrack (- k 1) (cdr lst) (- lst-len 1) (cons (car lst) cur-set) result))
                (backtrack k (cdr lst) (- lst-len 1) cur-set new_res)])))
    
    (my-reverse(backtrack selection given_list (my-length given_list) '() '()))))

(combination 3 '(a b c d e f))

;--------------------END--------------------------------


;------------------PROBLEM 27----------------------------

(display "\nPROBLEM 27\n")


(define group3
  (lambda (given_list)
    (group given_list '(2 3 4))))



(define combination-split
  (lambda (selection given_list)

    (define backtrack-split
      (lambda (k lst lst-len cur-set rem-set result)
        (cond
          [(> k lst-len) result]
          [(empty? lst) (cons (cons (my-reverse cur-set) (my-reverse rem-set)) result)]
          [(= 0 k) (cons (cons (my-reverse cur-set) (append (my-reverse rem-set) lst)) result)]
          [else (define new_res (backtrack-split (- k 1) (cdr lst) (- lst-len 1) (cons (car lst) cur-set) rem-set result))
                (backtrack-split k (cdr lst) (- lst-len 1) cur-set (cons (car lst) rem-set) new_res)])))
    
    (my-reverse(backtrack-split selection given_list (my-length given_list) '() '() '()))))

(combination-split 3 '(a b c d e f))


(define group
  (lambda (given_list count_list)

    (define group_recur
      (lambda (lst c_lst)
        (cond
          [(empty? (cdr c_lst)) (list lst)]
          [else (define group_split (combination-split (car c_lst) lst))
                (foldl (lambda (element result)
                         (define next_groups (group_recur (cdr element) (cdr c_lst)))
                         (define cur-group (map (lambda (next_group)
                                                  (append (list (car element)) (list next_group)))
                                                next_groups))
                         (append cur-group result))

                       '()
                       group_split)])))
                
    
    (if (= (foldl + 0 count_list) (length given_list))
        (my-reverse (group_recur given_list count_list))
        (error "Invalid Inputs"))))
    

(display "\nPart a\n")
(length (group3 '(aldo beat carla david evi flip gary hugo ida)))

(display "\nPart b\n")
 ;(group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
(group '(1 2 3 4 5) '(2 3))

;--------------------END--------------------------------



;------------------PROBLEM 28----------------------------

(display "\nPROBLEM 28\n")

(define lsort
  (lambda (given_list)
    (sort given_list (lambda (ele1 ele2) (< (length ele1) (length ele2))))))

(lsort '((a b c) (d e) (f g h) (k e) (i j k l) (m n) (o)))

(define frequency
  (lambda (given_list ele)
    (define ele_len (length ele))
    (define freq_recur
      (lambda (len lst freq)
        (if (empty? lst)
            freq
            (if (= (length (car lst)) len)
                (freq_recur len (cdr lst) (+ 1 freq))
                (freq_recur len (cdr lst) freq)))))
    (freq_recur ele_len given_list 0)))

(define lfsort
  (lambda (given_list)
    (sort given_list (lambda (ele1 ele2) (< (frequency given_list ele1) (frequency given_list ele2))))))

(lfsort '((a b c) (d e) (f g h) (k e) (i j k l) (m n) (o)))


;--------------------END--------------------------------

