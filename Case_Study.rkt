#lang sicp

(define book-list (cons '() '()))


(define (add-book b-id b-name q-left author genre price)
  (define (final-book book-list) 
    (if (null?  (cdr book-list)) book-list (final-book (cdr book-list))))
  (define (create-book b-id b-name q-left author genre price)
  (cons (list(cons 'Book_id b-id) (cons 'Book-Name b-name)
              (cons 'Quantity-Left q-left)
              (cons 'Author author)
              (cons 'Genre genre)
              (cons 'Price price)) '()))
  (let ((last-book (final-book book-list)))
        (set-cdr! last-book (create-book b-id b-name q-left author genre price))))


(add-book 1 'DL 5 'Ian 'Data-Science 1000)
(add-book 2 'ML 10 'Jeremy 'Machine-Learning 3000)
(add-book 3 'HarryPotter  5 'JKR 'Fiction 1000)
(add-book 4 'Keras 10 'KG 'Machine-Learning 3000)
(add-book 5 'Chronicles-of-Narnia 5  'CNG 'Fiction 1000)
(add-book 6  'Tirrukural 10 'Tiruvalluvar 'Tamil 3000)
(add-book 7 'Einstein 5 'AEinstein 'Autobiographies 1000)



(define (find-by-id book-id book-list)
  (if (null? (cdr book-list)) 'Iteration-completed
  (if (= book-id (cdaadr book-list))
      (display (cadr book-list))
      (find-by-id book-id (cdr book-list)))))

(define (find-by-name book-name book-list)
  (if (null? (cdr book-list)) 'Iteration-completed
      (if(eq? book-name (cdr (cadadr book-list)))
         (display (cadr book-list))
         (find-by-name book-name (cdr book-list)))))

(define (find-by-genre genre book-list)
   (if (null? (cdr book-list)) 'Iteration-completed
  (if (eq? genre (cdr (caddr (cddadr book-list))))
      (begin (display (cadr book-list)) (display "\n") (find-by-genre genre (cdr book-list)))
      (find-by-genre genre (cdr book-list)))))

#|
(define l (list (cons 1 100) (cons 2 1000) (cons 3 1020) (cons 4 3000)))

(define (sold l index)
    (if (equal? (caar l) index) (cdar l) (sold (cdr l) index))
    )


(define a '((1 book_title1 book_author1 book_genre1 book_copies_1 book_total_cost_1)(2 book_title2 book_author2 book_genre2 book_copies_2 book_total_cost_2)
       (3 book_title3 book_author3 book_genre3 book_copies_3 book_total_cost_3)(10 book_title4 book_author4 book_genre4 book_copies_4 book_total_cost_4)
       (5 book_title5 book_author5 book_genre5 book_copies_5 book_total_cost_5)))

|#
(define a '((1 book_title1 book_author1 book_genre1 10 )(2 book_title2 book_author2 book_genre2 5 )
       (3 book_title3 book_author3 book_genre3 30 )(10 book_title4 book_author4 book_genre4 50 )
       (5 book_title5 book_author5 book_genre5 1 )))


(define (mergesort L)
  (cond ((null? L)  '())
        ((= 1 (length L)) L)
        ((= 2 (length L)) (mergelists (list (car L))(cdr L)))
        (else (mergelists (mergesort (car (split L)) ) 
                          (mergesort (car (cdr (split L))) ) 
              ))
  )
)

(define (mergelists L M)         ; assume L and M are sorted lists
   (cond ( (null? L) M)
         ( (null? M) L)
         ( (> (car (cdr (cdddr (car L)))) (car (cdr (cdddr (car M)))))
              (cons (car L) (mergelists (cdr L)M)))
         (else
              (cons (car M) (mergelists L (cdr M))))
   )
)


(define (length L)         ; # elements in a list
   (cond ( (null? L) 0)
         (else (+ (length (cdr L)) 1))
  )
)


(define (sub L start stop ctr)    ; extract elements start to stop into a list
   (cond ( (null? L) L)
         ( (< ctr start) (sub (cdr L) start stop (+ ctr 1)))
         ( (> ctr stop) '() )
         (else
            (cons (car L) (sub (cdr L) start stop (+ ctr 1)))
         )
   )
)

(define (split L)                 ; split the list in half:
                                  ; returns ((first half)(second half))
    (let ((len (length L)))
       (cond ((= len 0) (list L L) )
             ((= len 1) (list L '() ))
             (else (list (sub L 1 (/ len 2) 1)(sub L (+(/ len 2)1) len 1)
                   )
             )
       )
     )
)

(define (printtree tree)
   (if (null? tree) '()
       (append (printtree (cadr tree)) (list (car tree))(printtree (caddr tree)))
   ) 
)

(define (addtree tree new)
    (cond ( (null? tree) (list new '() '()) )
          ( (<= (car (cdr (cdddr new))) (car (cdr (cdddr (car tree)))) )  
                (list (car tree) (addtree (cadr tree) new)(caddr tree)) )
          (else
                (list(car tree) (cadr tree) (addtree (caddr tree) new)) )
    )
)


(define (btreesort lis)
   (printtree (treesort lis))
)

(define (treesort lis)
  (if (null? lis) '() 
      (addtree (treesort (cdr lis)) (car lis))
  )
)

(define (reverse1 l)
  (if (null? l)
     nil
     (append (reverse1 (cdr l)) (list (car l)))
  )
)

(define best_selling_author (mergesort a))
(define worst_selling_author (reverse1 best_selling_author))


(define worst_selling_genre (btreesort a))
(define best_selling_genre (reverse1 worst_selling_genre))


(define (authors lst)
   (if (null? lst) 'Iteration-completed
      (begin (display  (car (cddr (car lst)))) (display "\n") (authors (cdr lst)))))

(define (genres lst)
   (if (null? lst) 'Iteration-completed
      (begin (display  (car (cdddr (car lst)))) (display "\n") (genres (cdr lst)))))