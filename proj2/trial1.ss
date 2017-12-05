; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2017                                *
; *  Author: Ulrich Kremer                    *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ltv", "vtl",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
(load "test-dictionary.ss")

;; (load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS
 ; recursively checks dictionary words for equivalence
 ; first(list) == word -> #t, '() -> #f
(define contains?
  (lambda (list word)
    (cond 
          ((null? list) #f)
          ((equal? word (car list)) #t)
          (else (contains? word (cdr list))))))


;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***


;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker
  (lambda (w)
    (contains? dictionary w)))


(define is-word?
  (lambda (w)
    (and (list? w) (not (list? (car w))))))
  
(define num-valid-words
  (lambda (p)
    (reduce + (spell-checker-p p) 0)))

(define arg-max
  (lambda (lst)
    (get-position (apply max lst) lst)))

(define abc-list '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define abc-nums (map ltv abc-list))

(define spell-checker-p
  (lambda (p)
    (cond ((null? p) '())
          ((spell-checker (car p)) (append (list 1) (spell-checker-p (cdr p))))
          (else (append (list 0) (spell-checker-p (cdr p)))))))

(define letter-histogram
  (lambda (p)
    (map (lambda (letter) (count-occurences letter (flatten p))) 
         abc-list)))

(define replace-n-0
  (lambda (n lst)
    (cond ((null? lst) '())
          ((equal? n (car lst)) (append (list 0) (cdr lst)))
          (else (append (list (car lst)) (replace-n-0 n (cdr lst)))))))

(define count-occurences 
  (lambda (item lst)
    (cond ((null? lst) 0)
          ((equal? item (car lst)) (+ 1 (count-occurences item (cdr lst))))
          (else (count-occurences item (cdr lst))))))

(define flatten
  (lambda (l)
    (cond ((null? l) '())
          ((not (list? l)) (list l))
          (else (append (flatten (car l)) (flatten (cdr l)))))))
          


;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input is a word, and output is the encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
      (map vtl
           (map (lambda (x) (modulo (+ n x) 26))
                (map ltv
                     w)));; encript = vtl(ltv(x) +n)mod 26)
      )))

(define get-position
  (lambda (a lst)
    (cond ((equal? a (car lst)) 0)
          (else (+ (get-position a (cdr lst)) 1)))))

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
    (cond ((null? d) '())
          ((is-word? d) (encoder d))
          (else (append (list (encode-d (car d) encoder))
                        (encode-d (cdr d) encoder))))
    ))

(define num-words
  (lambda (p)
    (cond ((null? p) 0)
          (else (+ 1 (num-words (cdr p)))))))
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
    (define check-decode
      (lambda (n)
        (num-valid-words (encode-d p (encode-n n))))
    )
      (let ((decode-compare (map check-decode abc-nums)))
        (encode-n (get-position (apply max decode-compare) decode-compare)))
      ))

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
   (define check-decode?
     (lambda (n)
       (if (number? n)
           (equal? (num-words p) (num-valid-words (encode-d p (encode-n n))))
           (display "error"))))
    (define decoder-iter
      (lambda (freqs)
        (cond ((equal? 0 (apply + freqs)) 0)
              ((check-decode? (- 4 (arg-max freqs))) (- 4 (arg-max freqs)))
              (else (decoder-iter (replace-n-0 (apply max freqs) freqs))))))
    (encode-n (decoder-iter (letter-histogram p)))
    ))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)
    
     (encode-d d decoder)
    ))

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(encode-d document add5)
;;(define decoderSP1 (Gen-Decoder-A paragraph))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)

;;(spell-checker '(h e l l o))(define add5 (encode-n 5))(encode-d document add5)(define decoderSP1 (Gen-Decoder-A (car document)))(define decoderFA1 (Gen-Decoder-B (car document)))(Code-Breaker document decoderSP1)
