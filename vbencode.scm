(define (prepend seq elm)
  (cons elm seq))

(define (extend seq1 seq2)
  (append seq1 seq2))

(define (vb-encode-number n1)
  (let loop ((bytes '())
             (n n1))
    (if (< n 128)
        (begin
          (unless (null? bytes)
                  (set-car! (list-tail bytes (- (length bytes) 1))
                            (+ (last bytes) 128)))
          (cons n bytes))
        (loop (prepend bytes (mod n 128)) (div n 128)))))
          
(define (vb-encode numbers)
  (fold (lambda  (n acc)
          (extend acc (vb-encode-number n)))
        '() numbers))
    
(define (vb-decode bytestream)
  (define (in-decode bstream n numbers)
    (if (null? bstream) numbers
        (let ((abyte (car bstream)))
          (if (< abyte 128)
              (in-decode (cdr bstream)
                         (+ (* 128 n) abyte)
                         numbers)
              (in-decode (cdr bstream)
                         0
                         (extend numbers (list (+ (* 128 n) abyte -128)))
                         )))))
  (in-decode bytestream 0 '()))

    
    
                          
                    
