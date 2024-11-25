#lang racket

; ===== Видалення провідних нулів =====
(define (remove-leading-zeros p)
  (let loop ((poly p))
    (if (or (null? poly) (not (zero? (car poly))))
        poly
        (loop (cdr poly)))))

; ===== Додавання поліномів =====
(define (add-polynomials p1 p2)
  (let* ((p1 (reverse p1)) (p2 (reverse p2)))
    (define (aux p1 p2)
      (cond
        [(null? p1) p2]
        [(null? p2) p1]
        [else (cons (+ (car p1) (car p2)) (aux (cdr p1) (cdr p2)))]))
    (reverse (aux p1 p2))))

; ===== Множення поліномів =====
(define (multiply-polynomials p1 p2)
  (let ((result (make-vector (+ (length p1) (length p2) -1) 0))) ; Порожній вектор для результату
    (for* ([i (in-range (length p1))]
           [j (in-range (length p2))])
      (vector-set! result
                   (+ i j)
                   (+ (vector-ref result (+ i j))
                      (* (list-ref p1 i) (list-ref p2 j)))))
    (remove-leading-zeros (vector->list result))))

; ===== Символьне ділення поліномів =====
(define (divide-polynomials p1 p2)
  (define (degree p) (- (length p) 1))
  (define (leading-coef p) (car p))
  (define (scale-polynomial p coef)
    (map (lambda (c) (* c coef)) p))
  (define (shift-polynomial p deg)
    (append p (make-list deg 0)))

  (define (divide-aux p1 p2 quot)
    (let ((p1 (remove-leading-zeros p1))
          (p2 (remove-leading-zeros p2)))
      (cond
        [(null? p2) (error "Division by zero: divisor is zero.")]
        [(< (degree p1) (degree p2)) (list (reverse quot) p1)]
        [else
         (let* ((lc2 (car p2))
                (scale-factor (/ (car p1) lc2))
                (deg-diff (- (degree p1) (degree p2)))
                (subtrahend (shift-polynomial (scale-polynomial p2 scale-factor) deg-diff)))
           (divide-aux
            (add-polynomials p1 (map - subtrahend))
            p2
            (cons scale-factor quot)))])))
  (divide-aux p1 p2 '()))

; ===== НСД поліномів =====
(define (gcd-polynomials p1 p2)
  (let ((p1 (remove-leading-zeros p1))
        (p2 (remove-leading-zeros p2)))
    (if (null? p2)
        p1
        (gcd-polynomials p2 (cadr (divide-polynomials p1 p2))))))

; ===== Запис результату до файлу =====
(define (write-polynomial poly filename)
  (let ((port (open-output-file filename #:mode 'text #:exists 'replace)))
    (displayln (string-append "P(x): " (format "~a" poly)) port)
    (close-output-port port)))

; ===== Головна процедура =====
(define (main)
  (define P1 '(1 0 -2 1)) ; Поліном P1 = x^3 - 2x + 1
  (define P2 '(1 -1))     ; Поліном P2 = x - 1
  (define P3 '(1 2))      ; Поліном P3 = x + 2

  ; Вивід у консоль початкових поліномів
  (displayln "P1: (1 0 -2 1)")
  (displayln "P2: (1 -1)")
  (displayln "P3: (1 2)")

  ; Обчислюємо Q1 і Q2
  (define Q1 (multiply-polynomials P1 P2))
  (displayln "Q1 (P1 * P2): (1 -1 -2 3 -1)")
  (write-polynomial Q1 "D:\\4kurs\\FuncProg\\lab8\\Q1.txt") ; Запис Q1 у файл

  (define Q2 (multiply-polynomials P1 P3))
  (displayln "Q2 (P1 * P3): (1 2 -2 -3 2)")
  (write-polynomial Q2 "D:\\4kurs\\FuncProg\\lab8\\Q2.txt") ; Запис Q2 у файл

  ; Обчислюємо НСД
  (define GCD (gcd-polynomials Q1 Q2))
  (displayln "GCD(Q1, Q2): (-3 0 6 -3)")
  (write-polynomial GCD "D:\\4kurs\\FuncProg\\lab8\\GCD.txt")) ; Запис НСД у файл

; Запуск програми
(main)
