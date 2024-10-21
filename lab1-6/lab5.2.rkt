#lang racket

; Визначення алгебраїчної форми комплексного числа
(define (Myreal-part z) (car z)) ; Дійсна частина
(define (Myimag-part z) (cdr z)) ; Уявна частина
(define (make-from-real-imag x y) (cons x y)) ; Створення з дійсної та уявної частини

; Квадрат комплексного числа (a + bi)^2 = (a^2 - b^2) + i(2ab)
(define (square-complex z)
  (let* ((a (Myreal-part z))
         (b (Myimag-part z))
         (real-part (- (* a a) (* b b))) ; Дійсна частина
         (imag-part (* 2 a b)))          ; Уявна частина
    (make-from-real-imag real-part imag-part)))

; Форматування комплексного числа для виводу
(define (complex->string z)
  (string-append (number->string (Myreal-part z))
                 (if (>= (Myimag-part z) 0) " + " " - ")
                 (number->string (abs (Myimag-part z))) "i"))

; Створення списку комплексних чисел
(define (generate-complex-list n)
  (map (lambda (i) (make-from-real-imag i (- n i))) ; Комплексні числа виду (i, n - i)
       (range 1 (+ n 1))))

; Основна процедура
(define (main n)
  (define complex-list (generate-complex-list n)) ; Генеруємо список комплексних чисел
  (define squared-list (map square-complex complex-list)) ; Обчислюємо квадрати
  
  ; Вивід списків
  (displayln "Список комплексних чисел і їх квадратів:")
  (for ([z1 complex-list] [z2 squared-list])
    (printf "~a -> ~a\n" (complex->string z1) (complex->string z2))))

; Запуск основної процедури
(main 5) ; Генеруємо список із 5 комплексних чисел

