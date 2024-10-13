; Функція для інтегрування
(define (f x)
  (/ (+ (* x x) 1.4) (+ (sqrt (+ (* x x) 0.2)))))

; Метод прямокутників
(define (rectangle-method f a b n mode)
  (let ((h (/ (- b a) n))) ; Крок
    (define (loop i sum)
      (if (= i n)
          (* h sum) ; Повертаємо інтеграл
          (let ((x (cond
                     ((eq? mode 'left) (+ a (* i h))) ; Ліві прямокутники
                     ((eq? mode 'right) (+ a (* (+ i 1) h))) ; Праві прямокутники
                     (else (error "Невідомий режим: використовуйте 'left або 'right")))))
            (loop (+ i 1) (+ sum (f x))))))
    (loop 0 0)))

; Параметри інтегрування
(define a 0.4) ; Нижня межа
(define b 1.8) ; Верхня межа
(define n 100) ; Кількість підінтервалів

; Обчислення інтегралів для лівих і правих прямокутників
(define integral-left (rectangle-method f a b n 'left))
(define integral-right (rectangle-method f a b n 'right))

; Вивід результатів
(display "Інтеграл методом лівих прямокутників: ") (display integral-left) (newline)
(display "Інтеграл методом правих прямокутників: ") (display integral-right) (newline)

