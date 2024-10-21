#lang racket

; Знаходимо найбільший спільний дільник (НСД)
(define (gcd a b)
  (if (= b 0) a (gcd b (modulo a b))))

; Знаходимо найменше спільне кратне (НСК)
(define (lcm a b)
  (/ (* a b) (gcd a b)))

; Приведення всіх дробів до спільного знаменника
(define (normalize-fractions fractions)
  (define common-denominator
    (foldl lcm 1 (map cadr fractions))) ; Обчислюємо НСК знаменників
  (map (lambda (frac)
         (let ((numerator (* (car frac) (/ common-denominator (cadr frac)))))
           (list numerator common-denominator))) ; Приводимо до спільного знаменника
       fractions))

; Форматування дробів у вигляді "чисельник/знаменник"
(define (fraction->string frac)
  (string-append (number->string (car frac)) "/" (number->string (cadr frac))))

; Обчислення суми всіх дробів
(define (sum-fractions fractions)
  (define normalized (normalize-fractions fractions))
  (let ((numerator (foldl + 0 (map car normalized))))
    (list numerator (cadr (first normalized))))) ; Зберігаємо спільний знаменник

; Обчислення добутку всіх дробів
(define (product-fractions fractions)
  (let ((numerator (foldl * 1 (map car fractions)))
        (denominator (foldl * 1 (map cadr fractions))))
    (let ((g (gcd numerator denominator))) ; Спрощуємо результат
      (list (/ numerator g) (/ denominator g)))))

; Генерація списку раціональних дробів
(define (generate-fractions n)
  (map (lambda (i)
         (list (+ i 1) (+ (* i 2) 3))) ; Формуємо дроби виду (i+1)/(2i+3)
       (range 1 (+ n 1))))

; Основна процедура
(define (main n)
  (define fractions (generate-fractions n)) ; Генеруємо список дробів
  (displayln (string-append "Згенеровані дроби: " (format "~a" (map fraction->string fractions))))
  (define normalized (normalize-fractions fractions))
  (displayln (string-append "Дроби після приведення до спільного знаменника: " 
                            (format "~a" (map fraction->string normalized))))
  (define sum (sum-fractions fractions))
  (displayln (string-append "Сума дробів: " (fraction->string sum)))
  (define product (product-fractions fractions))
  (displayln (string-append "Добуток дробів: " (fraction->string product))))

; Виконання основної процедури
(main 5) ; Згенеруємо 5 дробів

