#lang racket

; Створити список квадратних чисел
(define (generate-square-numbers n)
  (map (lambda (x) (* x x)) (range 1 (+ n 1))))

; Створити підсписок з парних елементів
(define (filter-even-squares square-list)
  (filter even? square-list))

; Підрахувати кількість елементів, що кратні 4
(define (count-multiples-of-4 square-list)
  (length (filter (lambda (x) (= (modulo x 4) 0)) square-list)))

; Генерація трикутних чисел
(define (generate-triangular-numbers n)
  (map (lambda (i) (/ (* i (+ i 1)) 2)) (range 1 (+ n 1))))

; Перевірити, чи є квадрат сумою двох послідовних трикутних чисел
(define (is-sum-of-two-triangulars square triangulars)
  (let loop ((i 0))
    (if (or (>= i (- (length triangulars) 1))) ; Перевірка виходу за межі списку
        #f ; Якщо нічого не знайдено
        (let ((sum (+ (list-ref triangulars i) (list-ref triangulars (+ i 1)))))
          (if (= square sum)
              #t ; Знайдено
              (loop (+ i 1))))))) ; Продовжуємо перевірку

; Знайти квадрати, які є сумою двох послідовних трикутних чисел
(define (find-squares-summing-triangulars square-list triangulars)
  (filter (lambda (square) (is-sum-of-two-triangulars square triangulars)) square-list))

; Основна процедура
(define (main n)
  (define square-numbers (generate-square-numbers n)) ; Генеруємо квадратні числа
  (define even-squares (filter-even-squares square-numbers)) ; Підсписок парних квадратів
  (define multiples-of-4 (count-multiples-of-4 square-numbers)) ; Кількість квадратів, кратних 4
  (define triangular-numbers (generate-triangular-numbers (* 2 n))) ; Генеруємо трикутні числа
  (define sums-check (find-squares-summing-triangulars square-numbers triangular-numbers)) ; Квадрати як сума трикутних
  
  ; Вивід результатів
  (displayln (string-append "Список квадратних чисел: " (format "~a" square-numbers)))
  (displayln (string-append "Підсписок парних квадратів: " (format "~a" even-squares)))
  (displayln (string-append "Кількість квадратів, кратних 4: " (number->string multiples-of-4)))
  (displayln (string-append "Квадрати, які є сумою двох послідовних трикутних чисел: " (format "~a" sums-check))))

; Виконати основну процедуру для 10 квадратних чисел
(main 10)

