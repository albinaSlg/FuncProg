; Реалізація функції range для створення списку значень
(define (range start end step)
  (if (>= start end)
      '()
      (cons start (range (+ start step) end step))))

; Функція для обчислення значення cos(x) за рядом Тейлора
; n - кількість членів ряду
(define (cos-taylor x n)
  (letrec 
      ((cos-helper ; Визначення допоміжної рекурсивної функції
        (lambda (k sum)
          (if (= k n)
              sum
              (cos-helper (+ k 1) (+ sum (* (/ (expt (- 1) k) (factorial (* 2 k))) (expt x (* 2 k))))))))) ; Сума ряду
    (cos-helper 0 0))) ; Початкові значення для cos-helper

; Функція для обчислення факторіалу
(define (factorial n)
  (letrec ((fact-helper (lambda (n acc)
                          (if (<= n 1)
                              acc
                              (fact-helper (- n 1) (* n acc)))))) ; Рекурсивний факторіал
    (fact-helper n 1))) ; Хвостова рекурсія для оптимізації

; Функція для обчислення значення y
(define (calculate-y x)
  (cond
    ((and (> x 0) (<= x 1)) ; 0 < x <= 1
     (- (expt (cos-taylor x 50) 2) (cos-taylor x 50)))
    ((and (>= x -2) (<= x 0)) ; -2 <= x <= 0
     (+ (expt (cos-taylor x 50) 3) (cos-taylor (* 2 x) 50)))
    (else
     #f))) ; Повертає #f, якщо x поза дозволеним діапазоном

; Функція для обчислення абсолютної похибки
(define (error exact-y approx-y)
  (if (and exact-y approx-y)
      (abs (- exact-y approx-y))
      #f)) ; Якщо значення #f, повертає #f

; Функція для форматування чисел із заданою кількістю десяткових знаків
(define (format-number x precision)
  (if x
      (/ (round (* x (expt 10 precision))) (expt 10 precision))
      "N/A")) ; Якщо значення #f, повертає "N/A"

; Основна функція
(define (main)
  (for-each
   (lambda (x)
     (let* ((exact-y (cond
                      ((and (> x 0) (<= x 1))
                       (- (expt (cos x) 2) (cos x))) ; Точне значення для 0 < x <= 1
                      ((and (>= x -2) (<= x 0))
                       (+ (expt (cos x) 3) (cos (* 2 x)))) ; Точне значення для -2 <= x <= 0
                      (else
                       #f))) ; Повертає #f, якщо x поза діапазоном
            (approx-y (calculate-y x))
            (error-value (error exact-y approx-y)))
       (display "x: ")
       (display x)
       (if (not exact-y)
           (display ", Помилка: x не входить у дозволений діапазон") ; Виводить помилку
           (begin
             (display ", y (exact): ")
             (display (format-number exact-y 6)) ; Форматоване точне значення
             (display ", y (approx): ")
             (display (format-number approx-y 6)) ; Форматоване наближене значення
             (display ", error: ")
             (display (format-number error-value 6)))) ; Форматована похибка
       (newline)))
   (range -2 2 0.5))) ; Використання функції range для створення списку значень x

; Виклик основної функції
(main)

