(define (triangle-area n m depth)
 (cond
   ;; Якщо n = 1 і m = 1
   ((and (= n 1) (= m 1))
    (display "Глибина рекурсії: ") (display depth) (newline) 1)
   ;; Якщо n > 1
   ((> n 1)
    (+ m (triangle-area (- n 1) m (+ depth 1))))
   ;; Якщо m > 1
   ((> m 1)
    (+ 1 (triangle-area n (- m 1) (+ depth 1))))))

;; Функція-обгортка для обчислення
(define (main)
 (display "Введіть n: ")
 (let ((n (read))) ; Зчитуємо n з клавіатури
   (display "Введіть m: ")
   (let ((m (read))) ; Зчитуємо m з клавіатури
     (if (and (> n 0) (> m 0)) ; Перевірка, що n > 0 і m > 0
         (begin
           (display "Результат: ")
           (display (triangle-area n m 1)) ; Обчислення площі трикутника
           (newline))
         (display "Помилка: n та m повинні бути додатними цілими числами"))))) ; Перевірка некоректних значень

;; Виклик основної функції
(main)
