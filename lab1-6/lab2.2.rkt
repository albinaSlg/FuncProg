; Функція для обчислення нескінченного ланцюгового дробу
; precision - задана точність
(define (continued-fraction precision)
  (define (cf-helper current-term denominator)
    (let ((new-denominator (+ 4 (/ 1 denominator)))) ; Обчислюємо новий знаменник
      (if (< (abs (- current-term (+ 2 (/ 1 new-denominator)))) precision)
          (+ 2 (/ 1 new-denominator)) ; Повертаємо значення, якщо досягнуто точності
          (cf-helper (+ 2 (/ 1 new-denominator)) new-denominator)))) ; Продовжуємо обчислення
  (cf-helper 2 4)) ; Початкові значення: 2 і 4

; Виклик функції з точністю 10^-6
(let ((result (continued-fraction 1e-6)))
  (display "Result: ")
  (display result)
  (newline))

