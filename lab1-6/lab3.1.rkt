; Метод Ньютона
(define (newton-method f f-derivative x0 tol max-iter)
  (let loop ((x x0) (iter 0))
    (if (or (> iter max-iter) (< (abs (f x)) tol))
        x ; Повертаємо корінь, якщо досягнута точність або максимум ітерацій
        (let ((next-x (- x (/ (f x) (f-derivative x)))))
          (if (<= next-x 0)
              (error "Значення x стало недійсним (x <= 0)")
              (loop next-x (+ iter 1)))))))

; Метод простої ітерації
(define (simple-iteration g x0 tol max-iter)
  (let loop ((x x0) (iter 0))
    (if (or (> iter max-iter) (< (abs (- x (g x))) tol))
        x ; Повертаємо корінь, якщо досягнута точність або максимум ітерацій
        (let ((next-x (g x)))
          (if (<= next-x 0)
              (error "Значення x стало недійсним (x <= 0)")
              (loop next-x (+ iter 1)))))))

; Основна функція f(x) = x^2 - 2x + ln(x)
(define (f x)
  (if (> x 0)
      (+ (* x x) (- (* 2 x)) (log x))
      (error "f(x) недійсне для x <= 0")))

; Похідна f'(x) = 2x - 2 + 1/x
(define (f-derivative x)
  (if (> x 0)
      (+ (* 2 x) -2 (/ 1 x))
      (error "f'(x) недійсне для x <= 0")))

; Функція g(x) для простої ітерації (перетворене рівняння)
(define (g x)
  (if (> x 0)
      (+ 2 (/ (- (log x)) x))
      (error "g(x) недійсне для x <= 0")))

; Початкові параметри
(define x0 1.5) ; Початкове наближення
(define tol 1e-6) ; Точність
(define max-iter 100) ; Максимальна кількість ітерацій

; Обчислення коренів
(define root-newton
  (newton-method f f-derivative x0 tol max-iter))

(define root-iteration
  (simple-iteration g x0 tol max-iter))

; Вивід результатів
(display "Корінь методом Ньютона: ") (display root-newton) (newline)
(display "Корінь методом простої ітерації: ") (display root-iteration) (newline)

