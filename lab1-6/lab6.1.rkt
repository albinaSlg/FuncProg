#lang racket

; Знаходження максимального значення серед елементів із непарними індексами
(define (max-odd-index vec)
  (let ((len (vector-length vec))
        (max-val (vector-ref vec 1)) ; Початкове значення (перший непарний індекс)
        (max-index 1))               ; Індекс цього значення
    (do ((index 3 (+ index 2)))      ; Починаємо з 3-го (індексація з 0)
        ((>= index len) (list max-val max-index)) ; Повертаємо максимум і його індекс
      (let ((val (vector-ref vec index)))
        (when (> val max-val)
          (set! max-val val)
          (set! max-index index))))))

; Знаходження мінімального значення серед елементів із парними індексами
(define (min-even-index vec)
  (let ((len (vector-length vec))
        (min-val (vector-ref vec 0)) ; Початкове значення (перший парний індекс)
        (min-index 0))               ; Індекс цього значення
    (do ((index 2 (+ index 2)))      ; Починаємо з 2-го
        ((>= index len) (list min-val min-index)) ; Повертаємо мінімум і його індекс
      (let ((val (vector-ref vec index)))
        (when (< val min-val)
          (set! min-val val)
          (set! min-index index))))))

; Основна процедура
(define (process-vector vec)
  (define max-odd (max-odd-index vec)) ; Знаходимо максимум із непарними індексами
  (define min-even (min-even-index vec)) ; Знаходимо мінімум із парними індексами

  ; Вивід результатів
  (printf "Максимальне значення серед елементів із непарними індексами: ~a (індекс ~a)\n"
          (car max-odd) (cadr max-odd))
  (printf "Мінімальне значення серед елементів із парними індексами: ~a (індекс ~a)\n"
          (car min-even) (cadr min-even)))

; Тестовий вектор
(define test-vec '#(7 2 9 4 6 1 3 5))
(process-vector test-vec)

