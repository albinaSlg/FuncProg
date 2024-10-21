#lang racket

; Розподіл дзвінків між операторами
(define (assign-calls calls operators)
  (define (shortest-queue queues)
    (argmin length queues)) ; Знаходимо оператора з найкоротшою чергою

  ; Рекурсивно розподіляємо дзвінки
  (define (distribute calls operators)
    (if (null? calls)
        operators ; Якщо дзвінків немає, повертаємо список операторів
        (let* ((current-call (car calls))
               (remaining-calls (cdr calls))
               (target-operator (shortest-queue operators)))
          (printf "Нерозподілені дзвінки: ~a\n" remaining-calls)
          (printf "Дзвінок ~a направляється до оператора ~a\n"
                  current-call
                  (+ 1 (index-of operators target-operator)))
          (distribute remaining-calls ; Розподіляємо залишок дзвінків
                      (map (lambda (op)
                             (if (eq? op target-operator)
                                 (append op (list current-call)) ; Додаємо дзвінок
                                 op))
                           operators))))) ; Не змінюємо інші списки операторів
  (distribute calls operators))

; Моделюємо процес обслуговування клієнтів
(define (simulate-call-center calls num-operators)
  (define operators (make-list num-operators '())) ; Ініціалізуємо операторів
  (define operator-queues (assign-calls calls operators)) ; Розподіляємо дзвінки

  ; Виводимо сценарій обслуговування
  (define (process-operators queues)
    (for ([queue queues] [i (in-naturals 1)])
      (printf "Оператор ~a завершує обслуговування дзвінків: ~a\n" i queue)))
  
  (process-operators operator-queues))

; Пошук індексу оператора (для виводу)
(define (index-of lst elem)
  (let loop ((lst lst) (idx 0))
    (cond
      [(null? lst) #f]
      [(eq? (car lst) elem) idx]
      [else (loop (cdr lst) (+ idx 1))])))

; Вхідні дані
(define calls '(1 2 3 4 5 6 7 8 9 10)) ; Черга дзвінків
(define num-operators 3) ; Кількість операторів

; Запуск симуляції
(simulate-call-center calls num-operators)

