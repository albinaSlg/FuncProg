#lang racket

; ####### Реалізація черги на основі списків #########
(define (make-queue)
  '()) ; Порожня черга

; Перевірка, чи черга порожня
(define (null-queue? q)
  (null? q))

; Додавання нового елемента в кінець черги
(define (enqueue q e)
  (append q (list e)))

; Вилучення елемента з початку черги
(define (dequeue q)
  (if (null-queue? q)
      (list 'Empty '()) ; Повертаємо 'Empty', якщо черга порожня
      (list (car q) (cdr q)))) ; Повертаємо перший елемент і нову чергу


; ####### Реалізація стеку на основі списків #########
(define (make-stack)
  '()) ; Порожній стек

; Додавання елемента в стек
(define (stack-push stack e)
  (cons e stack))

; Вилучення елемента зі стеку
(define (stack-pop stack)
  (if (null? stack)
      (list 'Empty '()) ; Повертаємо 'Empty', якщо стек порожній
      (list (car stack) (cdr stack)))) ; Повертаємо верхній елемент і новий стек


; ####### Головна процедура #########
(define (main chars)
  (define vowels '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U)) ; Голосні літери
  (define queue (make-queue)) ; Створення порожньої черги
  (define stack (make-stack)) ; Створення порожнього стеку

  ; Додавання елементів у чергу
  (for ([char chars])
    (set! queue (enqueue queue char)))

  ; Вилучення з черги та додавання голосних у стек
  (let loop ((q queue) (s stack))
    (if (null-queue? q)
        (begin
          (displayln "Стек голосних літер:")
          (for ([v s])
            (displayln v)))
        (let* ((deq-result (dequeue q))
               (ch (car deq-result))
               (new-queue (cadr deq-result)))
          (if (member ch vowels)
              (loop new-queue (stack-push s ch))
              (loop new-queue s))))))

; Виконання головної процедури
(main '(#\a #\b #\c #\d #\e #\f #\g #\o #\u)) ; Черга символів

