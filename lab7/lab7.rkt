#lang racket

; ===== Запис речень у текстовий файл =====
(define (write-sentences-to-file sentences filename)
  (let ((port (open-output-file filename #:mode 'text #:exists 'replace)))
    (for-each (lambda (sentence) (displayln sentence port)) sentences)
    (close-output-port port)))

; ===== Зчитування речень із текстового файлу =====
(define (read-sentences-from-file filename)
  (let ((port (open-input-file filename)))
    (let loop ((lines '()))
      (let ((line (read-line port 'any)))
        (if (eof-object? line)
            (begin
              (close-input-port port)
              (reverse lines)) ; Повертаємо список речень у правильному порядку
            (loop (cons line lines)))))))

; ===== Видалення крапки з останнього слова =====
(define (remove-period word)
  (if (string-suffix? word ".")
      (substring word 0 (- (string-length word) 1))
      word))

; ===== Сортування слів у зворотному до алфавітного порядку =====
; ===== Сортування слів у зворотному до алфавітного порядку без врахування регістру =====
(define (reverse-alphabetical-sort words)
  (sort words
        (lambda (w1 w2)
          (let loop ((chars1 (string->list (string-downcase w1))) ; Перетворюємо на нижній регістр
                     (chars2 (string->list (string-downcase w2))))
            (cond
              [(null? chars1) #f] ; Якщо слово w1 закінчилось, воно менше
              [(null? chars2) #t] ; Якщо слово w2 закінчилось, w1 більше
              [(char<? (car chars1) (car chars2)) #f] ; w1 менше, якщо його символ менший
              [(char>? (car chars1) (car chars2)) #t] ; w1 більше, якщо його символ більший
              [else (loop (cdr chars1) (cdr chars2))]))))) ; Переходимо до наступних символів



; ===== Обробка речень: сортування та додавання крапки =====
(define (process-sentences sentences)
  (map (lambda (sentence)
         (let* ((words (string-split sentence " ")) ; Розбиваємо речення на слова
                (cleaned-words (map remove-period words)) ; Видаляємо крапку з усіх слів
                (sorted-words (reverse-alphabetical-sort cleaned-words))) ; Сортуємо слова
           (string-append (string-join sorted-words " ") "."))) ; Додаємо крапку в кінці
       sentences))

; ===== Запис результату до нового файлу =====
(define (write-processed-to-file processed filename)
  (let ((port (open-output-file filename #:mode 'text #:exists 'replace)))
    (for-each (lambda (line) (displayln line port)) processed)
    (close-output-port port)))

; ===== Головна процедура =====
(define (main input-path output-path)
  (define sentences '("Hello world."
                      "Racket programming is fun."
                      "This is a test sentence."
                      "Apple banana orange."
                      "Zoo apple zebra.")) ; Приклад речень

  ; Записуємо початкові речення у вхідний файл
  (write-sentences-to-file sentences input-path)
  
  ; Зчитуємо речення з файлу
  (define read-sentences (read-sentences-from-file input-path))
  (displayln "Прочитані речення:")
  (for-each displayln read-sentences)

  ; Відступ
  (displayln "\n===================\n")

  ; Обробляємо речення
  (define processed-sentences (process-sentences read-sentences))
  (displayln "Оброблені речення:")
  (for-each displayln processed-sentences)

  ; Записуємо оброблені речення до вихідного файлу
  (write-processed-to-file processed-sentences output-path)

  ; Відступ перед завершенням
  (displayln "\nРечення записано до файлу.")
  (displayln "===================\n"))


; Запуск головної процедури з шляхами до файлів
(main "D:\\4kurs\\FuncProg\\lab7\\input.txt" "D:\\4kurs\\FuncProg\\lab7\\output.txt")

