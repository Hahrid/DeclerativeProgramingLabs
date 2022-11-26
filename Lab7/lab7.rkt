#lang racket
; Запис даних у файл
(define (writeDataToFile data file) (display-lines-to-file data file #:exists 'replace #:mode 'text))
; Читання нових рядкв з файла
(define (nextLine file)
(let ((line (read-line file 'any)))
(if (eof-object? line)
'()
(begin(displayln line)
(append (list line) (nextLine file)))
)
)
)

(define path "D:\\Data\\")
(define words (list
"My text for seventh lab"
"String for removing"
"The last string in file"
)
)

; Запис даних
(writeDataToFile words ( string-append path "input.txt"))
(display "\nВміст файлу:\n")
; Читання даних
(define readData (call-with-input-file ( string-append path
"input.txt") nextLine))

; Видалення із файлу за заданим шаблоном
(define removeData(
remove "String for removing" (list
"My text for seventh lab"
"String for removing"
"The last string in file"
)))

; Запис модифікованих даних в новий файл, та виведення їх на екран
(writeDataToFile removeData (string-append path "output.txt"))
(display "\nДані з початкового файлу були успішно змінені та записані у новий файл!")
(newline)
(display "\nВміст нового файлу: ")
(newline)
(define rm-data (call-with-input-file ( string-append path "output.txt")
nextLine))