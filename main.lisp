;;; 1. Допоміжні утиліти (Parsing CSV)
(defun split-string (string delimiter)
  "Розбиває рядок на список підрядків за роздільником"
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun parse-integer-safe (str)
  "Парсить ціле число з рядка, ігноруючи можливі пробіли"
  (if (or (null str) (string= str ""))
      0
      (parse-integer (string-trim " " str))))

(defun parse-float-safe (str)
  "Спрощений парсинг float"
  (if (or (null str) (string= str ""))
      0.0
      (let ((*read-eval* nil))
        (read-from-string (string-trim " " str)))))

;;; 2. Конструктори записів (Асоціативні списки)
;; Таблиця 1: Виробники (id, name, country, year)
(defun make-manufacturer (id name country year)
  (list (cons :id id)
        (cons :name name)
        (cons :country country)
        (cons :year year)))

;; Таблиця 2: Дрони (id, model, manufacturer-id, range-km, payload-kg)
(defun make-drone (id model manufact-id range payload)
  (list (cons :id id)
        (cons :model model)
        (cons :manufact-id manufact-id)
        (cons :range range)
        (cons :payload payload)))

;;; 3. Зчитування з файлу
(defun map-csv-line-to-record (line type)
  "Перетворює рядок CSV на асоціативний список відповідно до типу"
  (let ((parts (split-string line #\,)))
    (case type
      (:manufacturer
       (make-manufacturer
        (parse-integer-safe (nth 0 parts))
        (string-trim " " (nth 1 parts))
        (string-trim " " (nth 2 parts))
        (parse-integer-safe (nth 3 parts))))
      (:drone
       (make-drone
        (parse-integer-safe (nth 0 parts))
        (string-trim " " (nth 1 parts))
        (parse-integer-safe (nth 2 parts))
        (parse-integer-safe (nth 3 parts))
        (parse-float-safe (nth 4 parts)))))))

(defun read-csv-file (filepath type)
  "Зчитує CSV файл і повертає список записів (skip header)"
  (if (not (probe-file filepath))
      (format t "Помилка: Файл ~A не знайдено!~%" filepath)
      (with-open-file (stream filepath :direction :input)
        (read-line stream nil) ; Пропускаємо заголовок
        (loop for line = (read-line stream nil)
              while line
              collect (map-csv-line-to-record line type)))))

;;; 4. Функція SELECT
(defun get-value (record key)
  "Отримує значення з асоціативного списку за ключем"
  (cdr (assoc key record)))

(defun check-filters (record filters)
  "Перевіряє, чи відповідає запис всім переданим фільтрам"
  (loop for (key value) on filters by #'cddr
        always (equal (get-value record key) value)))

(defun select (filepath type)
  "Повертає лямбду для вибірки даних"
  (let ((records (read-csv-file filepath type)))
    (lambda (&rest filters)
      (if (null filters)
          records
          (remove-if-not #'(lambda (r) (check-filters r filters)) records)))))

;;; 5. Запис у файл
(defun record-to-csv-string (record type)
  "Формує CSV рядок із запису"
  (case type
    (:manufacturer
     (format nil "~A, ~A, ~A, ~A"
             (get-value record :id)
             (get-value record :name)
             (get-value record :country)
             (get-value record :year)))
    (:drone
     (format nil "~A, ~A, ~A, ~A, ~F"
             (get-value record :id)
             (get-value record :model)
             (get-value record :manufact-id)
             (get-value record :range)
             (get-value record :payload)))))

(defun save-to-csv (filepath records type)
  "Зберігає список записів у файл"
  (with-open-file (stream filepath :direction :output :if-exists :supersede)
    (format stream "~A~%"
            (case type
              (:manufacturer "ID,Name,Country,Year")
              (:drone "ID,Model,Manufacturer_ID,Range_km,Payload_kg")))
    (dolist (r records)
      (format stream "~A~%" (record-to-csv-string r type)))))

;;; 6. Конвертація та Вивід
(defun alist-to-hashtable (alist-record)
  "Конвертує один запис (alist) у геш-таблицю"
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist-record)
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))

(defun convert-list-to-hashtables (records)
  (mapcar #'alist-to-hashtable records))

(defun pretty-print (records)
  (if (null records)
      (format t "~&Result is EMPTY.~%")
      (dolist (r records)
        (format t "~&--------------------------------~%")
        (dolist (pair r)
          (format t "~20A : ~A~%" (car pair) (cdr pair)))))
  (format t "~&--------------------------------~%"))

;;; Демонстрація роботи
(defun create-demo-files ()
  "Створення тестових файлів для демонстрації"
  (format t ">>> Створення файлів 'manufacturesrs.csv' та 'drones.csv'...~%")
  (with-open-file (stream "manufacturesrs.csv" :direction :output :if-exists :supersede)
    (format stream "ID,Name,Country,Year~%")
    (format stream "1,SkySystems,USA,2010~%")
    (format stream "2,UkrJet,Ukraine,2018~%")
    (format stream "3,ChinaWing,China,2020~%"))
  
  (with-open-file (stream "drones.csv" :direction :output :if-exists :supersede)
    (format stream "ID,Model,Manufacturer_ID,Range_km,Payload_kg~%")
    (format stream "101,Hawk-1,1,150,5.5~%")
    (format stream "102,Hawk-2,1,300,10.0~%")
    (format stream "103,Borsuk,2,50,2.5~%")
    (format stream "104,Leleka,2,100,0.0~%")
    (format stream "105,Dragon,3,50,1.0~%")))

(defun run-demo ()
  ;; 1. Підготовка
  (create-demo-files)

  ;; 2. Робота з виробниками
  (format t "~%>>> 1. Зчитування ВСІХ виробників:~%")
  (let* ((manuf-selector (select "manufacturesrs.csv" :manufacturer))
         (all-manufs (funcall manuf-selector)))
    (pretty-print all-manufs)

    ;; 3. Фільтрація
    (format t "~%>>> 2. Фільтрація: Шукаємо виробників з України (Country = Ukraine):~%")
    (let ((ukr-manufs (funcall manuf-selector :country "Ukraine")))
      (pretty-print ukr-manufs)))

  ;; 4. Робота з дронами
  (format t "~%>>> 3. Зчитування дронів та фільтрація за Manufacturer_ID = 1:~%")
  (let* ((drone-selector (select "drones.csv" :drone))
         (my-drones (funcall drone-selector :manufact-id 1)))
    (pretty-print my-drones)
    
    ;; 5. Конвертація
    (format t "~%>>> 4. Конвертація результату (Alist) у Hash Table:~%")
    (let ((hashed (convert-list-to-hashtables my-drones)))
      (format t "Тип першого елемента: ~A~%" (type-of (first hashed)))
      (format t "Вміст першої геш-таблиці:~%")
      (maphash #'(lambda (k v) (format t "   ~A => ~A~%" k v)) (first hashed)))

    ;; 6. Запис у файл
    (format t "~%>>> 5. Запис відфільтрованих дронів у файл 'output_drones.csv'...~%")
    (save-to-csv "output_drones.csv" my-drones :drone)
    (format t "Запис виконано.~%"))

  ;; 7. Перевірка запису
  (format t "~%>>> 6. Перевірка: зчитування новоствореного файлу 'output_drones.csv':~%")
  (let ((check-selector (select "output_drones.csv" :drone)))
    (pretty-print (funcall check-selector))))

(run-demo)