;; TODO first filer out elements matching 'arXiv'

;; TODO add poppler (pdftohtml) backend. Unfortunately `pdftohtml' errors on my
;; system.

;; (defun height (x)
;;   (- (nth 3 x) (nth 1 x)))

(defun positivep (num)
  (> num 0))

;; (defun total-region (regions)
;;   (let ((first (car regions))
;;         (last (car (last regions))))
;;     (list (nth 0 first) (nth 1 first) (nth 2 last) (nth 3 last))))


;; (defun pdf-get-title ()
;;   (interactive)
;;   (let* ((text-regions (pdf-info-textregions 1))
;;          (title-regions (list (car text-regions)))
;;          (title-region (total-region
;;                         (dolist (x (cdr text-regions) (nreverse title-regions))
;;                           (cond ((= (height x) (height (car title-regions)))
;;                                  (push x title-regions))
;;                                 ((> (height x) (height (car title-regions)))
;;                                  (setq title-regions (list x))))))))
;;     (print (pdf-info-gettext 1 title-region))))

;; (defun pdf-to-html ()
;;   (interactive)
;;   (let ((file (buffer-file-name)))
;;     (with-temp-file "~/test.html"
;;       (insert (shell-command-to-string
;;                (format "pdftotext -f 1 -l 1 -bbox-layout '%s' -"
;;                        file))))))

;; (defun pdf-to-html ()
;;   (interactive)
;;   (let ((file (buffer-file-name)))
;;     (with-temp-file "~/test.html"
;;       (insert (shell-command-to-string
;;                (format "pdftohtml -f 1 -l 1 '%s' -"
;;                        file))))))

;; (defun pdf-get-lines ()
;;   (interactive)
;;   (let ((file (buffer-file-name)))
;;     (with-temp-buffer
;;       (insert (shell-command-to-string
;;                ;; (format "pdftotext -f 1 -l 1 -bbox-layout '%s' -"
;;                (format "mutool draw -F html '%s' 1"
;;                        file)))
;;       (print (dom-by-tag (libxml-parse-html-region (point-min) (point-max)) 'span)))))

;; (defun pdf-get-words ()
;;   (interactive)
;;   (let ((file (buffer-file-name)))
;;     (with-temp-buffer
;;       (insert (shell-command-to-string
;;                (format "pdftotext -f 1 -l 1 -bbox '%s' -"
;;                        file)))
;;       (print (dom-by-tag (libxml-parse-html-region (point-min) (point-max)) 'word)))))

;; (defun pdf-dom-element-height (element)
;;   (let ((size (cadr element)))
;;     (- (string-to-number (alist-get 'ymax size))
;;        (string-to-number (alist-get 'ymin size)))))

;; (defun pdf-get-title-lines (dom)
;;   (interactive (list (pdf-get-lines)))
;;   (let* ((max-lines (if (< (length dom) 10)
;;                                  (length dom)
;;                                10))
;;          (lines (cl-subseq dom 0 max-lines))
;;          (maximum (apply 'max (mapcar (lambda (x) (pdf-dom-element-height x)) lines)))
;;          (title-lines (list (car lines))))
;;     (dolist (x (cdr lines) title-lines)
;;       (when (< (pdf-line-height x) 25)
;;         (pcase (- (pdf-dom-element-height x) (pdf-dom-element-height (car title-lines)))
;;           (0 (push x title-lines))
;;           ((pred positivep) (setq title-lines (list x))))))
;;     (print (mapconcat (lambda (x) (caddr x))
;;                 (pdf-get-title-words (car title-lines))
;;                 " "))))

;; (defun pdf-get-title-words (line)
;;   (dom-by-tag line 'word))


;; (apply 'max (mapcar (lambda (x) (pdf-dom-element-height x)) (cl-subseq test 0 5)))

;; (dolist (x (cl-subseq test 0 5))
;;   (print x))

(defun pdf-get-span-font-size (span)
  (string-to-number
   (cadr (assoc-string "font-size"
                       (mapcar (lambda (x) (split-string x ":"))
                               (split-string (dom-attr span 'style) ";"))))))

(defun pdf-get-spans (file)
  (interactive (list (buffer-file-name)))
    (with-temp-buffer
      (insert (shell-command-to-string
               (format "mutool draw -F html '%s' 1"
                       file)))
      (dom-by-tag (libxml-parse-html-region (point-min) (point-max)) 'span)))

(defun pdf-get-title-spans (dom)
  ;; (interactive (list (pdf-get-spans)))
  (let* (
         ;; (max-spans (if (< (length dom) 30)
         ;;                (length dom)
         ;;              30))
         ;; (spans (cl-subseq dom 0 max-spans))
         ;; (maximum (apply 'max (mapcar (lambda (x) (pdf-get-span-font-size x)) spans)))
         ;; (title-spans (list (car spans))))
         (dom (if (string-match "arXiv" (car (last (car dom))))
                  (cdr dom)
                dom))
         (title-spans (list (car dom))))
    (dolist (x (cdr dom) title-spans)
      ;; conditional to exclude Initials (drop caps)
      ;; moreover, we assume that a title contains more then 3 letters
      (when (> (length (car (last x))) 3)
        (pcase (- (pdf-get-span-font-size x) (pdf-get-span-font-size (car title-spans)))
          (0 (push x title-spans))
          ((pred positivep) (setq title-spans (list x))))))
    (mapconcat (lambda (x) (car (last x)))
               title-spans
               " ")))

(defun pdf-get-title (file)
  (interactive (list (buffer-file-name)))
  (pdf-get-title-spans (pdf-get-spans file)))
