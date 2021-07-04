;; TODO fix arxiv metadata fetcher
;; TODO big cleanup/restructure

;; (defun ebib-pdf-auto-detect-doi (&optional filename end-page)
;;   "Invoke from ebib-index buffer.
;; This function requires the pdf-tools (pdf-tools.el) to be installed.
;; Scan for doi from page 1 upto (not including) END-PAGE (default 10) for pdf file."
;;   (interactive)
;;   (require 'pdf-tools nil t)
;;   (if (eq major-mode 'ebib-index-mode)
;;       (let* (doi-line
;;             ;; (isbn "")
;;             (page 1)
;;             (file-field-value (ebib-get-field-value "file" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref))
;;             (file-path (or filename
;;                            (concat
;;                             (file-name-as-directory ebib-default-directory)
;;                             (substring-no-properties (ebib--select-file file-field-value 0 (ebib--get-key-at-point)))))))
;;         (unless end-page (setq end-page 10))
;;         (cond ((string= (url-file-extension file-path) ".pdf")
;;                (while (< page end-page) ; scanning from below because we want to find first instance of DOI
;;                  (let ((match (cdr (assoc 'edges (car (pdf-info-search-string
;;                                                        "doi"
;;                                                        page
;;                                                        file-path))))))
;;                    ;; (current-buffer)))))))
;;                    (setq page (1+ page))
;;                    (cond (match (setq doi-line
;;                                       (pdf-info-gettext
;;                                        (1- page)
;;                                        (car match)
;;                                        'line file-path))
;;                                 (setq page (1+ end-page))))))
;;                (cond (doi-line
;;                       (string-match "doi:[ ]*\\([^ \n]+\\)" doi-line)
;;                       (print (match-string 1 doi-line)))
;;                      (t nil)))
;;               (t nil)))
;;         (message "Should be invoked from *ebib-index* buffer")))

(load "./pdf-get-title.el")
(require 'org-ref-pdf)

(defalias 'ebib-pdf-auto-detect-doi 'org-ref-extract-doi-from-pdf)

(defmacro parse-print (data)
  `(if (called-interactively-p)
       (pp ,data)
    ,data))

(defun ebib-pdf-auto-detect-arxiv-id (pdf)
  "Try to extract a arxiv-id from a PDF file.
There may be more than one arxiv-id in the file. This function returns
all the ones it finds based on two patterns: doi: up to a quote,
bracket, space or end of line. dx.doi.org/up to a quote, bracket,
space or end of line.

If there is a trailing . we chomp it off. Returns a list of arxiv-id
strings, or nil.

"
  (interactive (list (buffer-file-name)))
  (with-temp-buffer
    (insert (shell-command-to-string (format "%s -l 2 %s -"
					                                   pdftotext-executable
					                                   (shell-quote-argument (dnd-unescape-uri pdf)))))
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward "arXiv:\\([^ ]*\\)" nil t)
	      (let ((id (match-string 1)))
	        (cl-pushnew id matches :test #'equal)))
      (parse-print matches))))

(defun ebib--fetch-metadata (doi)
  "Insert BibTeX entry matching DOI."
  (let ((biblio-synchronous t))
    (biblio-doi-forward-bibtex
     (biblio-cleanup-doi doi)
     (lambda (result)
       (with-temp-buffer
         (biblio-doi--insert
          (biblio-format-bibtex result biblio-bibtex-use-autokey)
          (current-buffer))
         (goto-char (point-min))
         (parsebib-read-entry
          (buffer-substring-no-properties 2
                                          (1- (search-forward "{")))))))))

(defun ebib-fetch-metadata (doi &optional title)
    (interactive "MDOI: " )
    (let* ((db ebib--cur-db)
           (current-file-kv (assoc-string "file"
                                          (ebib-db-get-entry (ebib--get-key-at-point)
                                                             ebib--cur-db)))
           (fetched-md (if (not (string= doi ""))
                           (ebib--fetch-metadata doi)
                           (biblio-lookup nil (pdf-get-title
                                               (substring
                                                (cdr (assoc-string "file"
                                                                        (ebib-db-get-entry (ebib--get-key-at-point)
                                                                                           ebib--cur-db)))
                                                1 -1)))
                           (user-error "Good now?"))))
      (ebib-db-set-entry (ebib--get-key-at-point)
                         ;; (append fetched-md (list current-file-kv))
                         fetched-md
                         db
                         'overwrite)
      (ebib-generate-autokey)
      (ebib--update-entry-buffer)))

;; (defun ebib-fetch-and-merge-metadata (doi)
;;   (interactive "MDOI: ")
;;   (let ((current-entry-md  (sort (ebib-db-get-entry (ebib--get-key-at-point) ebib--cur-db)
;;                                  (lambda (x y) (string-lessp (car x) (car y)))))
;;         (fetched-md (sort (ebib--fetch-metadata doi)
;;                           (lambda (x y) (string-lessp (car x) (car y))))))
;;     (print current-entry-md)
;;     (print fetched-md)))

(defun ebib-add-entry2 (file-path &optional doi)
  "Interactively add a new entry to the database."
  (let ((file-path (if (string-match "^http" file-path)
                       (let ((path "/tmp/temp.pdf"))
                         (url-copy-file file-path path t)
                         (if (> (file-attribute-size (file-attributes path)) 25000)
                             path
                           (user-error "Downloaded file size too small for PDF")))
                     file-path)))
  (interactive "f")
  (ebib--execute-when
    (dependent-db (ebib-dependent-add-entry))
    (real-db
     (let ((entry-alist (list)))
       (push (cons '=key= "importkey") entry-alist)
       (let ((new-key (ebib--add-entry-stub entry-alist ebib--cur-db)))
         (ebib-db-set-current-entry-key new-key ebib--cur-db)
         (ebib--insert-entry-in-index-sorted new-key t)
         (let ((doi (or doi (let ((dois (ebib-pdf-auto-detect-doi file-path)))
                              (when dois
                                (if (= (length dois) 1)
                                (car dois)
                                (completing-read "Select DOI: "
                                                 dois))))))
               (arxiv-id (car (last (ebib-pdf-auto-detect-arxiv-id file-path)))))
           (if (doi
                (ebib-fetch-metadata doi))
                 ;; (arxiv-id (arxiv-lookup arxiv-id))
             (biblio-lookup nil (pdf-get-title file-path))))
         (ebib-import-file nil file-path)))))))

(defun ebib-pdf-dnd-protocol (uri action)
  (if (eq major-mode 'ebib-index-mode)
      (let* ((path (substring-no-properties uri 7))
	           dois)
	      (cond
	       ((f-ext? path "pdf")
	        (setq dois (ebib-pdf-auto-detect-doi
		                  path))
	        (cond
	         ((null dois)
	          (warn "No doi found in %s" path)
	          nil)
	         ((= 1 (length dois))
	          ;; we do not need to get the pdf, since we have one.
            (ebib-add-entry2 path (car dois))
	            ;; we should copy the pdf to the pdf directory though
	          action)
           (t
	          (helm :sources `((name . "Select a DOI")
			                       (candidates . ,(org-ref-pdf-doi-candidates dois))
			                       (action . ebib-add-entry2)))
	          action)))))
    (let ((dnd-protocol-alist
           (rassq-delete-all
            'ebib-pdf-dnd-protocol
            (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))

	         ;; Multiple DOIs found


(add-to-list 'dnd-protocol-alist '("^file:" . ebib-pdf-dnd-protocol))

(defun ebib-biblio-selection-set-callback (bibtex _entry)
  "Add a BibTeX entry to the current Ebib database.
BIBTEX is the textual representation of the entry, ENTRY is its
metadata."
  (ebib-db-set-entry (ebib--get-key-at-point)
                     ;; (append fetched-md (list current-file-kv))
                     (with-temp-buffer
                       (insert bibtex)
                       (goto-char (point-min))
                       (parsebib-read-entry "article"))
                     ebib--cur-db
                     'overwrite)
  (ebib-generate-autokey)
  (ebib--update-entry-buffer))

(defun ebib-biblio-selection-set ()
  "Import the current entry in the `biblio.el' selection buffer into Ebib."
  (interactive)
  (biblio--selection-forward-bibtex #'ebib-biblio-selection-set-callback))
