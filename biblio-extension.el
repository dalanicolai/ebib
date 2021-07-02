(defun biblio-arxiv-id--url (query)
  "Create an arXiv url to look up QUERY."
  (format "https://export.arxiv.org/api/query?id_list=%s"
          (url-encode-url query)))

;;;###autoload
(defun biblio-arxiv-id-backend (command &optional arg &rest more)
  "A arXiv backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "arXiv-id")
    (`prompt "arXiv id: arXiv: ")
    (`url (biblio-arxiv-id--url arg))
    (`parse-buffer (biblio-arxiv--parse-search-results))
    (`forward-bibtex (biblio-arxiv--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-arxiv-id-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-arxiv-id-backend)

(defun biblio-arxiv-id-lookup (&optional ID)
  "Start an arXiv search for ID, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-arxiv-id-backend ID))
