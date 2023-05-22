(defun jisho-word->japanese-part (jisho-word)
  (list (gethash "word" (elt (gethash "japanese" jisho-word) 0))
	(gethash "reading" (elt (gethash "japanese" jisho-word) 0))))

(defun jisho-word->english-part (jisho-word)
  (gethash "english_definitions" (elt (gethash "senses" jisho-word) 0)))

(defun word->drill (word)
  (if (car word)
      (kanji-word->drill word)
    (kana-word->drill (cdr word))))

(defun my-presorted-completion-table (completions)
  (lambda (string pred action)
    (if (eq action 'metadata)
	`(metadata (display-sort-function . ,(function identity)))
      (complete-with-action action completions string pred))))

(defvar *jisho-results* ())

(defun jisho-search->completing-read ()
  (interactive)
  (let* ((search-term (read-string "Search JISHO: "))
	 (url (concat "https://www.jisho.org/api/v1/search/words?keyword=" search-term)) ; Get correct URL to access Jisho API ; ; ;
	 ;; Get JSON File from URL
	 (contents (with-temp-buffer
		     (url-insert-file-contents url)
		     (json-parse-buffer :array-type 'list)))
	 ;; JSON File contains metadata on status, then a list of words
	 (status (gethash "meta" contents))
	 (words (gethash "data" contents))
	 ;; We will iterate over all words via word, and return a certain list in results
	 (word)
	 (vertico-sort-override-function (function identity)))
    ;; Build Results
    (setq *jisho-results* ())
    (while words
      (setq word (car words))
      (setq total-word (append (jisho-word->japanese-part word) (jisho-word->english-part word)))
      (setq *jisho-results* (append *jisho-results* (list (cons (string-join total-word " ") total-word))))
      (setq words (cdr words)))
    (alist-get
     (completing-read "Results: " *jisho-results*)
     *jisho-results* nil nil 'equal)))

(defun kanji-word->drill (word)
  (apply 'format "{{%s}} :Japanese:\n{{%s}} {{%s}}\n" word))

(defun kana-word->drill (word)
  (apply 'format "{{%s}} :Japanese:\n{{%s}}\n" word))

  (defun org-fc-type-cloze-single-complement-init (type)
    "Initialize the current heading for use as a cloze card of subtype TYPE.
Processes all holes in the card text."
    (interactive (list
                  (intern
                   (completing-read "Cloze Type: " org-fc-type-cloze-types))))
    (unless (member type org-fc-type-cloze-types)
      (error "Invalid cloze card type: %s" type))
    (org-fc--init-card "cloze")
    (org-fc-type-cloze-update)
    (org-set-property org-fc-type-cloze-type-property (format "%s" type)))

(defun org-fc-type-cloze-setup (position)
  "Prepare POSITION of a cloze card for review."
  (setq org-fc-type-cloze--text nil)
  (setq org-fc-type-cloze--hint nil)
  (outline-hide-subtree)
  (org-show-entry)
  (org-fc-type-cloze-hide-holes (string-to-number position)))

(defun org-fc-type-cloze-flip ()
  "Flip a cloze card."
  (org-show-children)
  (overlay-put org-fc-type-cloze--text 'invisible nil)
  (org-fc-show-latex)
  ;; Remove all overlays in the region of the hint to get rid of
  ;; latex overlays in the hint, then hide the region again.
  (let* ((hint-start (overlay-start org-fc-type-cloze--hint))
         (hint-end (overlay-end org-fc-type-cloze--hint)))
    (remove-overlays)
					;(org-fc-hide-region hint-start hint-end)
    ))

(defun org-fc-type-cloze-update ()
  "Update the review data & deletions of the current heading."
  (let* ((end (org-fc-type-cloze--end))
         (hole-id (1+ (org-fc-type-cloze-max-hole-id)))
         ids)
    (save-excursion
      (while (re-search-forward org-fc-type-cloze-hole-re end t)
        (let ((id (match-string 3))
              (hole-end (match-end 0)))
          (unless id
            (setq id hole-id)
            (cl-incf hole-id 1)
            (let ((id-str (number-to-string id)))
              (cl-incf end (+ 1 (length id-str)))
              (goto-char hole-end)
              (backward-char)
              (insert "@" id-str)))
          (push (format "%s" id) ids))))
    (org-set-property
     org-fc-type-cloze-max-hole-property
     (format "%s" (1- hole-id)))
    (org-fc-review-data-update (reverse ids))))

(org-fc-register-type
 'cloze
 'org-fc-type-cloze-setup
 'org-fc-type-cloze-flip
 'org-fc-type-cloze-update)



(defun org-fc-type-cloze-hide-holes (position)
  "Hide holes of a card of TYPE in relation to POSITION."
  (org-fc-with-point-at-entry
   (let* ((type (intern (org-entry-get (point) org-fc-type-cloze-type-property)))
          (end (1+ (org-fc-type-cloze--end)))
          (holes-index (org-fc-type-cloze--parse-holes position end))
          (holes (car holes-index))
          (current-index (cdr holes-index)))
     (cl-loop
      for i below (length holes)
      for (hole-beg hole-end text-beg text-end hint-beg hint-end) in holes
      do
      (progn
        ;; Fake position if there is no hint
        (unless hint-beg (setq hint-beg text-end))
        (unless hint-end (setq hint-end text-end))
        (cond
         ;; If the hole is the one currently being reviewed, hide all
         ;; the hole markup, hide the answer, format the hint as
         ;; "[...hint]" and set the font for the whole hole.
         ((= i current-index)
	  (cond ((eq type 'single)
		 (org-fc-hide-region hole-beg text-beg "")
		 (remove-overlays text-beg text-end)
		 (setq org-fc-type-cloze--text
		       (org-fc-make-overlay text-beg text-end ;'invisible t
					    ))
		 (org-fc-hide-region text-end hint-beg "")
		 (setq org-fc-type-cloze--hint
		       (org-fc-overlay-surround
			(org-fc-make-overlay hint-beg hint-end)
			"" "" 'org-fc-type-cloze-hole-face))
		 (org-fc-hide-region hint-end hole-end "")
		 (org-fc-make-overlay
		  hole-beg hole-end
		  'face 'org-fc-type-cloze-hole-face)
		 )
		(t
		 (org-fc-hide-region hole-beg text-beg "")
		 (remove-overlays text-beg text-end)
		 (setq org-fc-type-cloze--text
		       (org-fc-make-overlay text-beg text-end 'invisible t))
		 (org-fc-hide-region text-end hint-beg "")
		 (setq org-fc-type-cloze--hint
		       (org-fc-overlay-surround
			(org-fc-make-overlay hint-beg hint-end)
			"[..." "]" 'org-fc-type-cloze-hole-face))
		 (org-fc-hide-region hint-end hole-end "")
		 (org-fc-make-overlay
		  hole-beg hole-end
		  'face 'org-fc-type-cloze-hole-face))
		))
         ;; If the text of another hole should be visible,
         ;; hide the hole markup and the hint
         ((org-fc-type-cloze--hole-visible-p type i current-index)
          (org-fc-hide-region hole-beg text-beg)
          (org-fc-hide-region text-end hole-end))
         ;; If the text of another hole should not be visible,
         ;; hide the whole hole
         (t (org-fc-hide-region hole-beg hole-end "..."))))))))

(defun jisho->fc ()
  (interactive)
  (org-roam-with-file "~/notes/20211210212624-japanese.org" t
    (end-of-buffer)
    (insert (concat "* " (word->drill (jisho-search->completing-read))))
    (org-fc-type-cloze-init 'single)))

					;(jisho->fc)

