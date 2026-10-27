;;; elfeed-fill-ytdlp.el --- Fetch YouTube channels/playlists into elfeed via yt-dlp -*- lexical-binding: t; -*-

;; YouTube's /feeds/videos.xml endpoint returns 404/500 for most requests
;; (October 2026), so this file reads the channel or playlist page with
;; `yt-dlp --flat-playlist' instead, synthesises the plist shape that
;; `elfeed-tube--entry-create' consumes, and hands the result to
;; `elfeed-db-add'.  It does not depend on Invidious.
;;
;; Loading this file adds `elfeed-fill-ytdlp-fetch' to
;; `elfeed-fetch-functions', so `elfeed-update' fetches every YouTube feed in
;; `elfeed-feeds' through yt-dlp, listing the newest
;; `elfeed-fill-ytdlp-update-limit' videos.  Other feeds still use curl.
;;
;; The backfill commands use the same code with no limit:
;;   (a) M-x elfeed-fill-ytdlp-from-url RET <playlist-or-channel URL> RET
;;       Registers the corresponding RSS feed with elfeed (if absent) and
;;       backfills every video into the elfeed db.
;;
;;   (b) Place point on a YouTube feed entry in *elfeed-search* and call
;;       M-x elfeed-fill-ytdlp.  A numeric prefix argument limits the count.
;;
;; Example for MIT 6.041 (Probabilistic Systems Analysis, Tsitsiklis):
;;   (elfeed-fill-ytdlp-from-url
;;    "https://www.youtube.com/playlist?list=PLUl4u3cNGP61MdtwGTqZA0MreSaDybji8"
;;    '(lecture probability mit 6041))

(require 'elfeed)
(require 'elfeed-db)
(require 'elfeed-tube)
(require 'cl-lib)

(defvar elfeed-fill-ytdlp-update-limit 30
  "Videos listed per YouTube feed by `elfeed-update'; nil lists every video.
yt-dlp reads about 30 videos per page, so 30 costs the same as 15.")

(defvar elfeed-fill-ytdlp-max-processes 4
  "Most yt-dlp processes run at once.")

(defvar elfeed-fill-ytdlp--queue nil
  "Pending jobs, each a list (TARGET LIMIT CALLBACK), oldest first.")

(defvar elfeed-fill-ytdlp--active 0
  "Number of yt-dlp processes running.")

(defconst elfeed-fill-ytdlp--print-format
  "%(id)s\t%(title|)s\t%(uploader,playlist_uploader,channel|)s\t%(timestamp|)s"
  "yt-dlp --print template parsed by `elfeed-fill-ytdlp--parse-rows'.")

(defun elfeed-fill-ytdlp--resolve-handle (url)
  "Ask yt-dlp to resolve a /@handle URL to a channel id (UC...). Nil on failure."
  (with-temp-buffer
    (let ((exit (call-process "yt-dlp" nil t nil
                              "--flat-playlist" "--playlist-items" "1"
                              "--print" "%(playlist_channel_id|channel_id|)s"
                              url)))
      (and (zerop exit)
           (let ((s (string-trim (buffer-string))))
             (and (string-prefix-p "UC" s) s))))))

(defun elfeed-fill-ytdlp--ids-from-url (url)
  "Return (TYPE . ID) for a YouTube channel/playlist URL, or nil.
TYPE is the symbol `channel' or `playlist'."
  (cond
   ((string-match "[?&]list=\\([^&]+\\)" url)
    (cons 'playlist (match-string 1 url)))
   ((string-match "playlist_id=\\([^&]+\\)" url)
    (cons 'playlist (match-string 1 url)))
   ((string-match "channel_id=\\([^&]+\\)" url)
    (cons 'channel (match-string 1 url)))
   ((string-match "/channel/\\([^/?]+\\)" url)
    (cons 'channel (match-string 1 url)))
   ((string-match-p "/@[^/?]+" url)
    (when-let ((cid (elfeed-fill-ytdlp--resolve-handle url)))
      (cons 'channel cid)))))

(defun elfeed-fill-ytdlp--feed-url-for (type id)
  "Build the canonical YouTube RSS URL for TYPE (`channel' or `playlist') ID."
  (pcase type
    ('channel  (format "https://www.youtube.com/feeds/videos.xml?channel_id=%s" id))
    ('playlist (format "https://www.youtube.com/feeds/videos.xml?playlist_id=%s" id))))

(defun elfeed-fill-ytdlp--enumerate-target (type id)
  "Build the URL to feed yt-dlp for TYPE/ID."
  (pcase type
    ('channel  (format "https://www.youtube.com/channel/%s/videos" id))
    ('playlist (format "https://www.youtube.com/playlist?list=%s" id))))

(defun elfeed-fill-ytdlp--entry-by-video-id (video-id)
  "Look up the elfeed entry for VIDEO-ID, or nil."
  (elfeed-db-get-entry (cons "www.youtube.com" (concat "yt:video:" video-id))))

(defun elfeed-fill-ytdlp--parse-ts (s)
  "Parse a yt-dlp Unix-timestamp print field. Return float or nil."
  (and s (not (string-empty-p s)) (not (string= s "NA"))
       (let ((n (string-to-number s)))
         (and (> n 0) (float n)))))

(defun elfeed-fill-ytdlp--parse-rows (buf)
  "Parse yt-dlp output in BUF into a list of (ID TITLE UPLOADER TIMESTAMP) lists.
TIMESTAMP is the Unix epoch as a float, or nil if yt-dlp did not return one.
UPLOADER falls back to playlist_uploader/channel when the per-video
uploader field is missing (typical for `--flat-playlist' on channels)."
  (let ((rows '()))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (let ((parts (split-string
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))
                      "\t")))
          ;; Unavailable playlist videos come back with no title.
          (when (and (>= (length parts) 2)
                     (not (string-empty-p (nth 0 parts)))
                     (not (string-empty-p (nth 1 parts)))
                     (not (member (nth 1 parts) '("[Private video]" "[Deleted video]"))))
            (push (list (nth 0 parts)
                        (nth 1 parts)
                        (nth 2 parts)
                        (elfeed-fill-ytdlp--parse-ts (nth 3 parts)))
                  rows)))
        (forward-line 1)))
    (nreverse rows)))

(defun elfeed-fill-ytdlp--last-error (buf)
  "Return the last ERROR line yt-dlp wrote to BUF, or its last line."
  (with-current-buffer buf
    (goto-char (point-max))
    (string-trim
     (if (re-search-backward "^ERROR: .*" nil t)
         (match-string 0)
       (buffer-substring-no-properties
        (save-excursion (forward-line -1) (point)) (point-max))))))

(defun elfeed-fill-ytdlp--start (target limit callback)
  "Run yt-dlp on TARGET, listing at most LIMIT videos (nil means all).
Call CALLBACK with (ROWS nil) on success or (nil MESSAGE) on failure."
  (let* ((out (generate-new-buffer " *yt-dlp*"))
         (err (generate-new-buffer " *yt-dlp-stderr*"))
         (err-proc (make-pipe-process :name "yt-dlp-stderr" :buffer err
                                      :noquery t :sentinel #'ignore)))
    (make-process
     :name "yt-dlp"
     :buffer out
     :stderr err-proc
     :coding 'utf-8
     :noquery t
     :command (append (list "yt-dlp" "--flat-playlist" "--ignore-errors"
                            "--extractor-args" "youtubetab:approximate_date")
                      (and limit (list "--playlist-items" (format "1:%d" limit)))
                      (list "--print" elfeed-fill-ytdlp--print-format target))
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (unwind-protect
             (let ((rows (elfeed-fill-ytdlp--parse-rows out))
                   (code (process-exit-status proc)))
               ;; --ignore-errors exits 1 when any single video fails.
               (if (or rows (zerop code))
                   (funcall callback rows nil)
                 (funcall callback nil
                          (format "yt-dlp exit %d: %s" code
                                  (elfeed-fill-ytdlp--last-error err)))))
           (kill-buffer out)
           (kill-buffer err)
           (cl-decf elfeed-fill-ytdlp--active)
           (elfeed-fill-ytdlp--start-next)))))))

(defun elfeed-fill-ytdlp--start-next ()
  "Start queued jobs until `elfeed-fill-ytdlp-max-processes' are running."
  (while (and elfeed-fill-ytdlp--queue
              (< elfeed-fill-ytdlp--active elfeed-fill-ytdlp-max-processes))
    (cl-destructuring-bind (target limit callback) (pop elfeed-fill-ytdlp--queue)
      (cl-incf elfeed-fill-ytdlp--active)
      (condition-case e
          (elfeed-fill-ytdlp--start target limit callback)
        (error
         (cl-decf elfeed-fill-ytdlp--active)
         (funcall callback nil (error-message-string e)))))))

(defun elfeed-fill-ytdlp--list-videos (target limit callback)
  "List at most LIMIT videos at TARGET with yt-dlp, asynchronously.
LIMIT nil lists every video.  CALLBACK receives (ROWS ERROR), where ROWS
is a list of (ID TITLE UPLOADER TIMESTAMP) and ERROR is nil or a string."
  (unless (executable-find "yt-dlp")
    (user-error "yt-dlp not found on PATH"))
  (setq elfeed-fill-ytdlp--queue
        (nconc elfeed-fill-ytdlp--queue (list (list target limit callback))))
  (elfeed-fill-ytdlp--start-next))

(defun elfeed-fill-ytdlp--add-entries (feed rows)
  "Insert ROWS (from `elfeed-fill-ytdlp--parse-rows') into FEED.
Skips video ids already in the db and applies FEED's tags from
`elfeed-feeds'.  Synthesises monotonically decreasing publish dates
for rows without a timestamp so elfeed sorting preserves yt-dlp's
newest-first ordering.  Returns the number of entries added."
  (let* ((feed-id    (elfeed-feed-id feed))
         (feed-url   (elfeed-feed-url feed))
         (uploader-from-rows
          (cl-some (lambda (parts)
                     (let ((u (nth 2 parts)))
                       (and u (not (string-empty-p u)) u)))
                   rows))
         (current-title (elfeed-feed-title feed))
         (placeholder-p (and current-title
                             (string-match-p
                              "\\`\\(channel\\|playlist\\) [A-Za-z0-9_-]+\\'"
                              current-title))))
    (when (and uploader-from-rows
               (or (null current-title) placeholder-p))
      (setf (elfeed-feed-title feed) uploader-from-rows))
    (let* ((feed-title (or (elfeed-feed-title feed) feed-url))
           (tags       (elfeed-normalize-tags (elfeed-feed-autotags feed)
                                              elfeed-initial-tags))
           (now        (float-time))
           (new-data
            (cl-loop for parts in rows
                     for idx upfrom 0
                     unless (elfeed-fill-ytdlp--entry-by-video-id (nth 0 parts))
                     collect (list :videoId (nth 0 parts)
                                   :title   (nth 1 parts)
                                   :author  (if (and (nth 2 parts)
                                                     (not (string-empty-p
                                                           (nth 2 parts))))
                                                (nth 2 parts)
                                              feed-title)
                                   :published (or (nth 3 parts)
                                                  (- now (* idx 60))))))
           (entries (mapcar (lambda (data)
                              (let ((entry (elfeed-tube--entry-create feed-id data)))
                                (setf (elfeed-entry-tags entry) (copy-sequence tags))
                                entry))
                            new-data)))
      (when entries
        (elfeed-db-add entries))
      (length entries))))

;;;###autoload
(defun elfeed-fill-ytdlp-fetch (url callback)
  "Fetch the YouTube feed URL with yt-dlp, for `elfeed-fetch-functions'.
Return nil for other URLs so elfeed fetches them with curl."
  (when-let* (((string-match-p "\\`https?://www\\.youtube\\.com/feeds/videos\\.xml" url))
              ((executable-find "yt-dlp"))
              (parsed (elfeed-fill-ytdlp--ids-from-url url)))
    (elfeed-fill-ytdlp--list-videos
     (elfeed-fill-ytdlp--enumerate-target (car parsed) (cdr parsed))
     elfeed-fill-ytdlp-update-limit
     (lambda (rows err)
       (if err
           (progn
             (elfeed-handle-http-error url err)
             (funcall callback :error))
         (let ((feed (elfeed-db-get-feed url)))
           ;; elfeed's feed parsers set the url; a never-parsed feed has none.
           (setf (elfeed-feed-url feed) url)
           (elfeed-fill-ytdlp--add-entries feed rows))
         (funcall callback :success))))
    t))

(defun elfeed-fill-ytdlp--queue-count (n)
  "Add the yt-dlp jobs to N, the count of elfeed's curl jobs."
  (+ n elfeed-fill-ytdlp--active (length elfeed-fill-ytdlp--queue)))

(defun elfeed-fill-ytdlp--active-count (n)
  "Add the running yt-dlp processes to N, the count of active curl jobs."
  (+ n elfeed-fill-ytdlp--active))

(defun elfeed-fill-ytdlp--fill (feed type id limit)
  "List at most LIMIT videos of TYPE/ID with yt-dlp and add them to FEED."
  (let ((feed-url (elfeed-feed-url feed)))
    (message "Listing %s via yt-dlp..." feed-url)
    (elfeed-fill-ytdlp--list-videos
     (elfeed-fill-ytdlp--enumerate-target type id)
     limit
     (lambda (rows err)
       (if err
           (message "yt-dlp failed for %s: %s" feed-url err)
         (let ((added (elfeed-fill-ytdlp--add-entries feed rows)))
           (run-hook-with-args 'elfeed-update-hook feed-url)
           (message "Backfilled %s: %d new entries (%d listed)"
                    (or (elfeed-feed-title feed) feed-url)
                    added (length rows))))))))

(defun elfeed-fill-ytdlp--feed-at-point ()
  "Return the feed of the elfeed entry at point."
  (cond
   ((derived-mode-p 'elfeed-search-mode)
    (let ((sel (elfeed-search-selected :ignore-region)))
      (elfeed-entry-feed (if (listp sel) (car sel) sel))))
   ((derived-mode-p 'elfeed-show-mode)
    (elfeed-entry-feed elfeed-show-entry))
   (t (user-error "Not in an elfeed buffer"))))

;;;###autoload
(defun elfeed-fill-ytdlp (feed &optional limit)
  "Backfill the videos of FEED into the elfeed db using yt-dlp.
LIMIT caps the number of videos listed; nil lists every video.
Interactively, a numeric prefix argument sets LIMIT."
  (interactive
   (list (elfeed-fill-ytdlp--feed-at-point)
         (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let* ((feed-url (elfeed-feed-url feed))
         (parsed (or (elfeed-fill-ytdlp--ids-from-url feed-url)
                     (user-error "Not a YouTube channel/playlist feed: %s"
                                 feed-url))))
    (elfeed-fill-ytdlp--fill feed (car parsed) (cdr parsed) limit)))

;;;###autoload
(defun elfeed-fill-ytdlp-from-url (url &optional tags limit)
  "Backfill the YouTube channel/playlist at URL into elfeed.

URL may be any of:
  https://www.youtube.com/playlist?list=...
  https://www.youtube.com/channel/UC...
  https://www.youtube.com/channel/UC.../videos
  https://www.youtube.com/feeds/videos.xml?playlist_id=...
  https://www.youtube.com/feeds/videos.xml?channel_id=...

TAGS, if given, is a list of symbols added to `elfeed-feeds' alongside
the canonical RSS URL when registering the feed for the first time.
LIMIT caps the number of videos listed; nil lists every video."
  (interactive "sYouTube channel or playlist URL: ")
  (when-let* ((bad (cl-find-if-not #'symbolp tags)))
    (user-error "elfeed feed tags must be symbols, got %S (type %s) — did you mean '%s?"
                bad (type-of bad)
                (if (numberp bad) (format "n%s" bad) (format "%S" bad))))
  (let* ((parsed (or (elfeed-fill-ytdlp--ids-from-url url)
                     (user-error "Could not extract channel/playlist id from: %s"
                                 url)))
         (rss-url (elfeed-fill-ytdlp--feed-url-for (car parsed) (cdr parsed))))
    (unless (cl-find-if (lambda (e)
                          (equal (if (listp e) (car e) e) rss-url))
                        elfeed-feeds)
      (add-to-list 'elfeed-feeds (if tags (cons rss-url tags) rss-url) t)
      (message "Registered new elfeed feed: %s" rss-url))
    (let ((feed (elfeed-db-get-feed rss-url)))
      (setf (elfeed-feed-url feed) rss-url)
      (unless (elfeed-feed-title feed)
        (setf (elfeed-feed-title feed)
              (format "%s %s" (car parsed) (cdr parsed))))
      (elfeed-fill-ytdlp--fill feed (car parsed) (cdr parsed) limit))))

;;;###autoload
(defun elfeed-fill-ytdlp-fix-dates (feed &optional limit)
  "Re-list FEED via yt-dlp and overwrite each entry's :date with the real
upload timestamp returned by yt-dlp.  Entries without a returned timestamp
are left untouched.  LIMIT caps the number of videos listed; nil lists
every video.  Interactively, a numeric prefix argument sets LIMIT."
  (interactive
   (list (elfeed-fill-ytdlp--feed-at-point)
         (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let* ((feed-url (elfeed-feed-url feed))
         (parsed (or (elfeed-fill-ytdlp--ids-from-url feed-url)
                     (user-error "Not a YouTube channel/playlist feed: %s"
                                 feed-url))))
    (elfeed-fill-ytdlp--list-videos
     (elfeed-fill-ytdlp--enumerate-target (car parsed) (cdr parsed))
     limit
     (lambda (rows err)
       (if err
           (message "yt-dlp failed for %s: %s" feed-url err)
         (let ((changed 0))
           (dolist (parts rows)
             (let ((ts (nth 3 parts))
                   (entry (elfeed-fill-ytdlp--entry-by-video-id (nth 0 parts))))
               (when (and ts entry (not (= (elfeed-entry-date entry) ts)))
                 (setf (elfeed-entry-date entry) ts)
                 (cl-incf changed))))
           (message "%s: updated dates on %d/%d entries"
                    (or (elfeed-feed-title feed) feed-url)
                    changed (length rows))))))))

(add-hook 'elfeed-fetch-functions #'elfeed-fill-ytdlp-fetch)
(advice-add 'elfeed-queue-count-total :filter-return #'elfeed-fill-ytdlp--queue-count)
(advice-add 'elfeed-queue-count-active :filter-return #'elfeed-fill-ytdlp--active-count)

(provide 'elfeed-fill-ytdlp)
;;; elfeed-fill-ytdlp.el ends here
