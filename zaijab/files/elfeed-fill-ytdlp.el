;;; elfeed-fill-ytdlp.el --- Backfill a YouTube channel/playlist into elfeed via yt-dlp -*- lexical-binding: t; -*-

;; Drop-in replacement for `elfeed-tube-fill-feeds' that does not depend on
;; Invidious. Enumerates the channel/playlist with `yt-dlp --flat-playlist',
;; then synthesises the plist shape that `elfeed-tube--entry-create' already
;; consumes and hands the result to `elfeed-db-add'.
;;
;; Usage from a running Emacs session:
;;   M-x load-file RET ~/code/guix-channel/zaijab/files/elfeed-fill-ytdlp.el RET
;;
;; Then either:
;;   (a) M-x elfeed-fill-ytdlp-from-url RET <playlist-or-channel URL> RET
;;       Registers the corresponding RSS feed with elfeed (if absent) and
;;       backfills every video into the elfeed db.
;;
;;   (b) Place point on a YouTube feed entry in *elfeed-search* and call
;;       M-x elfeed-fill-ytdlp.
;;
;; Example for MIT 6.041 (Probabilistic Systems Analysis, Tsitsiklis):
;;   (elfeed-fill-ytdlp-from-url
;;    "https://www.youtube.com/playlist?list=PLUl4u3cNGP61MdtwGTqZA0MreSaDybji8"
;;    '(lecture probability mit 6041))

(require 'elfeed)
(require 'elfeed-db)
(require 'elfeed-tube)
(require 'cl-lib)

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

(defun elfeed-fill-ytdlp--existing-video-ids (feed)
  "Return the list of YouTube video ids already present in FEED."
  (delq nil
        (mapcar (lambda (e)
                  (let ((link (or (elfeed-entry-link e) "")))
                    (when (string-match "v=\\([^&]+\\)" link)
                      (match-string 1 link))))
                (elfeed-feed-entries feed))))

(defun elfeed-fill-ytdlp--parse-ts (s)
  "Parse a yt-dlp Unix-timestamp print field. Return float or nil."
  (and s (not (string-empty-p s)) (not (string= s "NA"))
       (let ((n (string-to-number s)))
         (and (> n 0) (float n)))))

(defun elfeed-fill-ytdlp--run-yt-dlp (target)
  "Run yt-dlp on TARGET and return a list of (ID TITLE UPLOADER TIMESTAMP) lists.
TIMESTAMP is the Unix epoch as a float, or nil if yt-dlp did not return one.
UPLOADER falls back to playlist_uploader/channel when the per-video
uploader field is missing (typical for `--flat-playlist' on channels)."
  (unless (executable-find "yt-dlp")
    (user-error "yt-dlp not found on PATH"))
  (let ((buf (generate-new-buffer " *yt-dlp-fill*"))
        (rows '()))
    (unwind-protect
        (progn
          (message "Enumerating %s via yt-dlp (this may take a while)..." target)
          (let ((exit (call-process
                       "yt-dlp" nil buf nil
                       "--flat-playlist" "--ignore-errors"
                       "--extractor-args" "youtubetab:approximate_date"
                       "--print"
                       "%(id)s\t%(title)s\t%(uploader,playlist_uploader,channel|)s\t%(timestamp|)s"
                       target)))
            (unless (zerop exit)
              (display-buffer buf)
              (user-error "yt-dlp failed (exit %s) — see %s"
                          exit (buffer-name buf))))
          (with-current-buffer buf
            (goto-char (point-min))
            (while (not (eobp))
              (let ((parts (split-string
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                            "\t")))
                (when (and (>= (length parts) 2)
                           (not (string-empty-p (nth 0 parts))))
                  (push (list (nth 0 parts)
                              (nth 1 parts)
                              (nth 2 parts)
                              (elfeed-fill-ytdlp--parse-ts (nth 3 parts)))
                        rows)))
              (forward-line 1))))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (nreverse rows)))

(defun elfeed-fill-ytdlp--add-entries (feed rows)
  "Insert ROWS (from `elfeed-fill-ytdlp--run-yt-dlp') into FEED.
Skips video ids already present. Synthesises monotonically
decreasing publish dates so elfeed sorting preserves yt-dlp's
newest-first ordering. Returns the number of entries added."
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
           (existing   (elfeed-fill-ytdlp--existing-video-ids feed))
           (now        (float-time))
           (new-data
            (cl-loop for parts in rows
                     for idx upfrom 0
                     unless (member (nth 0 parts) existing)
                     collect (list :videoId (nth 0 parts)
                                   :title   (nth 1 parts)
                                   :author  (if (and (nth 2 parts)
                                                     (not (string-empty-p
                                                           (nth 2 parts))))
                                                (nth 2 parts)
                                              feed-title)
                                   :published (or (nth 3 parts)
                                                  (- now (* idx 60)))))))
      (if (null new-data)
          (prog1 0
            (message "Nothing new to backfill for %s (%d enumerated)"
                     feed-title (length rows)))
        (let ((entries (mapcar (lambda (data)
                                 (elfeed-tube--entry-create feed-id data))
                               new-data)))
          (elfeed-db-add entries)
          (run-hook-with-args 'elfeed-update-hooks feed-url)
          (message "Backfilled %s: %d new entries (%d enumerated)"
                   feed-title (length entries) (length rows))
          (length entries))))))

;;;###autoload
(defun elfeed-fill-ytdlp (feed)
  "Backfill every video of FEED into the elfeed db using yt-dlp."
  (interactive
   (list (cond
          ((derived-mode-p 'elfeed-search-mode)
           (let ((sel (elfeed-search-selected :ignore-region)))
             (elfeed-entry-feed (if (listp sel) (car sel) sel))))
          ((derived-mode-p 'elfeed-show-mode)
           (elfeed-entry-feed elfeed-show-entry))
          (t (user-error "Not in an elfeed buffer")))))
  (let* ((feed-url (elfeed-feed-url feed))
         (parsed (elfeed-fill-ytdlp--ids-from-url feed-url)))
    (unless parsed
      (user-error "Not a YouTube channel/playlist feed: %s" feed-url))
    (elfeed-fill-ytdlp--add-entries
     feed
     (elfeed-fill-ytdlp--run-yt-dlp
      (elfeed-fill-ytdlp--enumerate-target (car parsed) (cdr parsed))))))

;;;###autoload
(defun elfeed-fill-ytdlp-from-url (url &optional tags)
  "Backfill the YouTube channel/playlist at URL into elfeed.

URL may be any of:
  https://www.youtube.com/playlist?list=...
  https://www.youtube.com/channel/UC...
  https://www.youtube.com/channel/UC.../videos
  https://www.youtube.com/feeds/videos.xml?playlist_id=...
  https://www.youtube.com/feeds/videos.xml?channel_id=...

TAGS, if given, is a list of symbols added to `elfeed-feeds' alongside
the canonical RSS URL when registering the feed for the first time."
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
      (elfeed-fill-ytdlp--add-entries
       feed
       (elfeed-fill-ytdlp--run-yt-dlp
        (elfeed-fill-ytdlp--enumerate-target (car parsed) (cdr parsed)))))))

(defun elfeed-fill-ytdlp--entry-by-video-id (video-id)
  "Look up the elfeed entry for VIDEO-ID, or nil."
  (elfeed-db-get-entry (cons "www.youtube.com" (concat "yt:video:" video-id))))

;;;###autoload
(defun elfeed-fill-ytdlp-fix-dates (feed)
  "Re-enumerate FEED via yt-dlp and overwrite each entry's :date with the real
upload timestamp returned by yt-dlp. Entries without a returned timestamp are
left untouched. Returns the number of entries whose date changed."
  (interactive
   (list (cond
          ((derived-mode-p 'elfeed-search-mode)
           (let ((sel (elfeed-search-selected :ignore-region)))
             (elfeed-entry-feed (if (listp sel) (car sel) sel))))
          ((derived-mode-p 'elfeed-show-mode)
           (elfeed-entry-feed elfeed-show-entry))
          (t (user-error "Not in an elfeed buffer")))))
  (let* ((feed-url (elfeed-feed-url feed))
         (parsed (or (elfeed-fill-ytdlp--ids-from-url feed-url)
                     (user-error "Not a YouTube channel/playlist feed: %s"
                                 feed-url)))
         (rows (elfeed-fill-ytdlp--run-yt-dlp
                (elfeed-fill-ytdlp--enumerate-target (car parsed) (cdr parsed))))
         (changed 0))
    (dolist (parts rows)
      (let ((ts (nth 3 parts))
            (entry (elfeed-fill-ytdlp--entry-by-video-id (nth 0 parts))))
        (when (and ts entry (not (= (elfeed-entry-date entry) ts)))
          (setf (elfeed-entry-date entry) ts)
          (cl-incf changed))))
    (message "%s: updated dates on %d/%d entries"
             (or (elfeed-feed-title feed) feed-url) changed (length rows))
    changed))

(provide 'elfeed-fill-ytdlp)
;;; elfeed-fill-ytdlp.el ends here
