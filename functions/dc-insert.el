;; Note: can't use narrow-to-region... Functions are run from point to end of buffer.

(defun dc-insert-photos ()
  "Convert Zenphoto URLs with the necessary code for Dotclear to include thumbnail + link to original picture."
  (interactive)
  ;; (save-restriction
  ;;   (narrow-to-region (point) (mark))
  ;;   (save-excursion
      (while (re-search-forward "^\\(http://[a-z]+.basille.net/photos/\\)\\([a-z0-9\-_/]+\\)\\(\\.jpg$\\)" nil t)
	(replace-match "[((\\1cache/\\2_780.jpg))|\\1albums/\\2.jpg]" nil nil)))
;; ))

(defun dc-correct-photos ()
  "Convert Zenphoto *cache* URLs with the necessary code for Dotclear to include thumbnail + link to original picture."
  (interactive)
  ;; (save-restriction
  ;;   (narrow-to-region (point) (mark))
  ;;   (save-excursion
      (while (re-search-forward "^\(\(\\(http://[a-z]+.basille.net/photos/\\)\\(cache/\\)\\([a-z0-9\-_/]+\\)\\(\_595\\|\_780\\)\\(\\.jpg\)\)$\\)" nil t)
	(replace-match "[((\\1\\2\\3_780.jpg))|\\1albums/\\3.jpg]" nil nil)))
;; ))place-match "[((\\1cache/\\2_780.jpg))|\\1albums/\\2.jpg]" nil nil)))
;; ))

(defun dc-insert-videos ()
  "Convert Zenphoto URLs with the necessary code for Dotclear to include thumbnail + video."
  (interactive)
  ;; (save-restriction
  ;;   (narrow-to-region (point) (mark))
  ;;   (save-excursion
      (while (re-search-forward "^\\(http://[a-z]+.basille.net/photos/\\)\\([a-z0-9\-_/]+\\)\\(\\.png$|\\.mp4$\\)" nil t)
	(replace-match "///html\n<video width=\"100\%\" height=\"100\%\" controls=\"controls\" preload=\"auto\" poster=\"\\1albums/\\2.png\">\n<source src=\"\\1albums/\\2.webm\" type=\"video/webm\">\n<source src=\"\\1albums/\\2.mp4\" type=\"video/mp4\">\n<p>Désolé, votre navigateur ne supporte pas la vidéo HTML5.</p>\n</video>\n///\n%%%\n" nil nil)))
;; ))
