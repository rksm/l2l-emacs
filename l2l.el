;;; l2l.el --- Lively-2-Lively emacs support
 
;; Copyright 2015 Robert Krahn
 
;; Author: Robert Krahn <robert@kra.hn>
;; URL: http://github.com/rksm/l2l-emacs
;; Version: 0.1.0
;; Package-Requires: ((websocket "20150330.2123"))

(require 'json)
(require 'websocket)

(defmacro comment (&rest body)
  nil)

(comment
 (defvar port 9312)

 (setq test-ws-server
       (websocket-server port
			 :on-open (lambda (ws) (message "ws open now %s" (prin1-to-string ws)))
			 :on-message (lambda (ws frame) (message "ws got msg %s %s" (prin1-to-string ws) (prin1-to-string frame)))
			 :on-error (lambda (ws when err) (message "ws errored %s %s %s" (prin1-to-string ws) (prin1-to-string when) (prin1-to-string err)))
			 :on-close (lambda (ws) (message "ws closed %s" (prin1-to-string ws)))
			 ))

 (setq url "ws://lively-web.org/nodejs/SessionTracker/connect")
					; (setq url (format "ws://localhost:%s" port))

 (setq test-ws-client
       (websocket-open url :protocols '(lively-json)
		       :on-open (lambda (ws) (message "ws client open now %s" (prin1-to-string ws)))
		       :on-message (lambda (ws frame) (message "ws client got msg %s %s" (prin1-to-string ws) (prin1-to-string frame)))
		       :on-error (lambda (ws when err) (message "ws client errored %s %s %s" (prin1-to-string ws) (prin1-to-string when) (prin1-to-string err)))
		       :on-close (lambda (ws) (message "ws client closed %s" (prin1-to-string ws)))
		       ))

 (setq register-msg
       (json-encode '(:action registerClient :data (:id 123 :user emacs-test))))

 (completing-read "test" '(a b) nil nil)
 (ido-completing-read "test" '(:a "b") nil nil)
 )

;;; -=-=-=-=-=-=-
;;; l2l-sessions

(defconst l2l-sessions-buffer "*l2l-sessions*")

(defvar l2l-sessions-mode-hook nil)

(defvar l2l-sessions-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<return>") (lambda () (interactive) (message "ENTER!!!")))
    map)
  "Keymap for l2l session mode")

(defvar l2l-sessions-mode-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda () (interactive) (message "clicked"))))
    map)

(define-derived-mode l2l-sessions-mode fundamental-mode "L2L"
  "Major mode for browsing and choosing l2l sessions."
  )

(defun l2l-sessions-show (buffer)
  (with-current-buffer buffer
    (l2l-sessions-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "foooo" 'font-lock-face 'font-lock-type-face))
      (newline)
      (dolist (item '("a" "b" "c"))
        (insert "  " item)
        (newline))
      (goto-char (point-min)))))

(defun l2l-browse-sessions ()
  "list l2l sessions"
  (interactive)
  (with-current-buffer (pop-to-buffer (get-buffer-create l2l-sessions-buffer))
    (l2l-sessions-show (current-buffer))))

;; (defun l2l-sessions-mode ()
;;   "Major mode for listing and choosing l2l sessions"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (use-local-map l2l-sessions-mode-map))


(comment
 
 (provide 'l2l-sessions-mode)

 (websocket-send-text test-ws-client register-msg)

 (json-encode '(:action registerClient :data (:id 123 :user emacs-test)))


 "lively-json"

 (websocket-server-close test-ws-server)

 (message "fooo")
 (completing-read)
 )

;;; l2l.el ends here
