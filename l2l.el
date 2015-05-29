
;;; l2l.el --- Lively-2-Lively emacs support

;; Copyright 2015 Robert Krahn

;; Author: Robert Krahn <robert@kra.hn>
;; URL: http://github.com/rksm/l2l-emacs
;; Version: 0.1.0
;; Package-Requires: ((websocket "20150330.2123") (json "1.4") (dash "20150513.1027") (uuid "20120910.151"))

(require 'json)
(require 'websocket)
(require 'dash)
(require 'cl)
(require 'uuid)

(defmacro comment (&rest body)
  nil)

(defvar l2l-default-url "ws://lively-web.org/nodejs/SessionTracker/connect")
(defvar l2l-connections nil)
(defvar l2l-message-response-callbacks nil)

(defun l2l-get-ws (&optional url)
  (let ((url (or url l2l-default-url)))
    (plist-get l2l-connections url)))

;;; -=-=-=-=-=-=-=-=-=-
;;; send callback helpers

(defun l2l-get-message-response-callbacks (ws)
  (-map (-partial #'-drop 1) l2l-message-response-callbacks))

(defun l2l-register-message-response-callback (msg-id callback)
  (setq l2l-message-response-callbacks
	(cons `(,msg-id . ,callback) l2l-message-response-callbacks)))

(defun l2l-unregister-message-response-callback (msg-id)
  (setq l2l-message-response-callbacks
	(-remove-item (assoc msg-id l2l-message-response-callbacks) l2l-message-response-callbacks)))

(defun l2l-invoke-message-response-callback (msg)
  (let* ((resp-id (cdr (assoc 'inResponseTo msg)))
	 (callback (cdr (assoc resp-id l2l-message-response-callbacks))))
    (when callback
      ;; FIXME only remove when not expoct more
      (l2l-unregister-message-response-callback resp-id)
      (apply callback (list msg)))))

;;; -=-=-=-=-=-=-=-=-=-
;;; connection creation

(defun l2l-when-connected (id user world-url url ws register-msg callback)
  (let ((tracker-id (->> register-msg
			 (assoc 'data) cdr
			 (assoc 'tracker) cdr
			 (assoc 'id) cdr)))
    (message "l2l connection established %s [%s] -> %s [%s]"
	     user world-url
	     tracker-id url))
  (if callback
      (apply callback (list register-msg))))

(defun l2l-connect (&optional url &optional callback)
  (lexical-let* ((callback (or callback (lambda (msg))))
		 (url (or url l2l-default-url))
		 (id (uuid-string))
		 (user "emacs-client")
		 (world-url system-name)
		 (reg-msg `(:action registerClient :data (:id ,id :user ,user :worldURL ,world-url))))
    (if (l2l-get-ws url)
	(error (format "connections for %s already exist:" url)))
    (cl-flet ((on-open (ws) 
		       (l2l-send ws reg-msg
				 (lambda (answer)
				   (l2l-when-connected id user world-url url
						       ws answer
						       callback))))
	       (on-message (ws frame)
			   (condition-case ex
			       (let ((msg (l2l-read-message frame)))
				 (if (assoc 'inResponseTo msg)
				     (l2l-invoke-message-response-callback msg)
				   (l2l-service-request ws msg)))
			    ('error (message ex))))
	       (on-error (ws when err)
			 (setq l2l-last-err err)
			 (message "ws client errored %s %s" when err))
	       (on-close (ws) (message "ws client closed %s" (prin1-to-string ws))))
      (let ((ws (websocket-open url
				:protocols '(lively-json)
				:on-open #'on-open
				:on-message #'on-message
				:on-error #'on-error
				:on-close #'on-close)))
	(setq l2l-connections (plist-put l2l-connections url ws))
	ws))))

(defun l2l-disconnect (&optional url)
  (let* ((url (or url l2l-default-url))
	 (ws (l2l-get-ws url)))
    (when ws
      (websocket-close ws)
      (plist-put l2l-connections url nil))))

;;; -=-=-=-=-=-=-=-=-=-
;;; message sending

(defun l2l-read-message (frame)
  (condition-case ex
      (json-read-from-string (websocket-frame-payload frame))
    ('error (progn
	      (message "error reading l2l msg %s: %s" frame ex)
	      nil))))

(defun l2l-send (ws msg &optional callback)
  (cl-assert (plist-get msg :action) t "No action!")
  (let* ((msg-id (or (plist-get msg :messageId) (uuid-string)))
	 (msg (plist-put msg :messageId msg-id))
	 (msg (json-encode msg)))
    (if callback
	(l2l-register-message-response-callback msg-id callback)) 
    (websocket-send-text ws msg)))

;;; -=-=-=-=-=-=-=-=-=-
;;; services
(defun l2l-service-request (ws msg)
  (message "got service request %s" msg)
  (let* ((action (->> msg (assoc 'action) cdr))
	 (answer-data
	  (cond
	    ((equal action "remoteEvalRequest")
	     (let* ((expr (->> msg (assoc 'data) cdr (assoc 'expr) cdr))
		    ;; (expr (replace-regexp-in-string "\s*\\/\\/\\# sourceURL.*\n.*;$" "" expr "")) 
		    (form (read expr))
		    (result (condition-case ex
				(eval form)
			      ('error ex)))) 
	       `(:result ,result)))))
	 (answer `(:messageId
		   ,(uuid-string)
		   :inResponseTo
		   ,(->> msg (assoc 'messageId) cdr)
		   :action
		   ,(format "%sResponse" action)
		   :data ,answer-data)))
    (message "answering %s" answer)
    (l2l-send ws answer)))

(comment

 (l2l-connect)
 (l2l-disconnect)
 
 (l2l-read-message l2l-last-frame)
 (json-read-from-string (websocket-frame-payload l2l-last-frame))

 (setq l2l-message-response-callbacks nil)

 (l2l-send (l2l-get-ws) '(:action registerClient :data (:id 123 :user emacs-test)) (lambda (msg) (message "received message report %s" msg)))
 (l2l-send (l2l-get-ws) '(:action reportServices :data nil))
 (l2l-send (l2l-get-ws) '(:action reportServices :data nil) (lambda (msg) (message "received message report %s" msg)))
 (l2l-send (l2l-get-ws) '(:action reportServices :data nil) (lambda (msg) (message "%s" (->> msg (assoc 'inResponseTo) cdr))))

 (l2l-send (l2l-get-ws) '(:action remoteEvalRequest :data (:expr "1 + 2")) (lambda (msg) (message "received message eval %s" msg)))
 (l2l-send (l2l-get-ws) '(:action reportServices :data nil))

 
 (setq x '((messageIndex . 21177)
   (messageId . client-msg:21A90A07-1721-4DDF-B2D5-BC57878080BD)
   (target . 2bed67b3-3eb8-0054-43ab-34bebcb5c082)
   (sender . client-session:A93EE4D7-C0E9-4D8C-802B-6C9A1921B4FF)
   (data (expr . "1 + 2
//# sourceURL=remote_Lively2Lively_workspace_1432897745885
					;")) (action . remoteEvalRequest)))

 
 
 (l2l-send (l2l-get-ws) `(:action remoteEvalRequest :messageId ,(uuid-string) :data (:expr "123 + 2;")))
 (plist-get '(:action registerClient :data (:id 123 :user emacs-test)) :action)

 )

(comment
 (defvar port 9312)

 (setq test-ws-server
       (websocket-server port
			 :on-open (lambda (ws) (message "ws open now %s" (prin1-to-string ws)))
			 :on-message (lambda (ws frame) (message "ws got msg %s %s" (prin1-to-string ws) (prin1-to-string frame)))
			 :on-error (lambda (ws when err) (message "ws errored %s %s %s" (prin1-to-string ws) (prin1-to-string when) (prin1-to-string err)))
			 :on-close (lambda (ws) (message "ws closed %s" (prin1-to-string ws)))
			 ))

 (setq url l2l-default-url)
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

(comment

 (provide 'l2l-sessions-mode)

 (websocket-send-text test-ws-client register-msg)

 (json-encode '(:action registerClient :data (:id 123 :user emacs-test)))


 "lively-json"

 (websocket-server-close test-ws-server)

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

(provide 'l2l)

;; (defun l2l-sessions-mode ()
;;   "Major mode for listing and choosing l2l sessions"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (use-local-map l2l-sessions-mode-map))


;;; l2l.el ends here
