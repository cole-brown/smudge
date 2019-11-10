;;; spotify-player-feedback.el --- Spotify action feedback messages -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; For feedback to user for their action.

;; E.g. `spotify-volume-up' can get a closure from here bundled up, fulfill the
;; only part it knows of - "volume" - and invoke it.

;; The user can then receive - or not - the intended message, properly
;; formatted, without any frontend messing about by the backends.

;;; Code:

;;---------------------------------Spotify.el-----------------------------------
;;--                             Player Feedback                              --
;;------------------------------------------------------------------------------

(require 'spotify-player-status)

;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-11-09]: any settings?

;; (defcustom spotify-player-feedback-refresh-interval 5
;;   "The interval, in seconds, that the mode line must be updated. When using the
;; 'connect transport, avoid using values smaller than 5 to avoid being rate
;; limited. Set to 0 to disable this feature."
;;   :type 'integer
;;   :group 'spotify)


;;------------------------------------------------------------------------------
;; User Action Responses
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-11-09]: use this in places for things.
(defun spotify--player-feedback-promise-message (fmt &rest keywords)
  "Creates a closure to be invoked in the future when async
operation is finished for feedback to user.

'((:callback <closure w/ fmt str ready>)
  (:volume nil)
  (:artist nil))

This will thin be fulfilled by the backend filling in whatever
fields it knows of via `spotify--player-feedback-fulfill-message'.

Finally, it can be delivered to the user via `spotify--invoke-callback'.
"
  ;; End up with callback on car so it's an easy/known to check/pop...
  (let ((callback (lambda (alist)
                    (spotify--player-feedback-future-message fmt alist)))
        (key-list))
    (dolist (key keywords)
      (push (list key nil) key-list))
    (push (list :callback
                callback)
          key-list)))
;; (spotify--player-feedback-promise-message "hi? %d %s" :digit :string)


(defun spotify--player-feedback-future-message (fmt alist)
  "Takes promised message and fulfilled alist values, and
delivers it to the user - now that it is the future.

Should only really be dealt with as a closure from
`spotify--player-feedback-promise-message'.
"
  (let ((output fmt))
    (dolist (element alist)
      (unless (null (nth 1 element)) ;; skip unfulilled elements
        ;; replace key with fulfillment
        (setq output (replace-regexp-in-string (symbol-name (nth 0 element))
                                               (nth 1 element)
                                               output))))
    (message output)))


(defun spotify--player-feedback-fulfill-message (alist key value)
  "Provides value to fulfill promised message.
Returns the updated-in-place alist.

Backend can fulfill values it knows of without caring about
message contents, formatting, etc.

E.g.:
(spotify--player-feedback-fulfill-message reply :volume 87)
"
  ;; Only try setting if it's found.
  (if-let ((cell (assoc key alist)))
      ;; Get alist cell for key, set the cdr to value. The 'cons' part will
      ;; leave the alist as a list-alist, as opposed to a dotted-pair alist.
      (setf (cdr cell) (cons value nil)))
  alist)
;; (spotify--player-feedback-fulfill-message '((:callback jeff) (:test nil)) :test 'more-jeff)
;; (spotify--player-feedback-fulfill-message '((:callback jeff) (:test nil)) :bob 'more-bob)


;; §-TODO-§ [2019-11-09]: probably doesn't belong in here...
(defun spotify--invoke-callback (callback &rest args)
  "Handles how to call the callback.

Can handle the normal callback(status) types, and the message
fulfillment alist types.
"
  (cond
   ;; Easy case: it's just a callback
   ((functionp callback)
    (apply callback args))

   ;; Callback is a list with the first cell of '(:callback <func>).
   ;; In this case, pop that and call the callback func with the rest.
   ((and (listp testfoo)         ;; is list
         (listp (nth 0 testfoo)) ;; first element is list/cons
         (eq :callback (nth 0 (nth 0 testfoo))) ;; in that, first is our keyword
         (= (length (cdr (nth 0 testfoo))) 1)   ;; and only one other thing
         (functionp (nth 1 (nth 0 testfoo))))   ;; and it's a function
    ;; so let's pop off the callback cell and then call with all the rest
    (let* ((callback-cell (pop callback))
           (callback-args callback))
      (funcall (nth 1 callback-cell) callback-args)))

   ;; otherwise just ignore it
   (t
    nil)))
;; (spotify--invoke-callback 'message "hello %s" "there")
;; (let* ((promise (spotify--player-feedback-promise-message "hello, :test :jeff" :test :jeff)))
;;   (spotify--player-feedback-fulfill-message promise :test "there.")
;;   (spotify--player-feedback-fulfill-message promise :bob "uh...")
;;   (spotify--invoke-callback promise))


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------
(provide 'spotify-player-feedback)
