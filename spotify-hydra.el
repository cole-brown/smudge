;;; spotify-hydra.el --- Pre-defined Hydra, in case that interests anyone... -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Hydra with a nice helpful docstr that uses player status cache to populate
;; fields with current values.

;; Requires Hydra to use.
;; Desires s.el for `s-center' - fallback is no centering.
;; Desires bind-key.el for `bind-key' - fallback is `define-key'.

;;; Code:

;;---------------------------------Spotify.el-----------------------------------
;;--                 Hydra - Alternative to the Command Map.                  --
;;------------------------------------------------------------------------------

;;---
;; Requires
;;---
;; want s.el for s-center, but if not we can do without.
(require 's nil 'noerror)

;; want bind-key for keybinds, but if not we can do without.
(require 'bind-key nil 'noerror)


;;---
;; Macros
;;---
(defmacro spotify--with-hydra (&rest body)
  "If Hydra is present (featurep), evaluate BODY forms,
otherwise silently does nothing."
  (declare (indent defun))
  `(when (featurep 'hydra)
     ,@body))


(defmacro spotify--hydra-or-error (error-message &rest body)
  "If Hydra is present (featurep) and player status cache is enabled, evaluate
BODY form, otherwise calls `error' with ERROR-MESSAGE appended to a generic
\"spotify-hydra error\" `error' message."
  (declare (indent defun))
  `(if (or (not (featurep 'hydra))
           (not 'spotify-cache-player-status-enabled))
       (error "spotify-hydra error - %s: %s"
              (if (not (featurep 'hydra))
                  "Hydra is not present"
                "Spotify Player Cache is not enabled")
              error-message)
     ,@body))


(defmacro spotify--eat-errors (error-form &rest body)
  "Executes BODY forms with grace, elegance, and a guard against
error bubbling through. I.e. wraps body in `condition-case-unless-debug'.

If an `error' signal is caught, executes ERROR-FORM with the error
bound to the variable `spotify--error'. Use nil, call `ignore',
or whatever if you desire error squelching.

Examples (\"hi\" message will not be reached):
  Demotes error to message:
    (spotify--eat-errors (message \"errored on: %S\" spotify--error)
      (error \"test\")
      (message \"hi\"))

  Eats/squelches error entirely:
    (spotify--eat-errors nil
      (error \"test\")
      (message \"hi\"))
"
  (declare (indent 1))
  `(condition-case-unless-debug spotify--error
       ,@body
     ;; demote error to message unless debugging
     (error ,error-form)))



;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

;; Just skip the defcustoms if we don't have Hydra...
(spotify--with-hydra

  (defgroup spotify-hydra nil
    "Hydra settings for Spotify client."
    :version "0.0.1"
    :group 'spotify)


  (defcustom spotify-hydra-keybind nil
    "Keybind to invoke Spotify.el's Hydra.
Will use bind-key if present, else define-key."
    :group 'spotify-hydra
    :type 'string)


  (defcustom spotify-hydra-auto-remote-mode nil
    "If nil, spotify-hydra will just error if `spotify-remote-mode' is not
enabled.

If non-nil, spotify-hydra will enable
`global-spotify-remote-mode' when it's not enabled before
executing its body."
    :type 'boolean
    :group 'spotify-hydra)


  (defcustom spotify-hydra-playing-text "Pause Track"
    "Text to be displayed when Spotify is playing.
Display 'Pause' as that is what Hydra will do for play/pause
toggle when playing."
    :type 'string
    :group 'spotify)


  (defcustom spotify-hydra-paused-text "Play Track"
    "Text to be displayed when Spotify is paused.
Display 'Play' as that is what Hydra will do for play/pause
toggle when playing."
    :type 'string
    :group 'spotify)


  (defcustom spotify-hydra-stopped-text "Play Track"
    "Text to be displayed when Spotify is paused.
Display 'Play' as that is what Hydra will do for play/pause
toggle when playing."
    :type 'string
    :group 'spotify)


  (defcustom spotify-hydra-repeating-text "[R] Toggle Repeat"
    "Text to be displayed when repeat is enabled.
e.g. '[R] Toggle Repeat'"
    :type 'string
    :group 'spotify)


  (defcustom spotify-hydra-not-repeating-text "[-] Toggle Repeat"
    "Text to be displayed when repeat is disabled.
e.g. '[-] Toggle Repeat'"
    :type 'string
    :group 'spotify)


  (defcustom spotify-hydra-shuffling-text "[S] Toggle Shuffle"
    "Text to be displayed when shuffling is enabled.
e.g. '[S] Toggle Shuffle'"
    :type 'string
    :group 'spotify)


  (defcustom spotify-hydra-not-shuffling-text "[-] Toggle Shuffle"
    "Text to be displayed when shuffling is disabled.
e.g. '[-] Toggle Shuffle'"
    :type 'string
    :group 'spotify)


  (defcustom spotify-hydra-muted-text "[M] Toggle Mute"
    "Text to be displayed when muting is enabled.
e.g. '[M] Toggle Mute'"
    :type 'string
    :group 'spotify)


  (defcustom spotify-hydra-not-muted-text "[-] Toggle Mute"
    "Text to be displayed when muting is disabled.
e.g. '[-] Toggle Mute'"
    :type 'string
    :group 'spotify)


  (defcustom spotify-hydra-format "%a - %t"
    "Format used to display the current Spotify client player status in the
top line of the Hydra docstring. The following placeholders are supported:

* %a - Artist Name
* %t - Track Name
* %n - Track Number
* %l - Track Duration (as formatted by `spotify-player-status-duration-format')
* %p - 'Player Status' indicator string for:
       'playing', 'paused', and 'stopped' states
* %s - 'Player Shuffling' indicator
* %r - 'Player Repeating' indicator
* %v - Player Volume (0-100)"
    :type 'string
    :group 'spotify-hydra)

  ) ;; end Setting's spotify--with-hydra block


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

(defconst spotify--hydra-translators
  ;; Use defcustom symbols so we don't have to keep this up-to-date.
  '(;; Don't truncate these in the Hydra by default.
    ;; Artist/Track: truncate to length
    ;; (:artist    :artist
    ;;            (lambda (len str) (truncate-string-to-width str len
    ;;                                                        0 nil "..."))
    ;;            spotify-player-status-truncate-length)
    ;; (:track     :track
    ;;            (lambda (len str) (truncate-string-to-width str len
    ;;                                                        0 nil "..."))
    ;;            spotify-player-status-truncate-length)

    ;; Duration: Format duration-ms to duration string
    (:duration  :duration-millisecond
                (lambda (fmt ms) (format-seconds fmt (/ ms 1000)))
                spotify-player-status-duration-format)

    ;; Device Active bool: translate from state
    (:device-active-bool :device-active-state
                         ;; map both nil/false and undefined to nil/false
                         ((t t) (nil nil) (undefined nil)))

    ;; The Rest: translate from bool to text
    (:shuffling :shuffling-bool
                ((t spotify-hydra-shuffling-text)
                 (nil spotify-hydra-not-shuffling-text)))
    (:repeating :repeating-bool
                ((t spotify-hydra-repeating-text)
                 (nil spotify-hydra-not-repeating-text)))
    (:playing   :playing-bool
                ((t spotify-hydra-playing-text)
                 (nil spotify-hydra-paused-text)))
    (:paused    :paused-bool
                ((nil spotify-hydra-paused-text)
                 (t spotify-hydra-playing-text)))
    (:muted     :muted-bool
                ((t spotify-hydra-muted-text)
                 (nil spotify-hydra-not-muted-text))))
  "A dictionary for translating fields in
`spotify-player-status-field'.

See docstring for `spotify--player-status-translators' for more info.")


;;------------------------------------------------------------------------------
;; Interface
;;------------------------------------------------------------------------------
;; (defun spotify--hydra-hook-into-remote-mode ()
;;   "Hooks hydra setup/teardown into remote mode."
;;   (spotify--with-hydra
;;     (add-hook 'spotify-remote-mode-hook #'spotify--hydra-hook)))

;; (defun spotify--hydra-hook ()
;;   "Configures the Hydra as needed."
;;   ;; Don't error... hooks called too often.
;;   (spotify--with-hydra
;;     (if spotify-remote-mode
;;         ;; Check if setup needed.
;;         (progn
;;           )

;;         )))


;;------------------------------------------------------------------------------
;; The Hydra
;;------------------------------------------------------------------------------
;; Just skip the Hydra if we don't have Hydra? Seems reasonable...
(spotify--with-hydra
  (defhydra spotify-hydra (:color blue ;; default to exit heads after invoked
                                  :idle 0.25  ;; delay docstr for this many secs
                                  :hint none  ;; no head hints - just the docstr
                                  ;; run this before running main hydra body
                                  :body-pre (spotify--hydra-pre-check))
    "
%s(spotify--hydra-status-format 65)
^Track^                 ^Playlists^                ^Misc^
^-^---------------------^-^------------------------^-^-----------------
_p_: ?p?^^^^^^^^^^^^  _l m_: My Lists              _x_  Select Device
_b_: Back a Track     _l f_: Featured Lists
_f_: Forward a Track  _l u_: User Lists            _u_: Volume Up
_s_: Search Track     _l s_: Search List           _d_: Volume Down
_r_: Recently Played  _l c_: Create list         _t m_: ?t m?^^^^^^^^^^^^^
^ ^                   ^   ^                      _t r_: ?t r?^^^^^^^^^^^^^
_q_/_ESC_: quit         ^   ^                      _t s_: ?t s?^^^^^^^^^^^^^
"

    ;;---
    ;; Player Status
    ;;---
    ("p" spotify-toggle-play
     (format "%-15s" (spotify-player-status-field
                      :playing
                      spotify--hydra-translators)))

    ("t r" spotify-toggle-repeat
     (format "%-18s" (spotify-player-status-field
                      :repeating
                      spotify--hydra-translators)))

    ("t s" spotify-toggle-shuffle
     (format "%-18s" (spotify-player-status-field
                      :shuffling
                      spotify--hydra-translators)))

    ("t m" spotify-volume-mute-unmute
     (format "%-18s" (spotify-player-status-field
                      :muted
                      spotify--hydra-translators)))

    ("x" spotify-select-device)


    ;;---
    ;; Track
    ;;---
    ("b" spotify-previous-track :color red)
    ("f" spotify-next-track);; :color red)

    ("s" spotify-track-search)
    ("r" spotify-recently-played)

    ;;---
    ;; Playlist
    ;;---
    ("l m" spotify-my-playlists)
    ("l f" spotify-featured-playlists)
    ("l u" spotify-user-playlists)
    ("l s" spotify-playlist-search)
    ("l c" spotify-create-playlist)

    ;;---
    ;; Volume & Misc
    ;;---
    ("u" spotify-volume-up   :color red)
    ("d" spotify-volume-down :color red)

    ;; quit - no need to do anything; just nil
    ("q"   nil)
    ("ESC" nil)
    )
  )


;;------------------------------------------------------------------------------
;; Hydra Helpers
;;------------------------------------------------------------------------------

(defun spotify--hydra-status-format (&optional center-at)
  "Formats `spotify-hydra-format' using
cached status. Requires `spotify-cache-player-status-enabled' to
be non-nil.

If CENTER-AT is an integer and `s-center' is present (functionp),
centers string at that length."
  (spotify--hydra-or-error "spotify--hydra-status-format"

    ;; (message "hydra hello: %S" (spotify-player-status-field :device-active-state))

    (let ((state (spotify-player-status-field :device-active-state)))
      (cond
       ;; :device-active-state of true is fine.
       ;; :device-active-state of 'unsupported I want to ignore, so also fine.
       ((or (eq state t)
            (eq state 'unsupported))
        ;; get and return format, centered if we can/want
        (let ((ret-val (spotify--player-status-format
                        spotify-hydra-format
                        nil
                        spotify--hydra-translators)))
          (if (and center-at
                   (integerp center-at)
                   (functionp 's-center))
              (s-center center-at ret-val)
            ret-val)))

       ;; other two device-active states I want to give feedback
       ((or (eq state 'nil) (eq state 'undefined))
        (if (and center-at
                 (integerp center-at)
                 (functionp 's-center))
            ;; Note we rely on cache so until device mode starts using cache
            ;; this can be outdated after a device select...
            (s-center center-at "No device is known to be active.")
          "No device is known to be active."))

       (t
        (error "spotify--hydra-status-format: unhandled condition"))))))


(defun spotify--hydra-pre-check ()
  "Sanity check before running hydra..."
  (unless spotify-remote-mode
    (if (bound-and-true-p spotify-hydra-auto-remote-mode)
        (global-spotify-remote-mode 1)
      (error (concat "Spotify-Remote-Mode is not enabled... "
                     "Call `spotify-remote-mode' or "
                     "`global-spotify-remote-mode'.")))))


;;------------------------------------------------------------------------------
;; Setup
;;------------------------------------------------------------------------------

;; Only setup hydra keybind if it's defined...
(when (and (bound-and-true-p spotify-hydra-keybind)
           (stringp spotify-hydra-keybind))

  ;; ...and we have hydra.
  (spotify--hydra-or-error "Spotify-Hydra.el setup"

    ;; Prefer `bind-key' over `define-key'. Bind hydra on `global-map'.
    ;; We're binding to `spotify-hydra/body' manually, instead of using hydra's
    ;; bindings, so that the hydra help docstring is actually shown/useful. Not
    ;; sure about more default Emacs, but mine shows nothing or `which-key'
    ;; info if I use hydra's binding.
    (if (featurep 'bind-key)
        (bind-key spotify-hydra-keybind #'spotify-hydra/body)
      (define-key 'global-map
        (kbd spotify-hydra-keybind)
        #'spotify-hydra/body))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-hydra)
