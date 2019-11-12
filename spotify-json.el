;;; spotify-json.el --- JSON read/write for statuses. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Spotify.el code for json.el, working with json objects (encoding/decoding),
;; and our internal/simplified json from the simpler backeneds (DBUS, Apple).

;;; Code:

;;---------------------------------Spotify.el-----------------------------------
;;--                        JSON Reading and Encoding                         --
;;------------------------------------------------------------------------------


(require 'json)



;; §-TODO-§ [2019-11-11]: Json getter for each object/response.

;; §-TODO-§ [2019-11-11]: Api and json don't know/care about spotify.el keywords?

;; §-TODO-§ [2019-11-11]: Connect.el doesn't know/care about json fields?


;;------------------------------------------------------------------------------
;; Links
;;------------------------------------------------------------------------------

;; JSON API:
;; https://developer.spotify.com/documentation/web-api/reference/player/


;;------------------------------------------------------------------------------
;; json.el Settings
;;------------------------------------------------------------------------------

(defconst spotify--json-object-type 'hash-table
  "Our value for setting `json-object-type' in lets.")


(defconst spotify--json-key-type 'symbol
  "Our value for setting `json-key-type' in lets.")


(defconst spotify--json-array-type 'list
  "Our value for setting `json-array-type' in lets.")


;;------------------------------------------------------------------------------
;; Setup/Helper Macro
;;------------------------------------------------------------------------------

(defmacro spotify--build-json (&rest body)
  "Macro that sets up json.el's vars in a let-binding, then runs
BODY forms. `json-obj' is the var ready to take in fields.

Let and body are enclosed in a condition-case-unless-debug that demotes errors.

Sets:
  - `json-object-type' -> `spotify--json-object-type'
  - `json-array-type'  -> `spotify--json-array-type'
  - `json-key-type'    -> `spotify--json-key-type'

Creates:
  - `json-obj' <- `json-new-object'
"
  (declare (indent defun))

  `(condition-case-unless-debug spotify--error
       (let* ((json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (json-obj (json-new-object)))
         ;; ok - execute body forms
         ,@body)

     ;; Error handlers:
     (error ;; 'error' signal:
      ;; demote error to message unless debugging
      (message "spotify--build-json: error caught: %S" spotify--error))))
;; (spotify--build-json (message "hi"))
;; (spotify--build-json (error "hi"))


;;------------------------------------------------------------------------------
;; General Helpers
;;------------------------------------------------------------------------------

(defun spotify--json-bool-encode (value &optional false testfn)
  "Converts Emacs t and nil (or value of FALSE) to json t/json-false.

If TESTFN is non-nil, it will be used as the comparator. Default is `eq'.

If FALSE is non-nil, it will be used as the comparison. Default is 'nil.
"
  (let ((false (or false nil))
        (testfn (or testfn #'eq)))
    (if (funcall testfn value false)
        json-false
      t)))
;; (spotify--json-bool-encode nil)
;; (spotify--json-bool-encode t)
;; (spotify--json-bool-encode testfoo)


(defun spotify--json-bool-decode (value &optional false testfn)
  "Converts Spotify API's... wide variety of bools into Emacs t and nil.

If TESTFN is non-nil, it will be used as the comparator. Default is `eq'.

If FALSE is non-nil, it will be used as the comparison. Default is :json-false.
"
  (let ((false (or false :json-false))
        (testfn (or testfn #'eq)))
    (if (funcall testfn value false)
        nil
      t)))


;;------------------------------------------------------------------------------
;; Our Simpler Internal JSON
;;------------------------------------------------------------------------------

(defun spotify--json-encode-internal (artist name
                                      duration-ms track-number
                                      playing shuffling repeating)
  "Builds json from args.

Requirements:
  - artist:       stringp
  - name:         stringp
  - duration-ms:  integerp
  - track-number: integerp
  - playing:      emacs bool (nil/non-nil)
  - shuffling:    emacs bool (nil/non-nil)
  - repeating:    emacs bool (nil/non-nil)

Outputs:
  - artist:       string (input str)
  - name:         string (input str)
  - duration-ms:  number (input number)
  - track-number: number (input number)
  - playing:      string (nil->\"paused\", non-nil->\"playing\"
  - shuffling:    json bool (false, true)
  - repeating:    json bool (false, true)

Old Example: ;; §-TODO-§ [2019-11-10]: remove old example...?
  {
    \"artist\": \"Aesop Rock\",
    \"duration\": 265333,
    \"track_number\": 9,
    \"name\":  \"Shrunk\",
    \"player_state\": \"playing\",
    \"player_shuffling\": \"t\",
    \"player_repeating\": \"context\"
  }

Correct Example:
  {
    \"artist\": \"Aesop Rock\",
    \"duration\": 265333,
    \"track_number\": 9,
    \"name\":  \"Shrunk\",
    \"player_state\": \"playing\",
    \"player_shuffling\": true,
    \"player_repeating\": false
  }
"
  ;; error checking
  (cond ((or (null artist)
             (null name)
             (null duration-ms)
             (null track-number))
         (error "spotify--json-internal: these fields cannot be nil: %S %S %S %S"
                artist name duration-ms track-number))
        ((or (not (stringp artist))
             (not (stringp name)))
         (error "spotify--json-internal: these fields must be strings: %S %S"
                artist name))
        ((or (not (integerp duration-ms))
             (not (integerp track-number)))
         (error "spotify--json-internal: these fields must be integers: %S %S"
                duration-ms track-number))
        ;; kinda hard to error check nil/non-nil so we're done.
        (t
         nil))

  ;; spotify--build-json will set us up, then we can just stuff json-obj full.
  (spotify--build-json
    ;; Build status into new json-obj using json lib functions so the json
    ;; stays properly escaped and e.g. artist "Weird Al" doesn't ruin
    ;; everything for our json.
    (setq json-obj (json-add-to-object
                    json-obj "artist"
                    artist))
    (setq json-obj (json-add-to-object
                    json-obj "duration"
                    duration-ms))
    (setq json-obj (json-add-to-object
                    json-obj "track_number"
                    track-number))
    (setq json-obj (json-add-to-object
                    json-obj "name"
                    name))
    (setq json-obj (json-add-to-object
                    json-obj "player_state"
                    (if playing
                        "playing"
                      "paused")))
    (setq json-obj (json-add-to-object
                    json-obj "player_shuffling"
                    (spotify--json-bool-encode shuffling)))
    (setq json-obj (json-add-to-object
                    json-obj "player_repeating"
                    (spotify--json-bool-encode repeating)))

    ;; convert new json to string and return it
    (json-encode json-obj)))
;; (setq debug-on-error t)
;; (spotify--json-encode-internal "Aesop Rock" "Shrunk" 265333 9 "playing" t "context")
;; (spotify--json-encode-internal "\"Weird Al\" Yankovic" "Foil" 265333 9 "playing" nil nil)


(defun spotify--json-internal-status-field (status field)
  "Returns value of FIELD in STATUS, or nil. FIELD must be one of the 'raw'
fields in `spotify-player-status-fields', not an actual field in the status.
This function figures out the translation.

STATUS must be simplified JSON structure (e.g.
`spotify--json-encode-internal') or nil. If STATUS is nil, nil
will be return value.
"
  (if (or (not status)
          (not (hash-table-p status)))
      ;; nothing we can do...
      nil

    ;; if-let* success case
    ;; get the field from the cache
    (cond
     ;;---
     ;; Track Status
     ;;---
     ((eq field :artist)
      (gethash 'artist status))

     ((eq field :track)
      (gethash 'name status))

     ((eq field :track-number)
      (gethash 'track_number status))

     ;; raw duration
     ((eq field :duration-millisecond)
      ;; (message "internal duration-ms: %S->%S"
      ;;          (gethash 'duration status)
      ;;          (gethash 'duration status))
      (gethash 'duration status))

     ;;---
     ;; Player Status
     ;;---
     ((eq field :shuffling-bool)
      ;; (message "internal shuffling: %S->%S"
      ;;          (gethash 'player_shuffling status)
      ;;          (not (eq (gethash 'player_shuffling status) :json-false)))
      (not (eq (gethash 'player_shuffling status) :json-false)))

     ((eq field :repeating-bool)
      ;; (message "internal repeating: %S->%S"
      ;;          (gethash 'player_repeating status)
      ;;          (not (eq (gethash 'player_repeating status) :json-false)))
      (not (eq (gethash 'player_repeating status) :json-false)))

     ((eq field :playing-bool)
      ;; (message "internal playing: %S->%S"
      ;;          (gethash 'player_state status)
      ;;          (string= (gethash 'player_state status) "playing"))
      ;; ...if it's not not playing, it's playing!
      (string= (gethash 'player_state status) "playing"))

     ((eq field :paused-bool)
      ;; ...if it's not playing, it's paused?
      ;; What about stopped or not started yet? *shrug* Paused I guess.
      (not (string= (gethash 'player_state status) "playing")))

     ;;---
     ;; Unsupported
     ;;---
     ((eq field :volume)
      nil)

     ((eq field :muted-bool)
      nil)

     ((eq field :device-active-state)
      'unsupported)

     ;;---
     ;; You should hopefully not get here.
     ;;---
     (t
      (error (concat "spotify--json-internal-status-field: "
                     "field '%s' known but not handled? Sorry.")
             field)))))


(defun spotify--json-api-to-internal (player-status)
  "§-TODO-§ [2019-11-04]: This really should get deprecated and just allow
normalized-get to take care of things instead of building a
reduced set out of full return json...

PLAYER-STATUS must be a json reply formatted hash-table (by json.el) from
Spotify Connect API. See:
https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/

Uses Emacs's json.el lib to properly convert from Spotify Connect API's
full json output to a reduced json usable by Spotify.el.
"
  (if-let* ((player-status player-status)
            (track (gethash 'item player-status)))

      (spotify--json-encode-internal
       ;; artist
       (gethash 'name (car (gethash 'artists track)))
       ;; name
       (gethash 'name track)
       ;; duration-ms
       (gethash 'duration_ms track)
       ;; track-number
       (gethash 'track_number track)
       ;; playing
       (not (eq (gethash 'is_playing player-status) :json-false))
       ;; shuffling
       (not (eq (gethash 'shuffle_state player-status) :json-false))
       ;; repeating
       (not (string= (gethash 'repeat_state player-status) "off")))

    ;; player-status or track null - can't really do anything
    ;; other than return null
    nil))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-json)
