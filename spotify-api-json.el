;;; spotify-json.el --- JSON read/write for statuses. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Code for reading fields from the Spotify Connect Web API
;; JSON responses/objects.

;;; Code:

;;---------------------------------Spotify.el-----------------------------------
;;--                     Spotify Connect Web API Reading                      --
;;------------------------------------------------------------------------------


(require 'json)
(require 'spotify-json)


;;------------------------------------------------------------------------------
;; Links
;;------------------------------------------------------------------------------

;; JSON API Endpoints:
;; https://developer.spotify.com/documentation/web-api/reference/player/

;; JSON Objects:
;; https://developer.spotify.com/documentation/web-api/reference/object-model/

;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst spotify--keyword->api-field
  '(;;---
    ;; Track Status
    ;;---
    (:artist                spotify--api-artist-simple  name)
    (:track                 spotify--api-track-full     name)
    (:track-number          spotify--api-track-full     track_number)
    (:duration-millisecond  spotify--api-track-full     duration_ms)

    ;;---
    ;; Player Status
    ;;---
    (:device-active-id      spotify--api-device-full    id)
    (:device-active-name    spotify--api-device-full    name)
    (:device-active-state   spotify--api-device-full    is_active)
    (:shuffling-bool        spotify--api-player-status  shuffle_state)
    (:repeating-bool        spotify--api-player-status  repeate_state)
    (:playing-bool          spotify--api-player-status  is_playing)

    (:volume                spotify--api-device-full    volume_percent))
  "Symbols that can be passed into `spotify-player-status' for
getting values from the player status.")


;;------------------------------------------------------------------------------
;; Spotify Connect API: Endpoint Replies
;;   - https://developer.spotify.com/documentation/web-api/reference/player/
;;------------------------------------------------------------------------------

(defun spotify--api-devices (json keyword)
  "Gets device list from \"/v1/me/player/devices\" endpoint of Spotify Connect API.

JSON must be a json reply formatted hash-table (by json.el) from
Spotify Connect API. See:
https://developer.spotify.com/documentation/web-api/guides/using-connect-web-api/

Returns list of devices from the json. These will have
`spotify--api-device-full' fields.
"
  (if-let ((json json)
           (devices (gethash 'devices json)))
      devices
    nil))


(defun spotify--api-player-status (json keyword)
  "For json from \"/v1/me/player\" endpoint of Spotify Connect API.

Returns value of KEYWORD in STATUS (wherever it is - even in
json sub-objects), or nil. KEYWORD must be one of the 'raw' keywords
in `spotify-player-status-fields', not an actual keyword in the
status. See `spotify--keyword->api-field' for the valid KEYWORDs.

STATUS must be json reply formatted hash-table (by json.el) from
Spotify Connect API. See:
https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/

If STATUS is nil, nil will be return value.
"
  (if-let* ((json json)
            (type (nth 1 (assoc keyword spotify--keyword->api-field))))

      (cond
       ((eq type 'spotify--api-player-status)
        ;; It's in player-status object and that's what we (expect to) have.
        (spotify--api-player-status-field json keyword))

       ((eq type 'spotify--api-track-full)
        ;; It's in the track object, so get that from json status for
        ;; passing in.
        (spotify--api-player-status-field
         (spotify--api-json-get-field spotify--api-player-status
                                      json
                                      'item)
         keyword))

       ((eq type 'spotify--api-artist-simple)
        ;; It's in the artists object, so:
        ;; json status -> track -> artists -> first artist only.
        (spotify--api-player-status-field
         ;; first of: get 'artists from track.
         (nth 0 (spotify--api-json-get-field
                 spotify--api-track-full
                 ;; get track from player-status
                 (spotify--api-json-get-field spotify--api-player-status
                                              json 'item)
                 'artists))
         keyword))

       ((eq type 'spotify--api-device-full)
        ;; It's in the device object, so get that from  json status.
        (spotify--api-player-status-field
         (spotify--api-json-get-field spotify--api-player-status
                                      json
                                      'device)
         keyword))

       ;; Otherwise, dunno - error.
       (t
        (error
         "spotify--api-player-status: don't know what to do with keyword: %S %S"
         keyword
         "(is it in `spotify--keyword->api-field'?)")))

    ;; else for if-let: something is null.
    (error "spotify--api-player-status: if-let failed: %S %S->%S %S"
           (null json)
           keyword
           type
           "(is keyword in `spotify--keyword->api-field'?)")))


;;------------------------------------------------------------------------------
;; Spotify Connect API Helpers
;;------------------------------------------------------------------------------

(defun spotify--api-json-verify-type (json type)
  "Checks fields in JSON to verify that it is the TYPE expected."
  (let ((valid t))
    (dolist (field type valid)
      (if (and valid
               (eq (gethash field json :spotify-key-dne) :spotify-key-dne))
          ;; §-TODO-§ [2019-11-11]: error out instead?
          (setq valid nil)))))


(defun spotify--api-json-get-field (type json field)
  "Checks that FIELD is an expected member of the json TYPE, then
tries to get it from JSON (hash-table). This will not walk into
sub-objects of JSON; FIELD is expected to be a top-level member.

If not in JSON, this will return nil.
"
  (when (not (member field type))
    (error (concat "spotify--api-json-has-field: "
                   "field '%s' unknown for type '%s'. Choose from: %s")
           field (symbol-name type) type))
  (gethash field json))


;;------------------------------------------------------------------------------
;; Spotify Connect API: Field Getters
;;------------------------------------------------------------------------------

(defun spotify--api-object-get-field (object key type)
  "§-TODO-§ [2019-11-11] this
"
  ;; null/sanity checks
  (if (or (null object)
          (not (listp key))
          (not (eq (nth 1 key) type)))
      (error "spotify--api-status-object-get: checks failed. %S %S %S"
             (null object)
             (not (listp key))
             (not (eq (nth 1 key) type)))

    (let* ((object-def  (nth 1 key))
           (field       (nth 2 key))
           (value       (gethash field object))
           (value-check (assoc field type)))
      ;; even more null checks now that we got some things...
      (if (or (null object-def)
              (null field)
              (null value-check))
          (error
           "spotify--api-status-object-get: setup failed. %S %S %S (value: %S)"
           object-def
           field
           value-check
           value)

        ;; validate value itself
        (cond
         ;; a function to validate
         ((and (functionp value-check)
               (funcall value-check value))
          ;; valid - just return it
          value)

         ;; not implemented -> nil
         ((eq value-check :spotify--no-impl)
          nil)

         ;; boolean - figure out t/nil
         ((and (listp value-check)
               (eq (nth 0 value-check) :boolean))
          ;; convert to t/nil boolean using either 3rd element func or `eq'.
          (let ((false (nth 1 value-check))
                (comparator (or (nth 2 value-check)
                                #'eq)))
            ;; We're checking fo true via "not equal to false" for
            ;; "repeat_state" reasons, basically... We have more object field
            ;; values that become true than false right now.
            (not (funcall comparator value false))))

         ;; sub-objects: Not putting the smarts for that in here... just
         ;; return nil.
         ;; Other things: IDK -> nil
         (t
          nil))))))


(defun spotify--api-player-status-field (status keyword)
  "§-TODO-§ [2019-11-11]: This
"
  (spotify--api-object-get-field status
                                 (assoc keyword spotify--keyword->api-field)
                                 spotify--api-player-status))


(defun spotify--api-track-field (track keyword)
  "§-TODO-§ [2019-11-11]: This
"
  (spotify--api-object-get-field status
                                 (assoc keyword spotify--keyword->api-field)
                                 spotify--api-track-full))


(defun spotify--api-artist-field (artist keyword)
  "§-TODO-§ [2019-11-11]: This
"
  (spotify--api-object-get-field status
                                 (assoc keyword spotify--keyword->api-field)
                                 spotify--api-artist-simple))


(defun spotify--api-device-field (device keyword)
  "§-TODO-§ [2019-11-11]: This
"
  (spotify--api-object-get-field status
                                 (assoc keyword spotify--keyword->api-field)
                                 spotify--api-device-full))


;; §-TODO-§ [2019-11-11]: delete
;; (defun spotify--api-json-status-field (status keyword)
;;   "Returns value of KEYWORD in STATUS (wherever it is - even in
;; json sub-objects), or nil. KEYWORD must be one of the 'raw' keywords
;; in `spotify-player-status-fields', not an actual keyword in the
;; status. See `spotify--keyword->api-field' for the valid KEYWORDs.

;; STATUS must be json reply formatted hash-table (by json.el) from
;; Spotify Connect API. See:
;; https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/

;; If STATUS is nil, nil will be return value.
;; "
;;   (if-let* ( ;; null check our important json objects
;;             (status status)
;;             (track (spotify--api-json-get-field spotify--api-player-status
;;                                                 status
;;                                                 'item))
;;             (device (spotify--api-json-get-field spotify--api-player-status
;;                                                  status
;;                                                  'device))
;;             (artists (spotify--api-json-get-field spotify--api-track-full
;;                                                   track
;;                                                   'artists))
;;             ;; now figure out some things...
;;             ;;
;;             (field (assoc keyword spotify--keyword->api-field))
;;             (field-object (cond ((eq (nth 1 field)
;;                                      spotify--api-player-status)
;;                                  status)
;;                                 ((eq (nth 1 field)
;;                                      spotify--api-track-full)
;;                                  track)
;;                                 ((eq (nth 1 field)
;;                                      spotify--api-device-full)
;;                                  device)
;;                                 ((eq (nth 1 field)
;;                                      spotify--api-artist-simple)
;;                                  (car artists)))))

;;       ;; if-let* success case
;;       ;; get the field from its object...
;;       (let ((value (gethash field field-object)))


;;       ;; if-let* success case
;;       ;; get the field from the cache
;;       (cond
;;        ;;---
;;        ;; Track Status
;;        ;;---
;;        ((eq field :artist)
;;         (spotify--api-json-get-field spotify--api-artist-simple
;;                                      ;; just first artist
;;                                      (car artists)
;;                                      'name))

;;        ((eq field :track)
;;         (spotify--api-json-get-field spotify--api-track-full
;;                                      track
;;                                      'name))

;;        ((eq field :track-number)
;;         (spotify--api-json-get-field spotify--api-track-full
;;                                      track
;;                                      'track_number))

;;        ;; raw duration
;;        ((eq field :duration-millisecond)
;;         (spotify--api-json-get-field spotify--api-track-full
;;                                      track
;;                                      'duration_ms))

;;        ;;---
;;        ;; Player Status
;;        ;;---

;;        ((eq field :shuffling-bool)
;;         (spotify--json-bool-decode
;;          ;; convert this field to bool...
;;          (spotify--api-json-get-field spotify--api-player-status
;;                                       status
;;                                       'shuffle_state)
;;          ;; with :json-false as the false comparator
;;          :json-false))

;;        ((eq field :repeating-bool)
;;         (spotify--json-bool-decode
;;          ;; convert this field to bool...
;;          (spotify--api-json-get-field spotify--api-player-status
;;                                       status
;;                                       'repeat_state)
;;          ;; with "off" and `string=' as the false comparator
;;          "off" #'string=))

;;        ((eq field :playing-bool)
;;         (spotify--json-bool-decode
;;          ;; convert this field to bool...
;;          (spotify--api-json-get-field spotify--api-player-status
;;                                       status
;;                                       'is_playing)
;;          ;; with :json-false as the false comparator
;;          :json-false))

;;        ((eq field :paused-bool)
;;         ;; ...if it's not playing, it's paused?
;;         ;; What about stopped or not started yet? *shrug* Paused I guess.
;;         (not
;;          (spotify--json-bool-decode
;;           ;; convert this field to bool...
;;           (spotify--api-json-get-field spotify--api-player-status
;;                                        status
;;                                        'is_playing)
;;           ;; with :json-false as the false comparator
;;           :json-false)))

;;        ((eq field :volume)
;;         (spotify--api-json-get-field spotify--api-device-full
;;                                      device
;;                                      'volume_percent))

;;        ((eq field :muted-bool)
;;         ;; no special field - just 0 volume
;;         (= 0
;;            (spotify--api-json-get-field spotify--api-device-full
;;                                         device
;;                                         'volume_percent)))

;;        ((eq field :device-active-state)
;;         ;; get field
;;         (let ((active (spotify--api-json-get-field
;;                        spotify--api-device-full
;;                        device
;;                        'is_active)))
;;           ;; Convert - active if 'is_active field says so.
;;           (cond ((eq active t)
;;                  t)
;;                 ((eq active :json-false)
;;                  nil)
;;                 ;; didn't find device - undefined?
;;                 (t 'undefined))))

;;        ;;---
;;        ;; You should hopefully not get here.
;;        ;;---
;;        (t
;;         (error (concat "spotify--json-status-field: "
;;                        "field '%s' known but not handled? Sorry.")
;;                field)))

;;     ;; And return nil in case you're debugging and have a message
;;     ;; just above here... >.>
;;     nil))


;;------------------------------------------------------------------------------
;; Spotify Connect API: Player Status Context Fields
;;------------------------------------------------------------------------------

;; Take a lot of room, so down here.


(defconst spotify--api-player-status
  '(;; Device Object
    ;;   - The device that is currently active
    (device spotify--api-device-full)

    ;; string
    ;;   - "off", "track", "context"
    ;;   - but we will convert to boolean with "context" to t and rest to nil.
    (repeat_state (:boolean "off" string=))

    ;; boolean
    ;;   - If shuffle is on or off
    (shuffle_state (:boolean :json-false))

    ;; a Context Object
    ;;   - A Context Object. Can be null (e.g. If private session is enabled
    ;;     this will be null).
    (context :spotify--no-impl)

    ;; integer
    ;;   - Unix Millisecond Timestamp when data was fetched
    (timestamp :spotify--no-impl)

    ;; integer
    ;;   - Progress into the currently playing track. Can be null (e.g. If
    ;;     private session is enabled this will be null).
    (progress_ms :spotify--no-impl)

    ;; boolean
    ;;   - If something is currently playing.
    (is_playing (:boolean :json-false))

    ;; A Full Track Object
    ;;   - The currently playing track. Can be null (e.g. If private session is
    ;;     enabled this will be null).
    (item spotify--api-track-full)

    ;; string
    ;;   - The object type of the currently playing item. Can be one of track,
    ;;     episode, ad or unknown.
    (currently_playing_type :spotify--no-impl)

    ;; An actions object which contains a disallows object
    ;;   - Allows to update the user interface based on which playback actions
    ;;     are available within the current context
    (actions :spotify--no-impl))
  "Fields in a player-status response from Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/
")


(defconst spotify--api-track-full
  '(;; a simplified album object
    ;;   - The album on which the track appears. The album object includes a
    ;;     link in href to full information about the album.
    (album :spotify--no-impl)

    ;; an array of simplified artist objects
    ;;   - The artists who performed the track. Each artist object includes a
    ;;     link in href to more detailed information about the artist.
    (artists spotify--api-artist-simple)

    ;; array of strings
    ;;   - A list of the countries in which the track can be played, identified
    ;;     by their ISO 3166-1 alpha-2 code.
    (available_markets :spotify--no-impl)

    ;; integer
    ;;   - The disc number (usually 1 unless the album consists of more than one
    ;;     disc).
    (disc_number :spotify--no-impl)

    ;; integer
    ;;   - The track length in milliseconds.
    (duration_ms integerp)

    ;; Boolean
    ;;   - Whether or not the track has explicit lyrics ( true = yes it does;
    ;;   - false = no it does not OR unknown).
    (explicit :spotify--no-impl)

    ;; an external ID object
    ;;   - Known external IDs for the track.
    (external_ids :spotify--no-impl)

    ;; an external URL object
    ;;   - Known external URLs for this track.
    (external_urls :spotify--no-impl)

    ;; string
    ;;   - A link to the Web API endpoint providing full details of the track.
    (href :spotify--no-impl)

    ;; string
    ;;   - The Spotify ID for the track.
    (id :spotify--no-impl)

    ;; boolean
    ;;   - Part of the response when Track Relinking is applied. If true , the
    ;;     track is playable in the given market. Otherwise false.
    (is_playable :spotify--no-impl)

    ;; a linked track object
    ;;   - Part of the response when Track Relinking is applied, and the
    ;;     requested track has been replaced with different track. The track in
    ;;     the linked_from object contains information about the originally
    ;;     requested track.
    (linked_from :spotify--no-impl)

    ;; a restrictions object
    ;;   - Part of the response when Track Relinking is applied, the original
    ;;     track is not available in the given market, and Spotify did not have
    ;;     any tracks to relink it with. The track response will still contain
    ;;     metadata for the original track, and a restrictions object containing
    ;;     the reason why the track is not available: "restrictions" : {"reason"
    ;;     : "market"}
    (restrictions :spotify--no-impl)

    ;; string
    ;;   - The name of the track.
    (name stringp)

    ;; integer
    ;;   - The popularity of the track. The value will be between 0 and 100,
    ;;     with 100 being the most popular.
    (popularity :spotify--no-impl)

    ;; string
    ;;   - A link to a 30 second preview (MP3 format) of the track. Can be null
    (preview_url :spotify--no-impl)

    ;; integer
    ;;   - The number of the track. If an album has several discs, the track number is the number on the specified disc.
    (track_number integerp)

    ;; string
    ;;   - The object type: “track”.
    (type :spotify--no-impl)

    ;; string
    ;;   - The Spotify URI for the track.
    (uri :spotify--no-impl)

    ;; boolean
    ;;   - Whether or not the track is from a local file.
    (is_local :spotify--no-impl))

  "Fields in a track object from Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full
")


(defconst spotify--api-artist-simple
  '(;; an external URL object
    ;;   - Known external URLs for this artist.
    (external_urls :spotify--no-impl)

    ;; string
    ;;   - A link to the Web API endpoint providing full details of the artist.
    (href :spotify--no-impl)

    ;; string
    ;;   - The Spotify ID for the artist.
    (id :spotify--no-impl)

    ;; string
    ;;   - The name of the artist
    (name stringp)

    ;; string
    ;;   - The object type: "artist"
    (type :spotify--no-impl)

    ;; string
    ;;   - The Spotify URI for the artist.
    (uri :spotify--no-impl))
  "Fields in a simplified artist object from Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/object-model/#artist-object-simplified
")


(defconst spotify--api-device-full
  '(;; string
    ;;   - The device ID. This may be null.
    (id stringp)

    ;; boolean
    ;;   - If this device is the currently active device.
    (is_active (:boolean :json-false))

    ;; boolean
    ;;   - If this device is currently in a private session.
    (is_private_session :spotify--no-impl)

    ;; boolean
    ;;   - Whether controlling this device is restricted. At present if this is “true” then no Web API commands will be accepted by this device.
    (is_restricted :spotify--no-impl)

    ;; string
    ;;   - The name of the device.
    (name :spotify--no-impl)

    ;; string
    ;;   - Device type. Options are:
    ;;     - Computer, Tablet, Smartphone, Speaker, TV, AVR, STB, AudioDongle,
    ;;       GameConsole, CastVideo, CastAudio, Automobile, Unknown
    (type :spotify--no-impl)

    ;; integer
    ;;   - The current volume in percent. This may be null.
    (volume_percent integerp))
  "Device object from Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/player/get-a-users-available-devices/
")


;; Many other objects but we're not using them (that I've seen) yet.
;;   https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full


;;------------------------------------------------------------------------------
;; §-TODO-§ [2019-11-10]: delete all this, or make real tests...
;;------------------------------------------------------------------------------
;; (defun testfoo ()
;;   (interactive)
;;   (let* ((json-object-type 'hash-table)
;;          (json-array-type 'list)
;;          (json-key-type 'symbol)
;;          (json-obj (json-new-object))
;;          (json (json-read-from-string "{  \"artist\": \"Aesop Rock\",  \"name\": \"Shrunk\",  \"duration\": 265333,  \"track_number\": 9,  \"player_state\": \"playing\",  \"player_shuffling\": true,  \"player_repeating\": true }")))
;;     (message "your json: %S... hash? %S" json (hash-table-p json))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-api-json)
