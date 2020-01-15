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
    (:artist                spotify--api/data/artist-simple  name)
    (:track                 spotify--api/data/track-full     name)
    (:track-number          spotify--api/data/track-full     track_number)
    (:duration-millisecond  spotify--api/data/track-full     duration_ms)

    ;;---
    ;; Player Status
    ;;---
    (:device-active-id      spotify--api/data/device-full    id)
    (:device-active-name    spotify--api/data/device-full    name)
    (:device-active-state   spotify--api/data/device-full    is_active)
    (:shuffling-bool        spotify--api/data/player-status  shuffle_state)
    (:repeating-bool        spotify--api/data/player-status  repeat_state)
    (:playing-bool          spotify--api/data/player-status  is_playing)

    (:volume                spotify--api/data/device-full    volume_percent))
  "Symbols that can be passed into `spotify-player-status' for
getting values from the player status.")


;;------------------------------------------------------------------------------
;; Spotify Connect API Helpers
;;------------------------------------------------------------------------------

(defun spotify--api-json-get-field (type json field)
  "Checks that FIELD is an expected member of the json TYPE, then
tries to get it from JSON (hash-table). This will not walk into
sub-objects of JSON; FIELD is expected to be a top-level member.

If not in JSON, this will return nil.
"
  (unless (assoc field type)
    (error (concat "spotify--api-json-get-field: "
                   "field '%s' unknown for type. Choose from: %S. json: %S")
           field type json))
  (gethash field json))


;;------------------------------------------------------------------------------
;; Spotify Connect API: Field Getters
;;------------------------------------------------------------------------------

(defun spotify--api-object-get-field (object key type)
  "
OBJECT is a JSON (hash-table) object from Spotify Connect API endpoint return.

KEY is an alist entry from `spotify--keyword->api-field'.

TYPE is the OBJECT type, as laid out at the bottom of this file. e.g.:
  - `spotify--api/data/player-status'
  - `spotify--api/data/track-full'
  - `spotify--api/data/artist-simple'
  - etc

Uses KEY to translate from Spotify.el keyword to Spotify Connect API field name,
checks that KEY is for TYPE.

Then gets value for field from OBJECT, and sanity checks it using data from TYPE.
"
  ;; null/sanity checks
  (if (or (null object)
          (not (listp key))
          (not (eq (eval (nth 1 key)) type)))
      (error "spotify--api-object-get-field: checks failed. %S %S %S"
             (null object)
             (not (listp key))
             (not (eq (eval (nth 1 key)) type)))

    (let* ((object-def  (nth 1 key))
           (field       (nth 2 key))
           (value       (gethash field object))
           ;; assoc gets (field checker-info) from type,
           ;; then we have to get that 2nd element for the value-check.
           (value-check (nth 1 (assoc field type))))

      ;; even more null checks now that we got some things...
      (if (or (null object-def)
              (null field)
              (null value-check))
          (error
           "spotify--api-object-get-field: setup failed. %S %S %S (value: %S)"
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

         ;; not implemented -> nil (or error?)
         ((eq value-check :spotify--no-impl)
          (error (concat "spotify--api-object-get-field: "
                         "key:'%S'->field:'%S' Not Implemented: "
                         "value: %S, value-check: %S")
                 key
                 field
                 value
                 value-check)
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
         ;; Other things: IDK -> nil (or error?)
         (t
          (error (concat
                  "spotify--api-object-get-field: "
                  "couldn't validate value... "
                  "value: %S, value-check: %S. "
                  "(func?: %S, check?: %S)"
                  "(no impl?: %S)"
                  "(bool?: %S, check?: %S)"
                  )
                 value
                 value-check

                 (functionp value-check)
                 (if (functionp value-check) (funcall value-check value) "NA")

                 (eq value-check :spotify--no-impl)

                 (and (listp value-check)
                      (eq (nth 0 value-check) :boolean))
                 (if (and (listp value-check)
                          (eq (nth 0 value-check) :boolean))
                     (let ((false (nth 1 value-check))
                           (comparator (or (nth 2 value-check)
                                           #'eq)))
                       (not (funcall comparator value false)))))
          nil))))))


;;------------------------------------------------------------------------------
;; Spotify Connect API: Object Getters
;;------------------------------------------------------------------------------

(defun spotify--api-device-field (device keyword)
  "KEYWORD should be a spotify.el keyword, /not/ a Spotify Connect API JSON
object's/response's field name.

DEVICE should be a JSON hash table representation of a device
object in a return value from Spotify Connect API's player status
call or devices list call.

Translates the KEYWORD to a field name, and gets it from the DEVICE.

If an incorrect/non-existant KEYWORD or not in DEVICE, this will return nil.
"
  (spotify--api-object-get-field device
                                 (assoc keyword spotify--keyword->api-field)
                                 spotify--api/data/device-full))


(defun spotify--api-artist-field (artist keyword)
  "KEYWORD should be a spotify.el keyword, /not/ a Spotify Connect API JSON
object's/response's field name.

ARTIST should be a JSON hash table representation of an artist
object in a return value from Spotify Connect API's player status
call.

Translates the KEYWORD to a field name, and gets it from the ARTIST.

If an incorrect/non-existant KEYWORD or not in ARTIST, this will return nil.
"
  (spotify--api-object-get-field artist
                                 (assoc keyword spotify--keyword->api-field)
                                 spotify--api/data/artist-simple))


(defun spotify--api-track-field (track keyword)
  "KEYWORD should be a spotify.el keyword, /not/ a Spotify Connect API JSON
object's/response's field name.

TRACK should be a JSON hash table representation of a track
object in a return value from Spotify Connect API's player status
call.

Translates the KEYWORD to a field name, and gets it from the TRACK.

If an incorrect/non-existant KEYWORD or not in TRACK, this will return nil.
"
  (spotify--api-object-get-field track
                                 (assoc keyword spotify--keyword->api-field)
                                 spotify--api/data/track-full))


(defun spotify--api-player-status-field (status keyword)
  "KEYWORD should be a spotify.el keyword, /not/ a Spotify Connect API JSON
object's/response's field name.

STATUS should be a JSON hash table representation of a return
value from Spotify Connect API's player status call.

Translates the KEYWORD to a field name, and gets it from the STATUS.

If an incorrect/non-existant KEYWORD or not in STATUS, this will return nil.
"
  (spotify--api-object-get-field status
                                 (assoc keyword spotify--keyword->api-field)
                                 spotify--api/data/player-status))


;;------------------------------------------------------------------------------
;; Spotify Connect API: Endpoint Replies
;;   - https://developer.spotify.com/documentation/web-api/reference/player/
;;------------------------------------------------------------------------------

(defun spotify--api-devices (json)
  "Gets device list from \"/v1/me/player/devices\" endpoint of
Spotify Connect API.

JSON must be a json reply formatted hash-table (by json.el) from
Spotify Connect API. See:
https://developer.spotify.com/documentation/web-api/guides/using-connect-web-api/

Returns list of devices from the json. These will have
`spotify--api/data/device-full' fields.
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
       ((eq type 'spotify--api/data/player-status)
        ;; It's in player-status object and that's what we (expect to) have.
        (spotify--api-player-status-field json keyword))

       ((eq type 'spotify--api/data/track-full)
        ;; It's in the track object, so get that from json status for
        ;; passing in.
        (spotify--api-track-field
         (spotify--api-json-get-field spotify--api/data/player-status
                                      json
                                      'item)
         keyword))

       ((eq type 'spotify--api/data/artist-simple)
        ;; It's in the artists object, so:
        ;; json status -> track -> artists -> first artist only.
        (spotify--api-artist-field
         ;; first off: get 'artists from track.
         (nth 0 (spotify--api-json-get-field spotify--api/data/track-full
                 ;; get track from player-status
                 (spotify--api-json-get-field spotify--api/data/player-status
                                              json 'item)
                 'artists))
         keyword))

       ((eq type 'spotify--api/data/device-full)
        ;; It's in the device object, so get that from  json status.
        (spotify--api-device-field
         (spotify--api-json-get-field spotify--api/data/player-status
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
;; Spotify Connect API: Player Status Context Fields
;;------------------------------------------------------------------------------

;; Take a lot of room, so down here.


(defconst spotify--api/data/player-status
  '(;; Device Object
    ;;   - The device that is currently active
    (device spotify--api/data/device-full)

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
    (item spotify--api/data/track-full)

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


(defconst spotify--api/data/track-full
  '(;; a simplified album object
    ;;   - The album on which the track appears. The album object includes a
    ;;     link in href to full information about the album.
    (album :spotify--no-impl)

    ;; an array of simplified artist objects
    ;;   - The artists who performed the track. Each artist object includes a
    ;;     link in href to more detailed information about the artist.
    (artists spotify--api/data/artist-simple)

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


(defconst spotify--api/data/artist-simple
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


(defconst spotify--api/data/device-full
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
    (name stringp)

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


(defconst spotify--api/data/paging
  '(;; string
    ;; A link to the Web API endpoint returning the full result of the request.
    (href stringp)

    ;; an array of objects
    ;; The requested data. Can't specify a sanity check, really, as what it is
    ;; depends on request.
    (items ignore)

    ;; integer
    ;; The maximum number of items in the response (as set in the query or by
    ;; default).
    (limit integerp)

    ;; string
    ;; URL to the next page of items. ( null if none)
    (next stringp)

    ;; integer
    ;; The offset of the items returned (as set in the query or by default).
    (offset integerp)

    ;; string
    ;; URL to the previous page of items. ( null if none)
    (previous stringp)

    ;; integer
    ;; The maximum number of items available to return.
    (total integerp))
  "Paging object from Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/object-model/#paging-object
")


;; Many other objects but we're not using them (that I've seen) yet.
;;   https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-api-json)
