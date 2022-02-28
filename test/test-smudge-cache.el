;;; test-smudge-cache.el --- Tests for Smudge Cache -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cole Brown
;;
;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:
;;
;; TODO something in here...
;;
;;; Code:

(require 'json)


;;------------------------------------------------------------------------------
;; JSON Test Strings
;;------------------------------------------------------------------------------

;; Originally from an actual Spotify Connect Player Status return value.
;;   - All "spotify.com" URLs changed to "example.com".
;;   - All hex strings, hashes, etc has been anonymized (replaced with random
;;     hex strings of the same length).
(defconst test-smudge-cache--json-str-full
  "{
  \"device\" : {
    \"id\" : \"b9e016fbc8772cdd96dea6f0dc89b556238444c3\",
    \"is_active\" : true,
    \"is_private_session\" : false,
    \"is_restricted\" : false,
    \"name\" : \"smudge-cache-test-device\",
    \"type\" : \"Computer\",
    \"volume_percent\" : 58
  },
  \"shuffle_state\" : false,
  \"repeat_state\" : \"off\",
  \"timestamp\" : 1645470649904,
  \"context\" : {
    \"external_urls\" : {
      \"spotify\" : \"https://open.example.com/playlist/b166601b93c305458cdc21\"
    },
    \"href\" : \"https://api.example.com/v1/playlists/b166601b93c305458cdc21\",
    \"type\" : \"playlist\",
    \"uri\" : \"spotify:playlist:b166601b93c305458cdc21\"
  },
  \"progress_ms\" : 45213,
  \"item\" : {
    \"album\" : {
      \"album_type\" : \"album\",
      \"artists\" : [ {
        \"external_urls\" : {
          \"spotify\" : \"https://open.example.com/artist/d9d08ae52b6be40daee1c3\"
        },
        \"href\" : \"https://api.example.com/v1/artists/d9d08ae52b6be40daee1c3\",
        \"id\" : \"d9d08ae52b6be40daee1c3\",
        \"name\" : \"Anonymous\",
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:d9d08ae52b6be40daee1c3\"
      } ],
      \"available_markets\" : [
        \"AD\", \"AE\", \"AG\", \"AL\", \"AM\", \"AO\", \"AR\", \"AT\", \"AU\", \"AZ\",
        \"BA\", \"BB\", \"BD\", \"BE\", \"BF\", \"BG\", \"BH\", \"BI\", \"BJ\", \"BN\",
        \"BO\", \"BR\", \"BS\", \"BT\", \"BW\", \"BY\", \"BZ\", \"CA\", \"CD\", \"CG\",
        \"CH\", \"CI\", \"CL\", \"CM\", \"CO\", \"CR\", \"CV\", \"CW\", \"CY\", \"CZ\",
        \"DE\", \"DJ\", \"DK\", \"DM\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\",
        \"FI\", \"FJ\", \"FM\", \"FR\", \"GA\", \"GB\", \"GD\", \"GE\", \"GH\", \"GM\",
        \"GN\", \"GQ\", \"GR\", \"GT\", \"GW\", \"GY\", \"HK\", \"HN\", \"HR\", \"HT\",
        \"HU\", \"ID\", \"IE\", \"IL\", \"IN\", \"IQ\", \"IS\", \"IT\", \"JM\", \"JO\",
        \"JP\", \"KE\", \"KG\", \"KH\", \"KI\", \"KM\", \"KN\", \"KR\", \"KW\", \"KZ\",
        \"LA\", \"LB\", \"LC\", \"LI\", \"LK\", \"LR\", \"LS\", \"LT\", \"LU\", \"LV\",
        \"LY\", \"MA\", \"MC\", \"MD\", \"ME\", \"MG\", \"MH\", \"MK\", \"ML\", \"MN\",
        \"MO\", \"MR\", \"MT\", \"MU\", \"MV\", \"MW\", \"MX\", \"MY\", \"MZ\", \"NA\",
        \"NE\", \"NG\", \"NI\", \"NL\", \"NO\", \"NP\", \"NR\", \"NZ\", \"OM\", \"PA\",
        \"PE\", \"PG\", \"PH\", \"PK\", \"PL\", \"PS\", \"PT\", \"PW\", \"PY\", \"QA\",
        \"RO\", \"RS\", \"RU\", \"RW\", \"SA\", \"SB\", \"SC\", \"SE\", \"SG\", \"SI\",
        \"SK\", \"SL\", \"SM\", \"SN\", \"SR\", \"ST\", \"SV\", \"SZ\", \"TD\", \"TG\",
        \"TH\", \"TJ\", \"TL\", \"TN\", \"TO\", \"TR\", \"TT\", \"TV\", \"TW\", \"TZ\",
        \"UA\", \"UG\", \"US\", \"UY\", \"UZ\", \"VC\", \"VE\", \"VN\", \"VU\", \"WS\",
        \"XK\", \"ZA\", \"ZM\", \"ZW\"
      ],
      \"external_urls\" : {
        \"spotify\" : \"https://open.example.com/album/d308bec0cb474a2a37c0c3\"
      },
      \"href\" : \"https://api.example.com/v1/albums/d308bec0cb474a2a37c0c3\",
      \"id\" : \"d308bec0cb474a2a37c0c3\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://i.scdn.co/image/cc854ad296a5eb95bd33b8e2392f8d755c7d3ee7\",
        \"width\" : 640
      }, {
        \"height\" : 300,
        \"url\" : \"https://i.scdn.co/image/92de1eb4ebe8f849162e68f6e5cae5ed8f9facd4\",
        \"width\" : 300
      }, {
        \"height\" : 64,
        \"url\" : \"https://i.scdn.co/image/a11ca6474d00b116998bf4d0cd7e75406b16409b\",
        \"width\" : 64
      } ],
      \"name\" : \"Anonymous (Deluxe)\",
      \"release_date\" : \"2009-02-25\",
      \"release_date_precision\" : \"day\",
      \"total_tracks\" : 14,
      \"type\" : \"album\",
      \"uri\" : \"spotify:album:d308bec0cb474a2a37c0c3\"
    },
    \"artists\" : [ {
      \"external_urls\" : {
        \"spotify\" : \"https://open.example.com/artist/d9d08ae52b6be40daee1c3\"
      },
      \"href\" : \"https://api.example.com/v1/artists/d9d08ae52b6be40daee1c3\",
      \"id\" : \"d9d08ae52b6be40daee1c3\",
      \"name\" : \"Anonymous\",
      \"type\" : \"artist\",
      \"uri\" : \"spotify:artist:d9d08ae52b6be40daee1c3\"
    } ],
    \"available_markets\" : [
      \"AD\", \"AE\", \"AG\", \"AL\", \"AM\", \"AO\", \"AR\", \"AT\", \"AU\", \"AZ\",
      \"BA\", \"BB\", \"BD\", \"BE\", \"BF\", \"BG\", \"BH\", \"BI\", \"BJ\", \"BN\",
      \"BO\", \"BR\", \"BS\", \"BT\", \"BW\", \"BY\", \"BZ\", \"CA\", \"CD\", \"CG\",
      \"CH\", \"CI\", \"CL\", \"CM\", \"CO\", \"CR\", \"CV\", \"CW\", \"CY\", \"CZ\",
      \"DE\", \"DJ\", \"DK\", \"DM\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\",
      \"FI\", \"FJ\", \"FM\", \"FR\", \"GA\", \"GB\", \"GD\", \"GE\", \"GH\", \"GM\",
      \"GN\", \"GQ\", \"GR\", \"GT\", \"GW\", \"GY\", \"HK\", \"HN\", \"HR\", \"HT\",
      \"HU\", \"ID\", \"IE\", \"IL\", \"IN\", \"IQ\", \"IS\", \"IT\", \"JM\", \"JO\",
      \"JP\", \"KE\", \"KG\", \"KH\", \"KI\", \"KM\", \"KN\", \"KR\", \"KW\", \"KZ\",
      \"LA\", \"LB\", \"LC\", \"LI\", \"LK\", \"LR\", \"LS\", \"LT\", \"LU\", \"LV\",
      \"LY\", \"MA\", \"MC\", \"MD\", \"ME\", \"MG\", \"MH\", \"MK\", \"ML\", \"MN\",
      \"MO\", \"MR\", \"MT\", \"MU\", \"MV\", \"MW\", \"MX\", \"MY\", \"MZ\", \"NA\",
      \"NE\", \"NG\", \"NI\", \"NL\", \"NO\", \"NP\", \"NR\", \"NZ\", \"OM\", \"PA\",
      \"PE\", \"PG\", \"PH\", \"PK\", \"PL\", \"PS\", \"PT\", \"PW\", \"PY\", \"QA\",
      \"RO\", \"RS\", \"RU\", \"RW\", \"SA\", \"SB\", \"SC\", \"SE\", \"SG\", \"SI\",
      \"SK\", \"SL\", \"SM\", \"SN\", \"SR\", \"ST\", \"SV\", \"SZ\", \"TD\", \"TG\",
      \"TH\", \"TJ\", \"TL\", \"TN\", \"TO\", \"TR\", \"TT\", \"TV\", \"TW\", \"TZ\",
      \"UA\", \"UG\", \"US\", \"UY\", \"UZ\", \"VC\", \"VE\", \"VN\", \"VU\", \"WS\",
      \"XK\", \"ZA\", \"ZM\", \"ZW\"
    ],
    \"disc_number\" : 1,
    \"duration_ms\" : 244066,
    \"explicit\" : false,
    \"external_ids\" : {
      \"isrc\" : \"USAT93209096\"
    },
    \"external_urls\" : {
      \"spotify\" : \"https://open.example.com/track/8962e1ac6fe8a1600f464a\"
    },
    \"href\" : \"https://api.example.com/v1/tracks/8962e1ac6fe8a1600f464a\",
    \"id\" : \"8962e1ac6fe8a1600f464a\",
    \"is_local\" : false,
    \"name\" : \"Anonymous Taste of Anonymity\",
    \"popularity\" : 52,
    \"preview_url\" : \"https://p.scdn.co/mp3-preview/84561e89f43c93197937db449a08f46dc64346c6?cid=d1fa654068d2355d0d4d758d14357e89\",
    \"track_number\" : 5,
    \"type\" : \"track\",
    \"uri\" : \"spotify:track:8962e1ac6fe8a1600f464a\"
  },
  \"currently_playing_type\" : \"track\",
  \"actions\" : {
    \"disallows\" : {
      \"resuming\" : true
    }
  },
  \"is_playing\" : true
}"
  "Spotify API JSON string with fields anonymized/randomized.
From `smudge-api-call-async'.")


;; The reduced version of `test-smudge-cache--json-str-full' as it would be from
;; `smudge-connect-player-status'.
(defconst test-smudge-cache--json-str-reduced
  "{
  \"artist\": \"Anonymous\",
  \"duration\": 244066,
  \"track_number\": 5,
  \"name\": \"Anonymous Taste of Anonymity\",
  \"player_state\": \"playing\",
  \"player_shuffling\": false,
  \"player_repeating\": false
}"
  "Smudge JSON string with fields anonymized/randomized.

From `smudge-connect-player-status'.")


;;------------------------------------------------------------------------------
;; Helpers: Test Data
;;------------------------------------------------------------------------------

(defun test-smudge-cache--json-hash-table (json)
  "Convert JSON string to JSON hash table."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read-from-string json)))

(defun test-smudge-cache--json-reduced ()
  "Parse `test-smudge-cache--json-str-reduced' JSON string into JSON hash-table."
  (test-smudge-cache--json-hash-table test-smudge-cache--json-str-reduced))

(defun test-smudge-cache--json-full ()
  "Parse `test-smudge-cache--json-str-full' JSON string into JSON hash-table."
  (test-smudge-cache--json-hash-table test-smudge-cache--json-str-full))

(defvar test-smudge-cache--device-name "test-smudge-cache-device"
  "Device Name from `test-smudge-cache--json-str-full'.")

(defvar test-smudge-cache--device-id "b9e016fbc8772cdd96dea6f0dc89b556238444c3"
  "Device ID from `test-smudge-cache--json-str-full'.")


;;------------------------------------------------------------------------------
;; Initialization / Set-Up
;;------------------------------------------------------------------------------

(defun test-smudge-cache--init-device-cache ()
  "Create entry in device cache for the test device."
  (smudge-cache--device-set test-smudge-cache--device-name
                            test-smudge-cache--device-id))

(defun test-smudge-cache--init-json-cache ()
  "Create entry in status cache for the test status JSON."
  (smudge-cache--set test-smudge-cache--device-id
                     :status (test-smudge-cache--json-full)))

(defun test-smudge-cache--init ()
  "Create entries in the caches from the test data."
  (test-smudge-cache--init-device-cache)
  (test-smudge-cache--init-json-cache))
;; (test-smudge-cache--init)


;;------------------------------------------------------------------------------
;; Helpers: Cached Data
;;------------------------------------------------------------------------------

(defun test-smudge-cache--force-volume (device-id volume)
  "Set DEVICE-ID's :volume and :status->\"device\"->\"volume_percent\" to VOLUME."
  (when-let ((full-cache (smudge-cache--get-device-cache device-id)))
    (when-let* ((status-cache (gethash :status full-cache))
                (device-cache (gethash 'device status-cache)))
      (puthash 'volume volume device-cache))

    (when (gethash :volume full-cache)
      (puthash :volume volume full-cache))))


;;------------------------------------------------------------------------------
;; Helpers: Misc
;;------------------------------------------------------------------------------

(defun test-smudge-cache--float= (x y &optional precision)
  "Test that floats X and Y are equal enough for float maths.

PRECISION should be a positive integer of the significant digits of X and Y.
A good default is 6, but for our tests and `float-time' we'll use 13."
  (let ((fuzz-factor (expt 10 (- (if (integerp precision)
                                     precision
                                   13)))))
    (or (= x y)
        (< (/ (abs (- x y))
              (max (abs x) (abs y)))
           fuzz-factor))))
;; (test-smudge-cache--float= 1645809977.6945927 1645809978.6945927)


(defun test-smudge-cache--float-within (x y &optional tolerance)
  "Test that the absolute value of the difference between X and Y is in TOLERANCE.

TOLERANCE should be a positive number. Defaults to 1.0."
  (let ((tolerance (if (numberp tolerance)
                       tolerance
                     1.0)))
    (or (= x y)
        (< (abs (- x y)) tolerance))))
;; (test-smudge-cache--float-within 5000.0 5000.4738)
;; (test-smudge-cache--float-within 5000.0 5040.4738)


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠═════════════════════════╤═══╧═══════════╧════╤════════════════════════════╣
;; ╟─────────────────────────┤ Smudge Cache Tests ├────────────────────────────╢
;; ╚═════════════════════════╧════════════════════╧════════════════════════════╝

;;------------------------------------------------------------------------------
;; Tests: Time
;;------------------------------------------------------------------------------

;;------------------------------
;; smudge-cache--time-keyword
;;------------------------------

(ert-deftest test-smudge-cache--time-keyword ()
  "Test that `smudge-cache--time-keyword' creates the correct timestamp keywords."
  (should (eq :timestamp:volume
              (smudge-cache--time-keyword :volume)))
  (should (eq :timestamp:status
              (smudge-cache--time-keyword :status)))
  (should (eq :timestamp:test
              (smudge-cache--time-keyword :test))))


;;------------------------------------------------------------------------------
;; Tests: Smudge Data Cache accessors
;;------------------------------------------------------------------------------

;;------------------------------
;; smudge-cache--get-data
;;------------------------------

(ert-deftest test-smudge-cache--get-data ()
  "Test that `smudge-cache--get-data' puts a key/value pair, and also its
timestamp pair, into a device's cache correctly."
  ;; Create the cache with a device and some data.
  (let* ((timestamp-key   (smudge-cache--time-keyword :volume))
         (timestamp-value (smudge-cache--current-timestamp))
         (data (list (cons :volume       42)
                     (cons timestamp-key timestamp-value)))
         (smudge-cache--data (list (cons test-smudge-cache--device-id
                                         data)))
         ;; This is what we're testing
         (device-data (smudge-cache--get-data test-smudge-cache--device-id)))

    ;; Should have something.
    (should device-data)
    (should (listp device-data))

    ;; And it should be an alist with the proper values.
    (let ((timestamp (alist-get timestamp-key device-data))
          (volume    (alist-get :volume device-data)))
      (should (test-smudge-cache--float= timestamp-value
                                         timestamp))
      (should (eq 42
                  volume)))))


;;------------------------------
;; smudge-cache--set-values
;;------------------------------

(ert-deftest test-smudge-cache--set-values ()
  "Test that `smudge-cache--set-values' puts a key/value pair, and also its
timestamp pair, into a device's cache correctly."
  ;; Create the cache with a device and some data.
  (let* ((timestamp-value        (smudge-cache--current-timestamp))
         device-data)

    (should-not device-data)

    ;;---
    ;; Set values.
    ;;---
    (let ((key   :volume)
          (value 42))
      (setq device-data (smudge-cache--set-values key
                                                  value
                                                  timestamp-value
                                                  device-data))
      ;; Check returned alist is correct.
      (should device-data)
      (should (listp device-data))
      (let ((timestamp    (alist-get (smudge-cache--time-keyword key) device-data))
            (actual-value (alist-get key device-data)))
        (should timestamp)
        (should actual-value)
        (should (test-smudge-cache--float= timestamp-value
                                           timestamp))
        (should (eq 42
                    actual-value))))

    ;;---
    ;; Set other values into existing cache.
    ;;---
    (let ((key   :test-key)
          (value :test-value))
      (setq device-data (smudge-cache--set-values key
                                                  value
                                                  timestamp-value
                                                  device-data))
      ;; Check returned alist is correct.
      (should device-data)
      (should (listp device-data))
      ;; Should still have volume from before.
      (should (test-smudge-cache--float= timestamp-value
                                         (alist-get (smudge-cache--time-keyword :volume)
                                                    device-data)))
      (should (eq 42
                  (alist-get :volume device-data)))
      ;; And we should have the new key/value.
      (let ((timestamp    (alist-get (smudge-cache--time-keyword key) device-data))
            (actual-value (alist-get key device-data)))
        (should timestamp)
        (should actual-value)
        (should (test-smudge-cache--float= timestamp-value
                                           timestamp))
        (should (eq value actual-value))))

    ;;---
    ;; Update values in cache.
    ;;---
    (let ((key   :test-key)
          (value '(hello "there"))
          (new-timestamp-value (1+ timestamp-value)))
      (setq device-data (smudge-cache--set-values key
                                                  value
                                                  new-timestamp-value
                                                  device-data))
      ;; Check returned alist is correct.
      (should device-data)
      (should (listp device-data))
      ;; Should still have volume from before.
      (should (test-smudge-cache--float= timestamp-value
                                         (alist-get (smudge-cache--time-keyword :volume)
                                                    device-data)))
      (should (eq 42
                  (alist-get :volume device-data)))
      ;; And we should have the key updated to the new value & timestamp.
      (let ((timestamp    (alist-get (smudge-cache--time-keyword key) device-data))
            (actual-value (alist-get key device-data)))
        (should timestamp)
        (should actual-value)
        ;; Timestamp should be updated.
        (should-not (test-smudge-cache--float= timestamp-value
                                               timestamp))
        (should (test-smudge-cache--float= new-timestamp-value
                                           timestamp))
        ;; Value should be updated.
        (should (listp actual-value))
        (should (eq 'hello (nth 0 actual-value)))
        (should (string= "there" (nth 1 actual-value)))))))


;;------------------------------
;; smudge-cache--get
;;------------------------------
(ert-deftest test-smudge-cache--get ()
  "Test that `smudge-cache--get' puts a key/value pair, and also its
timestamp pair, into a device's cache correctly."
  ;;------------------------------
  ;; Create the cache with some data.
  ;;------------------------------
  (let* ((expected-timestamp (smudge-cache--current-timestamp))
         (expected-volume    42)
         (expected-test-data '(hello "there"))
         ;; Start with null cache.
         smudge-cache--data
         device-data)
    ;;---
    ;; Create Device Cache's data.
    ;;---
    (setq device-data (smudge-cache--set-values :volume
                                                expected-volume
                                                expected-timestamp
                                                device-data))
    (setq device-data (smudge-cache--set-values :test-key
                                                expected-test-data
                                                expected-timestamp
                                                device-data))
    ;;---
    ;; Set Device Cache into `smudge-cache--data'.
    ;;---
    (should-not smudge-cache--data)
    (setq smudge-cache--data (list (cons test-smudge-cache--device-id
                                         device-data)))

    ;;------------------------------
    ;; Test getting data from cache.
    ;;------------------------------
    (let* ((key       :volume)
           (timestamp (smudge-cache--get test-smudge-cache--device-id
                                         (smudge-cache--time-keyword key)))
           (volume    (smudge-cache--get test-smudge-cache--device-id
                                         key)))
        (should timestamp)
        (should volume)
        (should (test-smudge-cache--float= expected-timestamp
                                           timestamp))
        (should (eq expected-volume
                    volume)))

    (let* ((key       :test-key)
           (timestamp (smudge-cache--get test-smudge-cache--device-id
                                         (smudge-cache--time-keyword key)))
           (test-data (smudge-cache--get test-smudge-cache--device-id
                                         key)))
      (should timestamp)
      (should test-data)
      (should (test-smudge-cache--float= expected-timestamp
                                         timestamp))
      (should (listp test-data))
      (should (symbolp (nth 0 test-data)))
      (should (eq (nth 0 expected-test-data)
                  (nth 0 test-data)))
      (should (stringp (nth 1 test-data)))
      (should (string= (nth 1 expected-test-data)
                       (nth 1 test-data))))))


;;------------------------------
;; smudge-cache--set
;;------------------------------
(ert-deftest test-smudge-cache--set ()
  "Test that `smudge-cache--set' puts a key/value pair, and also its
timestamp pair, into a device's cache correctly."
  ;;------------------------------
  ;; Create the cache with some data.
  ;;------------------------------
  (let* ((expected-volume 42)
         (expected-status (test-smudge-cache--json-full))
         (approx-timestamp (smudge-cache--current-timestamp))
         ;; Start with null cache.
         smudge-cache--data)

    ;;------------------------------
    ;; Test: Create Device Cache's data.
    ;;------------------------------
    (should (smudge-cache--set test-smudge-cache--device-id
                               :volume expected-volume
                               :status expected-status))
    (should smudge-cache--data)

    ;;------------------------------
    ;; Verify: Get values to ensure they were set correctly.
    ;;------------------------------
    (let* ((key       :volume)
           (timestamp (smudge-cache--get test-smudge-cache--device-id
                                         (smudge-cache--time-keyword key)))
           (volume    (smudge-cache--get test-smudge-cache--device-id
                                         key)))
      (should timestamp)
      (should (floatp timestamp))
      (should volume)
      ;; Our approximate timestamp should be within a second of the `smudge-cache--set' timestamp.
      (should (test-smudge-cache--float-within approx-timestamp
                                               timestamp))
      (should (eq expected-volume
                  volume)))

    (let* ((key       :status)
           (timestamp (smudge-cache--get test-smudge-cache--device-id
                                         (smudge-cache--time-keyword key)))
           (status    (smudge-cache--get test-smudge-cache--device-id
                                         key)))
      (should timestamp)
      (should (floatp timestamp))
      (should status)
      ;; Our approximate timestamp should be within a second of the `smudge-cache--set' timestamp.
      (should (test-smudge-cache--float-within approx-timestamp
                                               timestamp))
      ;; Expect to get back the same exact hash table.
      (should (eq expected-status
                  status)))))


;;------------------------------------------------------------------------------
;; Tests: Smudge Device Cache accessors
;;------------------------------------------------------------------------------

;;------------------------------
;; smudge-cache--device-get
;;------------------------------
(ert-deftest test-smudge-cache--device-get ()
  "Test that `smudge-cache--device-get' can get a device-id from a device-name."

  ;; ID and Name are both strings.
  (should (stringp test-smudge-cache--device-id))
  (should (stringp test-smudge-cache--device-name))

  ;;------------------------------
  ;; Create the cache with some data.
  ;;------------------------------
  (let* ((expected-device-id   "expected-device-id")
         (expected-device-name "expected-device-name")
         (smudge-cache--device (list (cons expected-device-name
                                           expected-device-id)
                                     (cons test-smudge-cache--device-name
                                           test-smudge-cache--device-id))))

    ;;------------------------------
    ;; Test getting from cache.
    ;;------------------------------
    (let ((device-id (smudge-cache--device-get expected-device-name)))
      (should (stringp device-id))
      (should (string= expected-device-id device-id)))

    (let ((device-id (smudge-cache--device-get test-smudge-cache--device-name)))
      (should (stringp device-id))
      (should (string= test-smudge-cache--device-id device-id)))))

;;------------------------------
;; TODO: smudge-cache--device-set
;;------------------------------


;;------------------------------------------------------------------------------
;; Tests: Smudge Device Name/ID helpers
;;------------------------------------------------------------------------------

;;------------------------------
;; TODO: smudge-cache--device-name-from-status
;;------------------------------
;; (smudge-cache--device-name-from-status (smudge-cache-test--json-full))

;;------------------------------
;; TODO: smudge-cache--device-id-from-status
;;------------------------------
;; (smudge-cache--device-id-from-status (smudge-cache-test--json-full))

;;------------------------------
;; TODO: smudge-cache--device-id-from-type
;;------------------------------
;; (smudge-cache--device-id-from-type :id smudge-cache-test--device-id)
;; (smudge-cache--device-id-from-type :name smudge-cache-test--device-name)


;;------------------------------------------------------------------------------
;; Tests: Smudge Cache API Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; TODO: smudge-cache-update-status
;;------------------------------
;; (setq smudge-cache--data nil)
;; smudge-cache--data
;; (smudge-cache-update-status (smudge-cache-test--json-full) (lambda () (message "Hello there.")))
;; (length smudge-cache--data)
;; smudge-cache--data

;;------------------------------
;; TODO: smudge-cache-get-status
;;------------------------------
;; (smudge-cache--device-id-from-type :id smudge-cache-test--device-id)
;; (smudge-cache-get-status :id smudge-cache-test--device-id)
;; (smudge-cache-get-status :name smudge-cache-test--device-name)

;;------------------------------
;; TODO: smudge-cache-update-volume
;;------------------------------
;; (smudge-cache--device-id-from-type :id smudge-cache-test--device-id)
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)
;; (smudge-cache-update-volume smudge-cache-test--device-id 55 (message "volume says hello"))

;;------------------------------
;; TODO: smudge-cache-get-volume
;;------------------------------
;; (smudge-cache--device-id-from-type :id smudge-cache-test--device-id)
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)

;;------------------------------
;; TODO: smudge-cache-is-muted
;;------------------------------
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)
;; (smudge-cache-is-muted :id smudge-cache-test--device-id)
;; (smudge-cache-test--force-volume smudge-cache-test--device-id 99)
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)
;; (smudge-cache-is-muted :id smudge-cache-test--device-id)
;; (smudge-cache-test--force-volume smudge-cache-test--device-id 0)
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)
;; (smudge-cache-is-muted :id smudge-cache-test--device-id)


(provide 'test-smudge-cache)
;;; test-smudge-cache.el ends here
