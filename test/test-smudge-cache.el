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


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠═════════════════════════╤═══╧═══════════╧════╤════════════════════════════╣
;; ╟─────────────────────────┤ Smudge Cache Tests ├────────────────────────────╢
;; ╚═════════════════════════╧════════════════════╧════════════════════════════╝

;;------------------------------------------------------------------------------
;; Tests: Feature Helpers
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

;; TODO: more tests

(provide 'test-smudge-cache)
;;; test-smudge-cache.el ends here
