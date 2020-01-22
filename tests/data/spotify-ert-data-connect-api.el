;;; spotify-ert-data-common.el --- Test data for spotify.el unit tests. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit test data for spotify.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                          Tasty, tasty data...                            --
;;-------------------(run all tests if you change the data)---------------------

;; (require ...)


;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;-----------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Player Status (Full)
;;------------------------------------------------------------------------------

(defconst spotify-ert/data/connect-api/player-status
  "{
    \"device\" : {
      \"id\" : \"test-device-id\",
      \"is_active\" : true,
      \"is_private_session\" : false,
      \"is_restricted\" : false,
      \"name\" : \"Emacs AR Glasses 5001+ Pro#\",
      \"type\" : \"Computer\",
      \"volume_percent\" : 42
    },
    \"shuffle_state\" : false,
    \"repeat_state\" : \"off\",
    \"timestamp\" : 3,
    \"context\" : {
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
      },
      \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
      \"type\" : \"album\",
      \"uri\" : \"spotify:album:1234567890\"
    },
    \"progress_ms\" : 15611,
    \"item\" : {
      \"album\" : {
        \"album_type\" : \"album\",
        \"artists\" : [ {
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
          },
          \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
          \"id\" : \"1234567890\",
          \"name\" : \"\\\"Weird Al\\\" Yankovic\",
          \"type\" : \"artist\",
          \"uri\" : \"spotify:artist:1234567890\"
        } ],
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
        },
        \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
        \"id\" : \"1234567890\",
        \"images\" : [ {
          \"height\" : 640,
          \"url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
          \"width\" : 640
        }, {
          \"height\" : 300,
          \"url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
          \"width\" : 300
        }, {
          \"height\" : 64,
          \"url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
          \"width\" : 64
        } ],
        \"name\" : \"Mandatory Fun\",
        \"release_date\" : \"2014-07-15\",
        \"release_date_precision\" : \"day\",
        \"total_tracks\" : 12,
        \"type\" : \"album\",
        \"uri\" : \"spotify:album:1234567890\"
      },
      \"artists\" : [ {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
        },
        \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
        \"id\" : \"0123456789\",
        \"name\" : \"\\\"Weird Al\\\" Yankovic\",
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:1234567890\"
      } ],
      \"disc_number\" : 1,
      \"duration_ms\" : 142946,
      \"explicit\" : false,
      \"external_ids\" : {
        \"isrc\" : \"USRC11401404\"
      },
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
      },
      \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
      \"id\" : \"test-track-id-0\",
      \"is_local\" : false,
      \"is_playable\" : true,
      \"name\" : \"Foil\",
      \"popularity\" : 49,
      \"preview_url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
      \"track_number\" : 3,
      \"type\" : \"track\",
      \"uri\" : \"spotify:track:1234567890\"
    },
    \"currently_playing_type\" : \"track\",
    \"actions\" : {
      \"disallows\" : {
        \"resuming\" : true
      }
    },
    \"is_playing\" : true
   }"
  "A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/me/player' endpoint.

https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/
")


;;------------------------------------------------------------------------------
;; Player Status (Truncated)
;;------------------------------------------------------------------------------

(defconst spotify-ert/data/connect-api/player-status/truncated
  "{
    \"timestamp\": 1490252122574,
    \"device\": {
      \"id\": \"test-device-id\",
      \"is_active\": false,
      \"is_restricted\": false,
      \"name\": \"Emacs Phone 3000\",
      \"type\": \"Smartphone\",
      \"volume_percent\": 54
    },
    \"progress_ms\": \"44272\",
    \"is_playing\": true,
    \"currently_playing_type\": \"track\",
    \"actions\": {
      \"disallows\": {
        \"resuming\": true
      }
    },
    \"item\": {},
    \"shuffle_state\": false,
    \"repeat_state\": \"off\",
    \"context\": {
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/49znshcYJROspEqBoHg3Sv\"
      },
      \"href\" : \"https://api.spotify.com/v1/users/spotify/playlists/49znshcYJROspEqBoHg3Sv\",
      \"type\" : \"playlist\",
      \"uri\" : \"spotify:user:spotify:playlist:49znshcYJROspEqBoHg3Sv\"
    }
  }"
  "A sample return value from Spotify Connect API '/v1/me/player'
endpoint. Sample is very minimal; see
`spotify-api-json-ert/data/player-status-in-full' for much
longer, fuller one.

https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/
")


;;------------------------------------------------------------------------------
;; Device List with One Active
;;------------------------------------------------------------------------------

(defconst spotify-ert/data/connect-api/devices-list/active
  "{
      \"devices\": [
         {
            \"id\": \"test-device-id-0\",
            \"is_active\": true,
            \"is_private_session\": false,
            \"is_restricted\": false,
            \"name\": \"Your MacBook\",
            \"type\": \"Computer\",
            \"volume_percent\": 70
         },
         {
            \"id\": \"test-device-id-1\",
            \"is_active\": false,
            \"is_private_session\": false,
            \"is_restricted\": false,
            \"name\": \"Living Room\",
            \"type\": \"TV\",
            \"volume_percent\": 25
         },
         {
            \"id\": \"test-device-id-2\",
            \"is_active\": false,
            \"is_private_session\": false,
            \"is_restricted\": false,
            \"name\": \"Office Speaker\",
            \"type\": \"Unknown\",
            \"volume_percent\": 82
         }
      ]
   }"
  "A sample return value from Spotify Connect API
'/v1/me/player/devices' endpoint.

https://developer.spotify.com/documentation/web-api/guides/using-connect-web-api/
")


;;------------------------------------------------------------------------------
;; Device List with Zero Active
;;------------------------------------------------------------------------------

(defconst spotify-ert/data/connect-api/devices-list/inactive
  "{
      \"devices\": [
         {
            \"id\": \"test-device-id-0\",
            \"is_active\": false,
            \"is_private_session\": false,
            \"is_restricted\": false,
            \"name\": \"Your MacBook\",
            \"type\": \"Computer\",
            \"volume_percent\": 70
         },
         {
            \"id\": \"test-device-id-1\",
            \"is_active\": false,
            \"is_private_session\": false,
            \"is_restricted\": false,
            \"name\": \"Living Room\",
            \"type\": \"TV\",
            \"volume_percent\": 25
         },
         {
            \"id\": \"test-device-id-2\",
            \"is_active\": false,
            \"is_private_session\": false,
            \"is_restricted\": false,
            \"name\": \"Office Speaker\",
            \"type\": \"Unknown\",
            \"volume_percent\": 82
         }
      ]
   }"
  "A sample return value from Spotify Connect API
'/v1/me/player/devices' endpoint.

https://developer.spotify.com/documentation/web-api/guides/using-connect-web-api/
")


;;------------------------------------------------------------------------------
;; Artist Object
;;------------------------------------------------------------------------------

(defconst spotify-ert/data/connect-api/artist
  "{
     \"external_urls\" : {
       \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
     },
     \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
     \"id\" : \"1234567890\",
     \"name\" : \"\\\"Weird Al\\\" Yankovic\",
     \"type\" : \"artist\",
     \"uri\" : \"spotify:artist:1234567890\"
   }"
"A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/me/player' endpoint, cut down to just an artist object.

https://developer.spotify.com/documentation/web-api/reference/object-model/#artist-object-simplified
")


;;------------------------------------------------------------------------------
;; Track Object
;;------------------------------------------------------------------------------

(defconst spotify-ert/data/connect-api/track
  "{
     \"album\" : {
       \"album_type\" : \"album\",
       \"artists\" : [ {
         \"external_urls\" : {
           \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
         },
         \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
         \"id\" : \"1234567890\",
         \"name\" : \"\\\"Weird Al\\\" Yankovic\",
         \"type\" : \"artist\",
         \"uri\" : \"spotify:artist:1234567890\"
       } ],
       \"external_urls\" : {
         \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
       },
       \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
       \"id\" : \"1234567890\",
       \"images\" : [ {
         \"height\" : 640,
         \"url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
         \"width\" : 640
       }, {
         \"height\" : 300,
         \"url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
         \"width\" : 300
       }, {
         \"height\" : 64,
         \"url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
         \"width\" : 64
       } ],
       \"name\" : \"Mandatory Fun\",
       \"release_date\" : \"2014-07-15\",
       \"release_date_precision\" : \"day\",
       \"total_tracks\" : 12,
       \"type\" : \"album\",
       \"uri\" : \"spotify:album:1234567890\"
     },
     \"artists\" : [ {
       \"external_urls\" : {
         \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
       },
       \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
       \"id\" : \"0123456789\",
       \"name\" : \"\\\"Weird Al\\\" Yankovic\",
       \"type\" : \"artist\",
       \"uri\" : \"spotify:artist:1234567890\"
     } ],
     \"disc_number\" : 1,
     \"duration_ms\" : 142946,
     \"explicit\" : false,
     \"external_ids\" : {
       \"isrc\" : \"USRC11401404\"
     },
     \"external_urls\" : {
       \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
     },
     \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
     \"id\" : \"test-track-id-0\",
     \"is_local\" : false,
     \"is_playable\" : true,
     \"name\" : \"Foil\",
     \"popularity\" : 49,
     \"preview_url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
     \"track_number\" : 3,
     \"type\" : \"track\",
     \"uri\" : \"spotify:track:test-track-uri-0\"
   }"
"A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/me/player' endpoint, cut down to just a track object.

https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full
")


;;------------------------------------------------------------------------------
;; Device Object
;;------------------------------------------------------------------------------

(defconst spotify-ert/data/connect-api/device
  "{
     \"id\" : \"test-device-id-only\",
     \"is_active\" : true,
     \"is_private_session\" : false,
     \"is_restricted\" : false,
     \"name\" : \"Emacs AR Glasses 5001+ Pro#\",
     \"type\" : \"Computer\",
     \"volume_percent\" : 42
   }"
"A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/me/player' endpoint, cut down to just a device object.

https://developer.spotify.com/documentation/web-api/reference/player/get-a-users-available-devices/
")


;;------------------------------------------------------------------------------
;; Paginated Search Result
;;------------------------------------------------------------------------------

(defconst spotify-ert/data/connect-api/search-artists
  "{
    \"artists\" : {
      \"href\" : \"https://api.spotify.com/v1/search?query=shine&type=artist&offset=0&limit=5\",
      \"items\" : [ {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/artist/1234567890\"
        },
        \"followers\" : {
          \"href\" : null,
          \"total\" : 7308
        },
        \"genres\" : [ \"heartland rock\", \"red dirt\", \"texas country\" ],
        \"href\" : \"https://api.spotify.com/v1/artists/1234567890\",
        \"id\" : \"test-artist-id-0\",
        \"images\" : [ {
          \"height\" : 1000,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 1000
        }, {
          \"height\" : 640,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 640
        }, {
          \"height\" : 200,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 200
        }, {
          \"height\" : 64,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 64
        } ],
        \"name\" : \"Dolly Shine\",
        \"popularity\" : 41,
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:1111111\"
      }, {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/artist/1234567890\"
        },
        \"followers\" : {
          \"href\" : null,
          \"total\" : 11699
        },
        \"genres\" : [ \"country rap\", \"redneck\" ],
        \"href\" : \"https://api.spotify.com/v1/artists/1234567890\",
        \"id\" : \"test-artist-id-1\",
        \"images\" : [ {
          \"height\" : 640,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 640
        }, {
          \"height\" : 300,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 300
        }, {
          \"height\" : 64,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 64
        } ],
        \"name\" : \"Tennessee Shine\",
        \"popularity\" : 39,
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:1111111\"
      }, {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/artist/1234567890\"
        },
        \"followers\" : {
          \"href\" : null,
          \"total\" : 308
        },
        \"genres\" : [ ],
        \"href\" : \"https://api.spotify.com/v1/artists/1234567890\",
        \"id\" : \"test-artist-id-2\",
        \"images\" : [ {
          \"height\" : 640,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 640
        }, {
          \"height\" : 320,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 320
        }, {
          \"height\" : 160,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 160
        } ],
        \"name\" : \"Rain or Shine\",
        \"popularity\" : 37,
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:1111111\"
      }, {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/artist/1234567890\"
        },
        \"followers\" : {
          \"href\" : null,
          \"total\" : 7123
        },
        \"genres\" : [ \"dirty south rap\", \"gangster rap\", \"pop rap\", \"rap\", \"southern hip hop\", \"trap\" ],
        \"href\" : \"https://api.spotify.com/v1/artists/1234567890\",
        \"id\" : \"test-artist-id-3\",
        \"images\" : [ {
          \"height\" : 640,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 640
        }, {
          \"height\" : 320,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 320
        }, {
          \"height\" : 160,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 160
        } ],
        \"name\" : \"Kia Shine\",
        \"popularity\" : 28,
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:1111111\"
      }, {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/artist/1234567890\"
        },
        \"followers\" : {
          \"href\" : null,
          \"total\" : 869
        },
        \"genres\" : [ \"battle rap\" ],
        \"href\" : \"https://api.spotify.com/v1/artists/1234567890\",
        \"id\" : \"test-artist-id-4\",
        \"images\" : [ {
          \"height\" : 640,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 640
        }, {
          \"height\" : 300,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 300
        }, {
          \"height\" : 64,
          \"url\" : \"https://i.scdn.co/image/1234567890\",
          \"width\" : 64
        } ],
        \"name\" : \"K-Shine\",
        \"popularity\" : 26,
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:1111111\"
      } ],
      \"limit\" : 5,
      \"next\" : \"https://api.spotify.com/v1/search?query=shine&type=artist&offset=5&limit=5\",
      \"offset\" : 0,
      \"previous\" : null,
      \"total\" : 681
    }
  }"

"A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/search' endpoint.

https://developer.spotify.com/documentation/web-api/reference/search/search/#response-format

Obtained via:  curl -X GET 'https://api.spotify.com/v1/search?q=shine&type=track' -H 'Authorization: Bearer <insert *spotify-oauth2-token* here>' > search-artists.txt
")


(defconst spotify-ert/data/connect-api/search-tracks
  "{
    \"tracks\" : {
      \"href\" : \"https://api.spotify.com/v1/search?query=shinedown&type=track&offset=0&limit=5\",
      \"items\" : [ {
        \"album\" : {
          \"album_type\" : \"album\",
          \"artists\" : [ {
            \"external_urls\" : {
              \"spotify\" : \"https://open.spotify.com/artist/11112345\"
            },
            \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
            \"id\" : \"test-id-0\",
            \"name\" : \"Shinedown\",
            \"type\" : \"artist\",
            \"uri\" : \"spotify:artist:6665577666\"
          } ],
          \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HN\", \"HU\", \"IE\", \"IL\", \"IS\", \"IT\", \"JO\", \"KW\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"NI\", \"NL\", \"NO\", \"OM\", \"PA\", \"PE\", \"PL\", \"PT\", \"PY\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"SV\", \"TN\", \"TR\", \"US\", \"UY\", \"ZA\" ],
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/album/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/albums/0123456789\",
          \"id\" : \"test-id-1\",
          \"images\" : [ {
            \"height\" : 640,
            \"url\" : \"https://i.scdn.co/image/test-url-0\",
            \"width\" : 640
          }, {
            \"height\" : 300,
            \"url\" : \"https://i.scdn.co/image/test-url-1\",
            \"width\" : 300
          }, {
            \"height\" : 64,
            \"url\" : \"https://i.scdn.co/image/test-url-2\",
            \"width\" : 64
          } ],
          \"name\" : \"Leave a Whisper (Deluxe Edition)\",
          \"release_date\" : \"2003-05-27\",
          \"release_date_precision\" : \"day\",
          \"total_tracks\" : 25,
          \"type\" : \"album\",
          \"uri\" : \"spotify:album:6665577666\"
        },
        \"artists\" : [ {
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/artist/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
          \"id\" : \"test-id-2\",
          \"name\" : \"Shinedown\",
          \"type\" : \"artist\",
          \"uri\" : \"spotify:artist:6665577666\"
        } ],
        \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HN\", \"HU\", \"IE\", \"IL\", \"IS\", \"IT\", \"JO\", \"KW\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"NI\", \"NL\", \"NO\", \"OM\", \"PA\", \"PE\", \"PL\", \"PT\", \"PY\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"SV\", \"TN\", \"TR\", \"US\", \"UY\", \"ZA\" ],
        \"disc_number\" : 1,
        \"duration_ms\" : 250093,
        \"explicit\" : false,
        \"external_ids\" : {
          \"isrc\" : \"test-isrc-0\"
        },
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/track/11112345\"
        },
        \"href\" : \"https://api.spotify.com/v1/tracks/0123456789\",
        \"id\" : \"test-id-3\",
        \"is_local\" : false,
        \"name\" : \"45\",
        \"popularity\" : 66,
        \"preview_url\" : \"https://p.scdn.co/mp3-preview/test-preview-0?cid=test-cid-0\",
        \"track_number\" : 12,
        \"type\" : \"track\",
        \"uri\" : \"spotify:track:6665577666\"
      }, {
        \"album\" : {
          \"album_type\" : \"album\",
          \"artists\" : [ {
            \"external_urls\" : {
              \"spotify\" : \"https://open.spotify.com/artist/11112345\"
            },
            \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
            \"id\" : \"test-id-4\",
            \"name\" : \"Shinedown\",
            \"type\" : \"artist\",
            \"uri\" : \"spotify:artist:6665577666\"
          } ],
          \"available_markets\" : [ \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"CA\", \"CH\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DZ\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"HK\", \"HU\", \"ID\", \"IE\", \"test-id-5\", \"IT\", \"JO\", \"JP\", \"KW\", \"LB\", \"LT\", \"LU\", \"LV\", \"MA\", \"MT\", \"MY\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PH\", \"PL\", \"PT\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"VN\", \"ZA\" ],
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/album/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/albums/0123456789\",
          \"id\" : \"test-id-6\",
          \"images\" : [ {
            \"height\" : 640,
            \"url\" : \"https://i.scdn.co/image/test-url-3\",
            \"width\" : 640
          }, {
            \"height\" : 300,
            \"url\" : \"https://i.scdn.co/image/test-url-4\",
            \"width\" : 300
          }, {
            \"height\" : 64,
            \"url\" : \"https://i.scdn.co/image/test-url-5\",
            \"width\" : 64
          } ],
          \"name\" : \"Threat to Survival\",
          \"release_date\" : \"2015-09-18\",
          \"release_date_precision\" : \"day\",
          \"total_tracks\" : 11,
          \"type\" : \"album\",
          \"uri\" : \"spotify:album:6665577666\"
        },
        \"artists\" : [ {
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/artist/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
          \"id\" : \"test-id-7\",
          \"name\" : \"Shinedown\",
          \"type\" : \"artist\",
          \"uri\" : \"spotify:artist:6665577666\"
        } ],
        \"available_markets\" : [ \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"CA\", \"CH\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DZ\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"HK\", \"HU\", \"ID\", \"IE\", \"test-id-8\", \"IT\", \"JO\", \"JP\", \"KW\", \"LB\", \"LT\", \"LU\", \"LV\", \"MA\", \"MT\", \"MY\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PH\", \"PL\", \"PT\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"VN\", \"ZA\" ],
        \"disc_number\" : 1,
        \"duration_ms\" : 224596,
        \"explicit\" : false,
        \"external_ids\" : {
          \"isrc\" : \"test-isrc-1\"
        },
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/track/11112345\"
        },
        \"href\" : \"https://api.spotify.com/v1/tracks/0123456789\",
        \"id\" : \"test-id-9\",
        \"is_local\" : false,
        \"name\" : \"Cut the Cord\",
        \"popularity\" : 67,
        \"preview_url\" : \"https://p.scdn.co/mp3-preview/test-preview-1?cid=test-cid-1\",
        \"track_number\" : 2,
        \"type\" : \"track\",
        \"uri\" : \"spotify:track:6665577666\"
      }, {
        \"album\" : {
          \"album_type\" : \"album\",
          \"artists\" : [ {
            \"external_urls\" : {
              \"spotify\" : \"https://open.spotify.com/artist/11112345\"
            },
            \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
            \"id\" : \"test-id-10\",
            \"name\" : \"Shinedown\",
            \"type\" : \"artist\",
            \"uri\" : \"spotify:artist:6665577666\"
          } ],
          \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IS\", \"IT\", \"JO\", \"JP\", \"test-id-11\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/album/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/albums/0123456789\",
          \"id\" : \"test-id-12\",
          \"images\" : [ {
            \"height\" : 640,
            \"url\" : \"https://i.scdn.co/image/test-url-6\",
            \"width\" : 640
          }, {
            \"height\" : 300,
            \"url\" : \"https://i.scdn.co/image/test-url-7\",
            \"width\" : 300
          }, {
            \"height\" : 64,
            \"url\" : \"https://i.scdn.co/image/test-url-8\",
            \"width\" : 64
          } ],
          \"name\" : \"ATTENTION ATTENTION\",
          \"release_date\" : \"2018-05-04\",
          \"release_date_precision\" : \"day\",
          \"total_tracks\" : 14,
          \"type\" : \"album\",
          \"uri\" : \"spotify:album:6665577666\"
        },
        \"artists\" : [ {
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/artist/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
          \"id\" : \"test-id-13\",
          \"name\" : \"Shinedown\",
          \"type\" : \"artist\",
          \"uri\" : \"spotify:artist:6665577666\"
        } ],
        \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IS\", \"IT\", \"JO\", \"JP\", \"test-id-14\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
        \"disc_number\" : 1,
        \"duration_ms\" : 207986,
        \"explicit\" : false,
        \"external_ids\" : {
          \"isrc\" : \"test-isrc-2\"
        },
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/track/11112345\"
        },
        \"href\" : \"https://api.spotify.com/v1/tracks/0123456789\",
        \"id\" : \"test-id-15\",
        \"is_local\" : false,
        \"name\" : \"DEVIL\",
        \"popularity\" : 68,
        \"preview_url\" : \"https://p.scdn.co/mp3-preview/test-preview-2?cid=test-cid-2\",
        \"track_number\" : 2,
        \"type\" : \"track\",
        \"uri\" : \"spotify:track:6665577666\"
      }, {
        \"album\" : {
          \"album_type\" : \"album\",
          \"artists\" : [ {
            \"external_urls\" : {
              \"spotify\" : \"https://open.spotify.com/artist/11112345\"
            },
            \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
            \"id\" : \"test-id-16\",
            \"name\" : \"Shinedown\",
            \"type\" : \"artist\",
            \"uri\" : \"spotify:artist:6665577666\"
          } ],
          \"available_markets\" : [ \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"CA\", \"CH\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DZ\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"HK\", \"HU\", \"ID\", \"IE\", \"test-id-17\", \"IT\", \"JO\", \"JP\", \"KW\", \"LB\", \"LT\", \"LU\", \"LV\", \"MA\", \"MT\", \"MY\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PH\", \"PL\", \"PT\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"VN\", \"ZA\" ],
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/album/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/albums/0123456789\",
          \"id\" : \"test-id-18\",
          \"images\" : [ {
            \"height\" : 640,
            \"url\" : \"https://i.scdn.co/image/test-url-9\",
            \"width\" : 640
          }, {
            \"height\" : 300,
            \"url\" : \"https://i.scdn.co/image/test-url-10\",
            \"width\" : 300
          }, {
            \"height\" : 64,
            \"url\" : \"https://i.scdn.co/image/test-url-11\",
            \"width\" : 64
          } ],
          \"name\" : \"Threat to Survival\",
          \"release_date\" : \"2015-09-18\",
          \"release_date_precision\" : \"day\",
          \"total_tracks\" : 11,
          \"type\" : \"album\",
          \"uri\" : \"spotify:album:6665577666\"
        },
        \"artists\" : [ {
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/artist/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
          \"id\" : \"test-id-19\",
          \"name\" : \"Shinedown\",
          \"type\" : \"artist\",
          \"uri\" : \"spotify:artist:6665577666\"
        } ],
        \"available_markets\" : [ \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"CA\", \"CH\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DZ\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"HK\", \"HU\", \"ID\", \"IE\", \"test-id-20\", \"IT\", \"JO\", \"JP\", \"KW\", \"LB\", \"LT\", \"LU\", \"LV\", \"MA\", \"MT\", \"MY\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PH\", \"PL\", \"PT\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"VN\", \"ZA\" ],
        \"disc_number\" : 1,
        \"duration_ms\" : 205653,
        \"explicit\" : false,
        \"external_ids\" : {
          \"isrc\" : \"test-isrc-3\"
        },
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/track/11112345\"
        },
        \"href\" : \"https://api.spotify.com/v1/tracks/0123456789\",
        \"id\" : \"test-id-21\",
        \"is_local\" : false,
        \"name\" : \"State of My Head\",
        \"popularity\" : 66,
        \"preview_url\" : \"https://p.scdn.co/mp3-preview/test-preview-3?cid=test-cid-3\",
        \"track_number\" : 3,
        \"type\" : \"track\",
        \"uri\" : \"spotify:track:6665577666\"
      }, {
        \"album\" : {
          \"album_type\" : \"album\",
          \"artists\" : [ {
            \"external_urls\" : {
              \"spotify\" : \"https://open.spotify.com/artist/11112345\"
            },
            \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
            \"id\" : \"test-id-22\",
            \"name\" : \"Shinedown\",
            \"type\" : \"artist\",
            \"uri\" : \"spotify:artist:6665577666\"
          } ],
          \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IS\", \"IT\", \"JO\", \"JP\", \"test-id-23\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/album/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/albums/0123456789\",
          \"id\" : \"test-id-24\",
          \"images\" : [ {
            \"height\" : 640,
            \"url\" : \"https://i.scdn.co/image/test-url-12\",
            \"width\" : 640
          }, {
            \"height\" : 300,
            \"url\" : \"https://i.scdn.co/image/test-url-13\",
            \"width\" : 300
          }, {
            \"height\" : 64,
            \"url\" : \"https://i.scdn.co/image/test-url-14\",
            \"width\" : 64
          } ],
          \"name\" : \"The Sound of Madness\",
          \"release_date\" : \"2008-06-24\",
          \"release_date_precision\" : \"day\",
          \"total_tracks\" : 11,
          \"type\" : \"album\",
          \"uri\" : \"spotify:album:6665577666\"
        },
        \"artists\" : [ {
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/artist/11112345\"
          },
          \"href\" : \"https://api.spotify.com/v1/artists/0123456789\",
          \"id\" : \"test-id-25\",
          \"name\" : \"Shinedown\",
          \"type\" : \"artist\",
          \"uri\" : \"spotify:artist:6665577666\"
        } ],
        \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IS\", \"IT\", \"JO\", \"JP\", \"test-id-26\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
        \"disc_number\" : 1,
        \"duration_ms\" : 222066,
        \"explicit\" : false,
        \"external_ids\" : {
          \"isrc\" : \"test-isrc-4\"
        },
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/track/11112345\"
        },
        \"href\" : \"https://api.spotify.com/v1/tracks/0123456789\",
        \"id\" : \"test-id-27\",
        \"is_local\" : false,
        \"name\" : \"Second Chance\",
        \"popularity\" : 66,
        \"preview_url\" : \"https://p.scdn.co/mp3-preview/test-preview-4?cid=test-cid-4\",
        \"track_number\" : 3,
        \"type\" : \"track\",
        \"uri\" : \"spotify:track:6665577666\"
      } ],
      \"limit\" : 5,
      \"next\" : \"https://api.spotify.com/v1/search?query=shinedown&type=track&offset=5&limit=5\",
      \"offset\" : 0,
      \"previous\" : null,
      \"total\" : 655
    }
  }"

  "A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/search' endpoint.

https://developer.spotify.com/documentation/web-api/reference/search/search/#response-format

Obtained via:  curl -X GET 'https://api.spotify.com/v1/search?query=shinedown&type=track&limit=5' -H 'Authorization: Bearer <insert *spotify-oauth2-token* here>' > search-tracks.txt
")


(defconst spotify-ert/data/connect-api/search-playlists
  "{
    \"playlists\" : {
      \"href\" : \"https://api.spotify.com/v1/search?query=doom+metal&type=playlist&offset=0&limit=5\",
      \"items\" : [ {
        \"collaborative\" : false,
        \"description\" : \"Test playlist description 0.\",
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/playlist/00000000\"
        },
        \"href\" : \"https://api.spotify.com/v1/playlists/0123456789\",
        \"id\" : \"00011011\",
        \"images\" : [ {
          \"height\" : 300,
          \"url\" : \"https://i.scdn.co/image/test-url-0\",
          \"width\" : 300
        } ],
        \"name\" : \"Doom Playlist 0\",
        \"owner\" : {
          \"display_name\" : \"test-user-0\",
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/user/test-user-0\"
          },
          \"href\" : \"https://api.spotify.com/v1/users/test-user-0\",
          \"id\" : \"test-user-0\",
          \"type\" : \"user\",
          \"uri\" : \"spotify:user:test-user-0\"
        },
        \"primary_color\" : null,
        \"public\" : null,
        \"snapshot_id\" : \"00011011\",
        \"tracks\" : {
          \"href\" : \"https://api.spotify.com/v1/playlists/0123456789/tracks\",
          \"total\" : 63
        },
        \"type\" : \"playlist\",
        \"uri\" : \"spotify:playlist:222223323232\"
      }, {
        \"collaborative\" : false,
        \"description\" : \"Test playlist description 1.\",
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/playlist/00000000\"
        },
        \"href\" : \"https://api.spotify.com/v1/playlists/0123456789\",
        \"id\" : \"00011011\",
        \"images\" : [ {
          \"height\" : null,
          \"url\" : \"https://pl.scdn.co/images/pl/default/test-url-1\",
          \"width\" : null
        } ],
        \"name\" : \"Doom Playlist 1\",
        \"owner\" : {
          \"display_name\" : \"test-user-1\",
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/user/test-user-1\"
          },
          \"href\" : \"https://api.spotify.com/v1/users/test-user-1\",
          \"id\" : \"test-user-1\",
          \"type\" : \"user\",
          \"uri\" : \"spotify:user:test-user-1\"
        },
        \"primary_color\" : null,
        \"public\" : null,
        \"snapshot_id\" : \"00011011\",
        \"tracks\" : {
          \"href\" : \"https://api.spotify.com/v1/playlists/0123456789/tracks\",
          \"total\" : 85
        },
        \"type\" : \"playlist\",
        \"uri\" : \"spotify:playlist:222223323232\"
      }, {
        \"collaborative\" : false,
        \"description\" : \"Test playlist description 2.\",
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/playlist/00000000\"
        },
        \"href\" : \"https://api.spotify.com/v1/playlists/0123456789\",
        \"id\" : \"00011011\",
        \"images\" : [ {
          \"height\" : null,
          \"url\" : \"https://pl.scdn.co/images/pl/default/test-url-2\",
          \"width\" : null
        } ],
        \"name\" : \"Doom Playlist 1\",
        \"owner\" : {
          \"display_name\" : \"test-user-1\",
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/user/00000000\"
          },
          \"href\" : \"https://api.spotify.com/v1/users/0123456789\",
          \"id\" : \"00011011\",
          \"type\" : \"user\",
          \"uri\" : \"spotify:user:222223323232\"
        },
        \"primary_color\" : null,
        \"public\" : null,
        \"snapshot_id\" : \"00011011\",
        \"tracks\" : {
          \"href\" : \"https://api.spotify.com/v1/playlists/0123456789/tracks\",
          \"total\" : 143
        },
        \"type\" : \"playlist\",
        \"uri\" : \"spotify:playlist:222223323232\"
      }, {
        \"collaborative\" : false,
        \"description\" : \"Test playlist description 3.\",
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/playlist/00000000\"
        },
        \"href\" : \"https://api.spotify.com/v1/playlists/0123456789\",
        \"id\" : \"00011011\",
        \"images\" : [ {
          \"height\" : 640,
          \"url\" : \"https://mosaic.scdn.co/640/test-url-5\",
          \"width\" : 640
        }, {
          \"height\" : 300,
          \"url\" : \"https://mosaic.scdn.co/300/test-url-6\",
          \"width\" : 300
        }, {
          \"height\" : 60,
          \"url\" : \"https://mosaic.scdn.co/60/test-url-7\",
          \"width\" : 60
        } ],
        \"name\" : \"Doom Playlist 3\",
        \"owner\" : {
          \"display_name\" : \"test-user-3\",
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/user/test-user-3\"
          },
          \"href\" : \"https://api.spotify.com/v1/users/test-user-3\",
          \"id\" : \"test-user-3\",
          \"type\" : \"user\",
          \"uri\" : \"spotify:user:test-user-3\"
        },
        \"primary_color\" : null,
        \"public\" : null,
        \"snapshot_id\" : \"00011011\",
        \"tracks\" : {
          \"href\" : \"https://api.spotify.com/v1/playlists/0123456789/tracks\",
          \"total\" : 361
        },
        \"type\" : \"playlist\",
        \"uri\" : \"spotify:playlist:222223323232\"
      }, {
        \"collaborative\" : false,
        \"description\" : \"Test playlist description 4.\",
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/playlist/00000000\"
        },
        \"href\" : \"https://api.spotify.com/v1/playlists/0123456789\",
        \"id\" : \"00011011\",
        \"images\" : [ {
          \"height\" : 640,
          \"url\" : \"https://mosaic.scdn.co/640/test-url-8\",
          \"width\" : 640
        }, {
          \"height\" : 300,
          \"url\" : \"https://mosaic.scdn.co/300/test-url-9\",
          \"width\" : 300
        }, {
          \"height\" : 60,
          \"url\" : \"https://mosaic.scdn.co/60/test-url-10\",
          \"width\" : 60
        } ],
        \"name\" : \"Doom Playlist 4\",
        \"owner\" : {
          \"display_name\" : \"test-user-4\",
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/user/00000000\"
          },
          \"href\" : \"https://api.spotify.com/v1/users/test-user-4\",
          \"id\" : \"00011011\",
          \"type\" : \"user\",
          \"uri\" : \"spotify:user:222223323232\"
        },
        \"primary_color\" : null,
        \"public\" : null,
        \"snapshot_id\" : \"00011011\",
        \"tracks\" : {
          \"href\" : \"https://api.spotify.com/v1/playlists/0v9cjE3vjgCv0aVXpeIlHT/tracks\",
          \"total\" : 924
        },
        \"type\" : \"playlist\",
        \"uri\" : \"spotify:playlist:222223323232\"
      } ],
      \"limit\" : 5,
      \"next\" : \"https://api.spotify.com/v1/search?query=doom+metal&type=playlist&offset=5&limit=5\",
      \"offset\" : 0,
      \"previous\" : null,
      \"total\" : 2077
    }
  }"
  "A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/search' endpoint.

https://developer.spotify.com/documentation/web-api/reference/search/search/#response-format

Obtained via: curl -X GET 'https://api.spotify.com/v1/search?q=doom%20metal&type=playlist&limit=5' -H 'Authorization: Bearer <insert *spotify-oauth2-token* here>' > search-playlist.txt
")


(defconst spotify-ert/data/connect-api/featured-playlists
  "{
    \"message\" : \"This is the test featured playlist data!\",
    \"playlists\" : {
      \"href\" : \"https://api.spotify.com/v1/browse/featured-playlists?country=SE&timestamp=2015-05-18T06:44:32&offset=0&limit=2\",
      \"items\" : [ {
        \"collaborative\" : false,
        \"description\" : \"test-featured-playlist-0\",
        \"external_urls\" : {
          \"spotify\" : \"http://open.spotify.com/user/test-user-0/playlist/test-featured-playlist-0\"
        },
        \"href\" : \"https://api.spotify.com/v1/users/test-user-0/playlists/test-featured-playlist-0\",
        \"id\" : \"test-featured-playlist-0\",
        \"images\" : [ {
          \"height\" : 300,
          \"url\" : \"https://i.scdn.co/image/test-featured-playlist-0\",
          \"width\" : 300
        } ],
        \"name\" : \"test-featured-playlist-0\",
        \"owner\" : {
          \"external_urls\" : {
            \"spotify\" : \"http://open.spotify.com/user/test-user-0\"
          },
          \"href\" : \"https://api.spotify.com/v1/users/test-user-0\",
          \"id\" : \"spotify\",
          \"type\" : \"user\",
          \"uri\" : \"spotify:user:test-user-0\"
        },
        \"public\" : null,
        \"snapshot_id\" : \"0000/test-featured-playlist-0\",
        \"tracks\" : {
          \"href\" : \"https://api.spotify.com/v1/users/test-user-0/playlists/test-featured-playlist-0/tracks\",
          \"total\" : 245
        },
        \"type\" : \"playlist\",
        \"uri\" : \"spotify:user:test-user-0:playlist:test-featured-playlist-0\"
      }, {
        \"collaborative\" : false,
        \"description\" : \"test-featured-playlist-1\",
        \"external_urls\" : {
          \"spotify\" : \"http://open.spotify.com/user/test-user-1/playlist/test-featured-playlist-1\"
        },
        \"href\" : \"https://api.spotify.com/v1/users/test-user-1/playlists/test-featured-playlist-1\",
        \"id\" : \"4uOEx4OUrkoGNZoIlWMUbO\",
        \"images\" : [ {
          \"height\" : 300,
          \"url\" : \"https://i.scdn.co/image/test-url-1\",
          \"width\" : 300
        } ],
        \"name\" : \"test-featured-playlist-1\",
        \"owner\" : {
          \"external_urls\" : {
            \"spotify\" : \"http://open.spotify.com/user/test-user-1\"
          },
          \"href\" : \"https://api.spotify.com/v1/users/test-user-1\",
          \"id\" : \"test-user-1\",
          \"type\" : \"user\",
          \"uri\" : \"spotify:user:test-user-1\"
        },
        \"public\" : null,
        \"snapshot_id\" : \"0001/test-featured-playlist-1\",
        \"tracks\" : {
          \"href\" : \"https://api.spotify.com/v1/users/test-user-1/playlists/test-featured-playlist-1/tracks\",
          \"total\" : 38
        },
        \"type\" : \"playlist\",
        \"uri\" : \"spotify:user:test-user-1:playlist:test-featured-playlist-1\"
      } ],
      \"limit\" : 2,
      \"next\" : \"https://api.spotify.com/v1/browse/featured-playlists?country=SE&timestamp=2015-05-18T06:44:32&offset=2&limit=2\",
      \"offset\" : 0,
      \"previous\" : null,
      \"total\" : 12
    }
  }"

  "A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/browse/featured-playlists' endpoint.

https://developer.spotify.com/documentation/web-api/reference-beta/#endpoint-get-featured-playlists

Obtained via: curl -X GET 'https://api.spotify.com/v1/search?q=doom%20metal&type=playlist&limit=5' -H 'Authorization: Bearer <insert *spotify-oauth2-token* here>' > search-playlist.txt
")


(defconst spotify-ert/data/connect-api/playlist-tracks
  "{
    \"href\": \"https://api.spotify.com/v1/users/test-user-0/playlists/test-playlist-id-0/tracks\",
    \"items\": [
      {
        \"added_at\": \"2016-10-11T13:44:40Z\",
        \"added_by\": {
          \"external_urls\": {
            \"spotify\": \"http://open.spotify.com/user/test-user-0\"
          },
          \"href\": \"https://api.spotify.com/v1/users/test-user-0\",
          \"id\": \"test-user-0\",
          \"type\": \"user\",
          \"uri\": \"spotify:user:test-user-0\"
        },
        \"is_local\": false,
        \"track\": {
          \"album\": {
            \"album_type\": \"single\",
            \"artists\": [
              {
                \"external_urls\": {
                  \"spotify\": \"https://open.spotify.com/artist/test-id-0\"
                },
                \"href\": \"https://api.spotify.com/v1/artists/test-id-0\",
                \"id\": \"test-id-0\",
                \"name\": \"test-artist-0\",
                \"type\": \"artist\",
                \"uri\": \"spotify:artist:test-id-0\"
              }
            ],
            \"available_markets\": [
              \"TW\",
              \"UY\"
            ],
            \"external_urls\": {
              \"spotify\": \"https://open.spotify.com/album/test-id-1\"
            },
            \"href\": \"https://api.spotify.com/v1/albums/test-id-1\",
            \"id\": \"test-id-1\",
            \"images\": [
              {
                \"height\": 640,
                \"url\": \"https://i.scdn.co/image/test-image-id-0\",
                \"width\": 640
              },
              {
                \"height\": 300,
                \"url\": \"https://i.scdn.co/image/test-image-id-1\",
                \"width\": 300
              },
              {
                \"height\": 64,
                \"url\": \"https://i.scdn.co/image/test-image-id-2\",
                \"width\": 64
              }
            ],
            \"name\": \"test-album-0\",
            \"type\": \"album\",
            \"uri\": \"spotify:album:test-id-1\"
          },
          \"artists\": [
            {
              \"external_urls\": {
                \"spotify\": \"https://open.spotify.com/artist/test-id-0\"
              },
              \"href\": \"https://api.spotify.com/v1/artists/test-id-0\",
              \"id\": \"test-id-0\",
              \"name\": \"test-artist-0\",
              \"type\": \"artist\",
              \"uri\": \"spotify:artist:test-id-0\"
            },
            {
              \"external_urls\": {
                \"spotify\": \"https://open.spotify.com/artist/test-id-1\"
              },
              \"href\": \"https://api.spotify.com/v1/artists/test-id-1\",
              \"id\": \"test-id-1\",
              \"name\": \"test-artist-1\",
              \"type\": \"artist\",
              \"uri\": \"spotify:artist:test-id-1\"
            }
          ],
          \"available_markets\": [
            \"AD\",
            \"AR\",
            \"TW\",
            \"UY\"
          ],
          \"disc_number\": 1,
          \"duration_ms\": 209453,
          \"explicit\": false,
          \"external_ids\": {
            \"isrc\": \"test-isrc-0\"
          },
          \"external_urls\": {
            \"spotify\": \"https://open.spotify.com/track/test-id-2\"
          },
          \"href\": \"https://api.spotify.com/v1/tracks/test-id-2\",
          \"id\": \"test-id-2\",
          \"name\": \"test-artist-2 (feat. test-artist-1)\",
          \"popularity\": 85,
          \"preview_url\": \"https://p.scdn.co/mp3-preview/test-url-0\",
          \"track_number\": 1,
          \"type\": \"track\",
          \"uri\": \"spotify:track:test-id-2\"
        }
      },
      {
        \"added_at\": \"2016-10-11T13:44:40Z\",
        \"added_by\": {
          \"external_urls\": {
            \"spotify\": \"http://open.spotify.com/user/test-user-0\"
          },
          \"href\": \"https://api.spotify.com/v1/users/test-user-0\",
          \"id\": \"test-user-0\",
          \"type\": \"user\",
          \"uri\": \"spotify:user:test-user-0\"
        },
        \"is_local\": false
      }
    ],
    \"limit\": 100,
    \"next\": null,
    \"offset\": 0,
    \"previous\": null,
    \"total\": 58
  }"

  "A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/playlists/{playlist-id}/tracks' endpoint.

https://developer.spotify.com/documentation/web-api/reference-beta/#endpoint-get-playlists-tracks

Obtained from Spotify documentation.
")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-ert-data-connect-api)
