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
      \"id\" : \"1234567890\",
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
     \"id\" : \"1234567890\",
     \"is_local\" : false,
     \"is_playable\" : true,
     \"name\" : \"Foil\",
     \"popularity\" : 49,
     \"preview_url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
     \"track_number\" : 3,
     \"type\" : \"track\",
     \"uri\" : \"spotify:track:1234567890\"
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

(defconst spotify-ert/data/connect-api/search
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
'/v1/me/player' endpoint, cut down to just a device object.

https://developer.spotify.com/documentation/web-api/reference/player/get-a-users-available-devices/
")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-ert-data-connect-api)
