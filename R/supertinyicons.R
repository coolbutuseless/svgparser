

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Names of icons in \code{supertinyicons} set
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
supertinyicon_names = c(
  "acast", "access", "adobe", "airbnb", "amazon_alexa", "amazon_s3",
  "amazon", "amberframework", "andotp", "android", "angellist",
  "angular", "ansible", "apereo", "apple_music", "apple", "arch_linux",
  "auth0", "authy", "backbone", "badoo", "baidu", "bandcamp", "behance",
  "bing", "bitbucket", "bitcoin", "bitwarden", "blogger", "bluetooth",
  "briar", "buffer", "bugcrowd", "calendar", "centos", "chrome",
  "chromium", "clojure", "cloudflare", "codeberg", "codepen", "coffeescript",
  "coil", "coinpot", "colaboratory", "cplusplus", "crystal", "css3",
  "datacamp", "debian", "deezer", "delicious", "dev_to", "digidentity",
  "digitalocean", "discord", "disqus", "djangoproject", "docker",
  "dribbble", "drone", "dropbox", "drupal", "duckduckgo", "ea",
  "ebay", "edge", "elastic", "element", "elementaryos", "email",
  "epub", "espressif", "ethereum", "etsy", "evernote", "facebook",
  "finder", "firefox", "flattr", "flickr", "flutter", "foobar2000",
  "freebsd", "freecodecamp", "friendica", "fritz", "gandi", "gatehub",
  "ghost", "git", "gitea", "github", "gitlab", "glitch", "gmail_old",
  "gmail", "go", "gogcom", "gojek", "goodreads", "google_calendar",
  "google_collaborative_content_tools", "google_docs_editors",
  "google_drive_old", "google_drive", "google_maps_old", "google_maps",
  "google_meet", "google_play", "google_plus", "google_podcasts",
  "google_scholar", "google", "gradle", "grafana", "hackernews",
  "hackerone", "haml", "heroku", "homekit", "hp", "html5", "humblebundle",
  "ibm", "iheartradio", "imdb", "imgur", "instagram", "intel",
  "intercom", "internet_archive", "itch_io", "itunes_podcasts",
  "jacobin", "java", "javascript", "jellyfin", "json", "julia",
  "kaggle", "keepassdx", "kemal", "keskonfai", "keybase", "kickstarter",
  "ko-fi", "kodi", "kotlin", "laravel", "lastpass", "liberapay",
  "line", "linkedin", "linux_mint", "linux", "lobsters", "lock",
  "luckyframework", "macos", "mail", "mailchimp", "malt", "markdown",
  "mastodon", "mattermost", "medium", "meetup", "messenger", "microformats",
  "microsoft", "minecraft", "nextcloud", "nhs", "npm", "ok", "olympics",
  "openbenches", "openbugbounty", "opencast", "opencollective",
  "opencores", "opensource", "openvpn", "opera", "orcid", "overcast",
  "overleaf", "patreon", "paypal", "pdf", "phone", "php", "pinboard",
  "pinterest", "pixelfed", "plex", "pocket", "pocketcasts", "preact",
  "print", "protonmail", "python", "qq", "raspberry_pi", "react",
  "reddit", "redhat", "researchgate", "roundcube", "rss", "ruby",
  "rubygems", "rubyonrails", "rust", "safari", "samsung_internet",
  "samsung_s", "samsung_swoop", "samsung", "sass", "semaphoreci",
  "sentry", "signal", "sketch", "skype", "slack", "slideshare",
  "snapchat", "soundcloud", "spotify", "square_cash", "stackexchange",
  "stackoverflow", "steam", "stitcher", "strava", "stumbleupon",
  "sublimetext", "svelte", "svg", "symantec", "taiga", "teamspeak",
  "telegram", "thisamericanlife", "threema", "tiktok", "tox", "trello",
  "tripadvisor", "tumblr", "tunein", "tutanota", "twilio", "twitch",
  "twitter", "typescript", "uber", "ubiquiti", "ubisoft", "ubuntu",
  "unicode", "untappd", "uphold", "uplay", "upwork", "vegetarian",
  "venmo", "viber", "vimeo", "vivino", "vk", "vlc", "vue", "w3c",
  "webassembly", "wechat", "wekan", "whatsapp", "whatwg", "wifi",
  "wikipedia", "windows", "wire", "wireguard", "wordpress", "workato",
  "xing", "xmpp", "yahoo", "yammer", "yarn", "yelp", "youtube",
  "yubico", "zoom"
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pick the closest matching word from a list of words.
#'
#' As long as 'word' & 'words' are not empty, this function should always
#' return a valid member of 'words'.
#'
#' @param word user word
#' @param words list of all words
#'
#' @importFrom utils adist
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shotgun_match <- function(word, words) {

  # If the word is in 'words' we have a perfect match!
  if (word %in% words) {
    return(word)
  }

  # Find the index of the word which is the closest Levenshtein edit
  # distance from the given word
  idx <- which.min(as.vector(adist(word, words)))

  # Return the closest word
  words[idx]
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Brute force file reader that is not concerned with file endings
#'
#' \code{readLines()} in combination with \code{unz()} fails o read the last
#' line of a file if it does not end in a CR/LF.  I haven't found a workaround
#' besides a manual read loop like below.
#'
#' This function reads all character in a file regardless of line endings or
#' file endings.
#'
#' @param conn connection. For svgparser this is always an \code{unz()} connection to
#'        the zipped version of the icon set
#'
#' @return single string representing file contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_file <- function(conn) {

  tot <- c()
  buf <- "init"
  while(length(buf) != 0) {
    buf <- readChar(conn, 10000)
    tot <- c(tot, buf)
  }

  paste(tot, collapse = "")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Load an SVG icon from the \code{supertinyicons} icon pack
#'
#' @param name name of icon. e.g. \code{"twitter"}.  If the exact name is not
#'        found, then the icon with
#'        the closest matching name is returned.  See \code{supertinyicon_names}
#'        for a full list of supported icons in this set.
#' @param obj_type What kind of R object to return - choices
#'        \code{c('grob', 'data.frame', 'list', 'debug', 'svg')}. Default: 'grob'.
#'        See documentation for \code{read_svg()} for more details.
#' @param ... other arguments passed to \code{read_svg()}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load_supertinyicon <- function(name, obj_type = c('grob', 'data.frame', 'list', 'debug', 'svg'), ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Shotghun match to find the closest icon name
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  best_name <- shotgun_match(name, supertinyicon_names)
  if (best_name != name) {
    message(sprintf("Icon '%s' not found. Using best match: '%s'", name, best_name))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # assemble filename and zipfilename
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  filename  <- paste0(best_name, ".svg")
  zipfile <- system.file(
    paste0("set/supertinyicons.zip"),
    package = 'svgparser',
    mustWork = TRUE
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # load and return this icon
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  obj_type <- match.arg(obj_type)
  if (obj_type != 'svg') {
    read_svg(unz(zipfile, filename), obj_type = obj_type, ...)
  } else {
    conn <- unz(zipfile, filename, open = 'rb')
    on.exit(close(conn))
    read_file(conn)
  }
}




