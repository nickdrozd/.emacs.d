(require 'engine-mode)

(defengine amazon
  "https://www.amazon.com/s/field-keywords=%s"
  :keybinding "a")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(defengine google-maps
  "http://maps.google.com/maps?q=%s"
  :keybinding "m")

(defengine hacker-news
  "https://hn.algolia.com/?q=%s"
  :keybinding "h")

(defengine imdb
  "http://www.imdb.com/find?s=all&q=%s"
  :keybinding "i")

(defengine oeis
  "http://oeis.org/search?q=%s"
  :keybinding "o")

(defengine rap-genius
  "http://genius.com/search?q=%s"
  :keybinding "r")

(defengine robert-christgau
  "http://robertchristgau.com/get_artist.php?name=%s"
  :keybinding "c")

(defengine turkish-dictionary
  "http://www.turkishdictionary.net/?word=%s"
  :keybinding "t")

(defengine wikipedia
  "https://en.wikipedia.org/w/index.php?title=Special:Search&search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://en.wiktionary.org/w/index.php?title=Special:Search&search=%s"
  :keybinding "k")
