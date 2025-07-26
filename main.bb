#!/usr/bin/env bb

(require '[taoensso.timbre :as log])
(require '[clojure.data.csv :as csv])
(require '[cheshire.core :as json])
(require '[clojure.java.io :as io])
(require '[babashka.fs :as fs])
(require '[babashka.process :refer [shell]])
(require '[clojure.string :as str])

;; === Model ===

(defrecord CoverImage [^String url
                       ^int width
                       ^int height])

(defrecord ChannelEntry [^String id
                         ^String title
                         ^String description
                         ^long timestamp
                         ^double duration
                         ^String url
                         cover-images
                         ^String audio-url])

(defrecord Channel [^String id
                    ^String title
                    ^String description
                    ^String url
                    ^String avatar-url
                    entries])

;; === Make [Channel] ===

(defn channel-info-download-cmd
  "Generate yt-dlp cmd for downloading [Channel] info"
  [channel-id]
  (format
   "yt-dlp -s -q --abort-on-error --flat-playlist -J %s"
   (str "https://m.youtube.com/channel/" channel-id)))

(comment
  (channel-info-download-cmd "UC6IYxkyLgsCer80qS8hRbFg"))

(defn download-channel-info
  "Download [Channel] info"
  [channel-id save-file]
  (let [cmd (channel-info-download-cmd channel-id)
        _ (log/debug "[" channel-id "]" "Executing shell cmd:" cmd)
        _ (shell {:out save-file} cmd)]))

(defn channel-entries-audio-urls-download-cmd
  "Generate yt-dlp cmd for downloading [audio-url]s of [ChannelEntry]s"
  [channel-id save-file]
  (let [template "%(id)s,%(urls)s"
        ; sleep-opts "--sleep-requests 1 --retry-sleep linear=1::2"
        sleep-opts ""]
    (format
     "yt-dlp %s -s -f 'ba' --print-to-file '%s' '%s' %s"
     sleep-opts
     template
     save-file
     (str "https://m.youtube.com/channel/" channel-id))))

(comment
  (channel-entries-audio-urls-download-cmd "UC6IYxkyLgsCer80qS8hRbFg" "_channel-entries-audio-urls.csv"))

(defn download-channel-entries-audio-urls
  "Download [audio-url]s of [ChannelEntry]s"
  [channel-id save-file]
  (let [cmd (channel-entries-audio-urls-download-cmd channel-id save-file)
        _ (log/debug "[" channel-id "]" "Executing shell cmd:" cmd)
        _ (shell cmd)]))

(defn parse-channel
  "Parse [Channel] from the given files"
  [channel-info-file channel-entries-audio-urls-file]
  (with-open [r (io/reader channel-info-file)
              r2 (io/reader channel-entries-audio-urls-file)]
    (let [data (json/parse-stream r true)
          entry-id->audio-url (into {} (csv/read-csv r2))]
      (map->Channel {:id (:channel_id data)
                     :title (:channel data)
                     :description (:description data)
                     :url (:channel_url data)
                     :avatar-url (some->> (:thumbnails data)
                                          (filter #(= "avatar_uncropped" (:id %)))
                                          first
                                          :url)
                     :entries (some->> (:entries data)
                                       (mapv #(map->ChannelEntry {:id (:id %)
                                                                  :title (:title %)
                                                                  :description (:description %)
                                                                  :timestamp (:timestamp %)
                                                                  :duration (:duration %)
                                                                  :url (:url %)
                                                                  :audio-url (get entry-id->audio-url (:id %))
                                                                  :cover-images (some->> (:thumbnails %)
                                                                                         (mapv map->CoverImage))})))}))))

(comment
  (let [channel-id "UC6IYxkyLgsCer80qS8hRbFg"
        dir channel-id
        channel-info-file (str dir "/_channel.json")
        channel-entries-audio-urls-file  (str dir "/_channel-entries-audio-urls.csv")]
    (parse-channel channel-info-file channel-entries-audio-urls-file)))

(defn make-channel
  "Make [Channel]"
  [channel-id]
  (let [dir channel-id
        channel-info-file (str dir "/_channel.json")
        channel-entries-audio-urls-file (str dir "/_channel-entries-audio-urls.csv")]
    (when-not  (fs/exists? dir)
      (log/info "[" channel-id "]" "Creating dir")
      (fs/create-dir dir))
    (log/info "[" channel-id "]" "Downloading channel info")
    (download-channel-info channel-id channel-info-file)
    (log/info "[" channel-id "]" "Downloading audio-urls of channel entries")
    (download-channel-entries-audio-urls channel-id channel-entries-audio-urls-file)
    (log/info "[" channel-id "]" "Parsing channel")
    (parse-channel channel-info-file channel-entries-audio-urls-file)))

(comment
  (make-channel "UC6IYxkyLgsCer80qS8hRbFg"))

;; === [Channel] to rss ===

(defn rfc1123-datetime-formatted
  [timestamp]
  (->
   (java.time.Instant/ofEpochSecond timestamp)
   (.atZone (java.time.ZoneId/of "UTC"))
   (.format  java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)))

(comment
  (rfc1123-datetime-formatted 1678886400))

(defn replace-and-char
  "Replace & to &amp;"
  [s]
  (str/replace s "&" "&amp;"))

(comment
  (replace-and-char "https://abc.xyz?foo=1&bar=2&baz=3"))

(defn write-cdata
  "Write [data] wrapped by [CDATA] into [writer]"
  [writer data]
  (doto writer
    (.write "<![CDATA[ ")
    (.write data)
    (.write " ]]>")))

(defn channel-entry->rss
  "Write [ChannelEntry] as rss xml"
  [writer {:keys [id
                  title
                  description
                  cover-images
                  url
                  audio-url
                  duration
                  timestamp]}]

  (doto writer
    (.write "<item>")
    (.write "<guid>")
    (.write id)
    (.write "</guid>")
    (.write "<title>")
    (write-cdata title)
    (.write "</title>")
    (.write "<description>")
    (write-cdata description)
    (.write "</description>")
    (.write "<pubDate>")
    (.write (or (some-> timestamp rfc1123-datetime-formatted) ""))
    (.write "</pubDate>")
    (.write "<itunes:duration>")
    (.write (str duration))
    (.write "</itunes:duration>")
    (.write (format "<itunes:image href=\"%s\"/>" (-> (last cover-images) :url replace-and-char)))
    (.write (format "<enclosure url=\"%s\" type=\"audio/mpeg\"/>" (-> audio-url replace-and-char)))
    (.write "</item>")))

(defn channel-entries->rss
  [writer entries]
  (doseq [e entries]
    (channel-entry->rss writer e))
  writer)

(defn channel->rss
  "Write [Channel] as rss xml"
  [save-file {:keys [id title description url avatar-url entries]}]
  (with-open [w (io/writer save-file)]
    (doto w
      (.write "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
      (.write "<rss xmlns:itunes=\"http://www.itunes.com/dtds/podcast-1.0.dtd\" xmlns:atom=\"http://www.w3.org/2005/Atom\" version=\"2.0\">")
      (.write "<channel>")
      (.write "<title>")
      (write-cdata title)
      (.write "</title>")
      (.write "<description>")
      (write-cdata description)
      (.write "</description>")
      (.write "<link>")
      (.write url)
      (.write "</link>")
      (.write (format "<itunes:image href=\"%s\"/>" (-> avatar-url replace-and-char)))
      (channel-entries->rss entries)
      (.write "</channel>")
      (.write "</rss>")
      (.flush))
    nil))

;; === Main ===

(defn -main [args]
  (let [channel-id "UC6IYxkyLgsCer80qS8hRbFg"
        rss-xml-file (str channel-id "/_rss.xml")
        channel (make-channel channel-id)]
    (log/info "[" channel-id "]" "Writing channel as rss xml")
    (channel->rss rss-xml-file channel)))

(when (= *file* (System/getProperty "babashka.file"))
  (-main *command-line-args*))

(comment
  (let [channel-id "UC6IYxkyLgsCer80qS8hRbFg"
        dir channel-id
        channel-info-file (str dir "/_channel.json")
        channel-entries-audio-urls-file  (str dir "/_channel-entries-audio-urls.csv")
        rss-xml-file (str dir "/_rss.xml")
        channel (parse-channel channel-info-file channel-entries-audio-urls-file)]
    (channel->rss rss-xml-file channel)))
