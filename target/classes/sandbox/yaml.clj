(ns scratch
  (:require [clojure.string :as str]))

; title	        Title of the article or page
; date	        Publication date (e.g., YYYY-MM-DD HH:SS)
; modified	Modification date (e.g., YYYY-MM-DD HH:SS)
; tags	        Content tags, separated by commas
; keywords	Content keywords, separated by commas (HTML content only)
; category	Content category (one only — not multiple)
; slug	        Identifier used in URLs and translations
; author	Content author, when there is only one
; authors	Content authors, when there are multiple
; summary	Brief description of content for index pages
; lang	        Content language ID (en, fr, etc.)
; translation	If content is a translation of another (true or false)
; status	Content status: draft, hidden, or published
; template	Name of template to use to generate content (without extension)
; save_as	Save content to this relative file path
; url	        URL to use for this article/page

(def metadata
  {:title     nil
   :date      nil
   :modified  nil
   :tags      nil
   :category  nil
   :slug      nil
   :summary   nil
   :status    nil})

(keys metadata)

(def data "data/data.md")

(defn yaml-headers
  "
  Serializes yaml headers into a vector 

  Example:
    ---
    title: Title
    date: 2021-03-01
    ---

    [\"title: Title\" \"date: 2021-03-01\"] 
  "

  [file]
  (-> (slurp file)
      (str/split #"---\n")
      second
      (str/split #"\n")))

(yaml-headers "data/data.md")

(defn title-map [title]
  (->> title
       (map #(str/split % #":"))
       (map #(hash-map (keyword (first %)) (second %)))
       (apply merge)))

(into metadata (title-map (yaml-headers "data/data.md")))

(filter #(->> % val some?)
        (->> "data/data.md" yaml-headers title-map (merge metadata)))

(name (keyword "Title"))


