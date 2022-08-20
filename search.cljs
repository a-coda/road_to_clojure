(ns searchme.core 
  (:require [clojure.string :as str]
            [goog.string :as gstr]
            [clojure.set :as set]))
 
(extend-type js/FileList
  ISeqable
  (-seq [array] (array-seq array 0)))
 
(defn element [id]
  (.getElementById js/document id))
 
(defn set-html! [id html]
  (set! (.-innerHTML (element id)) html))
 
(defn method-on-this? [method]
  (let [this (js* "this")]
    (.hasOwnProperty this method)))
 
(defn file-api-supported? []
  (every? method-on-this? '("File" "FileReader" "FileList" "Blob")))
 
(defn tokenize [text]
  (map str/lower-case (str/split text #"\s+")))
 
(def index {})
 
(defn lookup-keywords [text]
  (reduce set/intersection (map index (tokenize text))))
 
(defn index-contents [contents file]
  (doseq [word (tokenize contents)]
    (set! index (assoc index word (conj (index word #{}) file)))))
 
(defn index-file [event file]
  (index-contents (.-result (.-target event)) file)
  (set-html! "statistics" (gstr/format "Found %d words" (count index))))
 
(defn start-indexing-files [event]
  (let [files (.-files (.-target event))]
    (if files
      (doseq [file files]
        (let [reader (js/FileReader.)]
          (set! (.-onload reader) (fn [event] (index-file event file))) 
          (.readAsText reader file)))
      (js/alert "Failed to index any files"))))
 
(defn search-index [event]
  (let [files (lookup-keywords (.-value (element "searchinput")))
        results (if (pos? (count files))
                  (str/join (map (fn [file] (gstr/format "<li>%s</li>" (.-name file))) files))
                  "<li>Not found</li>")]
    (set-html! "results" results)))
 
(if (file-api-supported?)
  (do
    (.addEventListener (element "fileinput") "change" start-indexing-files false)
    (.addEventListener (element "searchinput") "input" search-index false))
  (js/alert "The JavaScript File API is not supported in your browser"))
