(ns repl-show.parser (:gen-class)
    (:require [clojure.tools.trace :as trace]))

(def separators {"---"  :slide
                 "--"   :break
                 "```"  :code})

(defn identify-separators [text]
  (map #(get separators % %)
       (clojure.string/split-lines text)))

(defn chunks [separator coll] 
  (remove #(= [separator] %) 
          (partition-by #(not= separator %) coll)))

(defn breaks-to-even-position [break]
  (into (if (= :code (first break)) [nil] []) 
        (map #(clojure.string/join "\n" %) (chunks :code break))))

(defn slides [file-content]
  (->> file-content
       identify-separators
       (chunks :slide)
       (map #(chunks :break %)) 
       (map (fn [slide] 
              (map breaks-to-even-position slide)))))

(defn parse-to-slides [content-file-name]
  (slides (slurp content-file-name)))
