(ns repl-show.parser (:gen-class)
  (:require [clojure.tools.trace :as trace]))

(defn unify-end-of-line [text]
  (clojure.string/replace text "\r\n" "\n"))

(defn to-raw-slides [input-file-content]
  (clojure.string/split input-file-content #"\n---"))

(defn to-texts-and-codes [raw-slide]
  (let [[f & r :as splitted] 
        (clojure.string/split raw-slide #"\n```")
        first-code? (.startsWith f "```") 
        decide-code-or-text (if first-code? odd? even?)]
  (map-indexed 
    #(if (decide-code-or-text %1) [:text %2] [:code %2]) 
    (if first-code? 
      (into [(.substring f 3)] r)
      splitted))))

(defn to-texts-and-codes-list [raw-slides]
  (map to-texts-and-codes raw-slides))

(defn to-builds-with-break [texts-and-codes] 
    (mapcat (fn [[ k v]] 
              (if (= k :text) 
                (interpose [:break] 
                           (map #(vector :text %) 
                                (clojure.string/split v #"\n--")))  
                [[:code v]])) 
            texts-and-codes))

(defn pres-and-fullcode [texts-and-codes]
  {:pres  (to-builds-with-break texts-and-codes)
   :full-code (reduce 
           (fn [all [k v]] 
             (if (= k :code) (str all v "\n") all)) 
           "" 
           texts-and-codes)})

(defn fullcode-and-pres-list [texts-and-codes-list]
  (vec (map pres-and-fullcode texts-and-codes-list)))

(defn parse-to-slides [content-file-name]
 (-> (slurp content-file-name)
                  (unify-end-of-line) 
                  (to-raw-slides)
                  (to-texts-and-codes-list) 
                  (fullcode-and-pres-list)))
