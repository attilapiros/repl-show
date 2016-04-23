(ns repl-show.parser (:gen-class)
  (:require [clojure.tools.trace :as trace]))

(def separators {"---"  :slide
                 "--"   :break
                 "```"  :code})

(defn identify-separators [text]
  (map #(get separators % %) (clojure.string/split-lines text)))

(defn chunks [separator coll] 
  (remove #(= [separator] %) 
          (partition-by #(not= separator %) coll)))

(defn to-pres-and-full-code [pres]
  {
   :pres pres
   :num-breaks (dec (count pres))
   :full-code (clojure.string/join "\n" (flatten (mapcat 
                (fn [break] 
                  (keep-indexed 
                    #(when (odd? %1) %2) 
                    break)) pres)))})

(defn slides [file-content]
      (->> file-content
           identify-separators
           (chunks :slide)
           (map #(chunks :break %)) 
           (map (fn [slide] 
                  (map (fn [break]
                    (into (if (= :code (first break)) [nil] []) 
                      (map #(clojure.string/join "\n" %) (chunks :code break)))) 
                    slide)))
           (map to-pres-and-full-code)))

(defn parse-to-slides [content-file-name]
 (slides (slurp content-file-name)))
