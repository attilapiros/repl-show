(ns repl-show.view (:gen-class)
  (:require [glow.core :as glow]
            [clojure.tools.trace :as trace]))

(def default-left-margin-size 4)

(def view-size [75 25])

(defn text-length [colored-text]
  (count (clojure.string/replace colored-text 
                                 #"\u001b\[[0-9]{0,2}m" "")))

(defn string-with-n-char [n one-char]
  (apply str (repeat n one-char)) )

(defn margin [margin-size]
  (string-with-n-char margin-size \ ))

(defn length-and-colored [text] 
  (let [ansi-map  {\\  \\
                   \*  "\u001b[1m"
                   \!  "\u001b[7m"
                   \_  "\u001b[4m"
                   \s  "\u001b[0m"
                   \r  "\u001b[31m"
                   \w  "\u001b[37m"
                   \k  "\u001b[30m"
                   \g  "\u001b[32m"
                   \b  "\u001b[34m"
                   \y  "\u001b[33m"
                   \c  "\u001b[36m"
                   \m  "\u001b[35m"
                   \R  "\u001b[41m"
                   \W  "\u001b[47m"
                   \K  "\u001b[40m"
                   \G  "\u001b[42m"
                   \B  "\u001b[44m"
                   \Y  "\u001b[43m"
                   \C  "\u001b[46m"
                   \M  "\u001b[45m"}
        ] 
    (loop [in-marker? false
           [curr & rest-text] text
           colored-text ""] 
      (if curr
        (if in-marker?
          (if-let [ansi-code (ansi-map curr)] 
            (recur false rest-text (str colored-text ansi-code))
            (recur false rest-text (str colored-text \\ curr)))
          (case curr
            \\  (recur true rest-text colored-text)
            (recur false rest-text (str colored-text curr))))  
        colored-text))))

(defn full-line [view-size-x left-margined-text]
  (let [right-margin-size (- view-size-x 
                             1
                             (text-length left-margined-text) 
                             1)]
    (str \* left-margined-text "\u001b[0m" (margin right-margin-size) "*")))

(defn get-line [text-with-marker view-size-x]
  (let [[alignment text-body]
        (condp re-matches text-with-marker
          #"<<(.*)"  :>> #(vector :flush-left (second %)) 
          #"<(.*)"  :>> #(vector :left (second %)) 
          #">>(.*)"  :>> #(vector :flush-right (second %)) 
          #">(.*)"  :>> #(vector :right (second %)) 
          #"[|](.*)" :>> #(vector :center (second %)) 
          #"/" :>> (fn [_] ( vector :center (string-with-n-char 
                                      (- view-size-x (* 2 default-left-margin-size)) \-))) 
          #"//" :>> (fn [_] ( vector :center (string-with-n-char 
                                               (- view-size-x 2) \-))) 
          [:default text-with-marker])
        colored-text (length-and-colored text-body) 
        colored-length (text-length colored-text) 
        left-margin-size (case alignment
                           :left default-left-margin-size
                           :right (- view-size-x default-left-margin-size 
                                     colored-length)
                           :flush-left 1
                           :flush-right (- view-size-x 3 colored-length)
                           :center (quot (- view-size-x 2 colored-length) 2) 
                           :default default-left-margin-size)
        left-margined-text (str (margin left-margin-size) colored-text)]  
    left-margined-text))

(defn format-code [code-block]
  (map #(let [[view-size-x _] view-size]
          (full-line view-size-x (str (margin default-left-margin-size) %))) 
       (clojure.string/split (glow/highlight code-block) #"\n")))

(defn format-text [text-block]
  (let [[view-size-x view-size-y] view-size]
    (map #(let [line %]  
            (full-line view-size-x (get-line line view-size-x))) 
         (clojure.string/split text-block #"\n"))))

(defn take-num-breaks [n-limit texts-and-codes]
  (loop [[h & more] texts-and-codes
         num-visited-breaks 0
         res []]
    (if (or (nil? h) (>= num-visited-breaks n-limit)) 
      res
      (recur more 
             (if (= h [:break]) 
               (inc num-visited-breaks) 
               num-visited-breaks) 
             (conj res h)))))


(defn merge-texts-and-codes [texts-and-codes break-limit]
  (let [t-c (take-num-breaks break-limit texts-and-codes)
        slide-as-list (filter #(not (empty? %))
                                 (mapcat (fn [[k v]]
                                                (case k 
                                                  :code (format-code v) 
                                                  :text  (format-text v)
                                                  :break  [""]))
                                      t-c))
        [view-size-x view-size-y] view-size

        remaining-lines (- view-size-y (count slide-as-list))
        n-lines-before (quot remaining-lines 2)
        n-lines-after (- remaining-lines n-lines-before 1)
        lines-before (repeat n-lines-before (full-line view-size-x ""))
        lines-after (repeat n-lines-after (full-line view-size-x ""))
        ]
        
    (clojure.string/join "\n" (into ( into ( into [] lines-before) slide-as-list) lines-after))))

(defn get-horizontal-frame []
  (string-with-n-char (first view-size) \*))

(defn format-slide [slide-content-text break-limit]
  (let [
        formatted-content  
                   (merge-texts-and-codes slide-content-text break-limit)
        line 
          (get-horizontal-frame)]
    (apply str line "\n" formatted-content)))


(defn show-slide [content break-limit]
  (println (format-slide content break-limit))
  (symbol (get-horizontal-frame)))
