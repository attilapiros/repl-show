(ns repl-show.view (:gen-class)
  (:require [glow.core :as glow]
            [clojure.tools.trace :as trace]))

(def default-left-margin-size 4)

(def view-config (atom {:view-width 75
                        :view-height 25
                        :show-footage true}))

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

(defn full-line [view-width left-margined-text]
  (let [right-margin-size (- view-width 
                             1
                             (text-length left-margined-text) 
                             1)]
    (str \* left-margined-text "\u001b[0m" (margin right-margin-size) "*")))

(defn get-line [text-with-marker view-width]
  (let [[alignment text-body]
        (condp re-matches text-with-marker
          #"<<(.*)"  :>> #(vector :flush-left (second %)) 
          #"<(.*)"  :>> #(vector :left (second %)) 
          #">>(.*)"  :>> #(vector :flush-right (second %)) 
          #">(.*)"  :>> #(vector :right (second %)) 
          #"[|](.*)" :>> #(vector :center (second %)) 
          #"/" :>> (fn [_] ( vector :center (string-with-n-char 
                                              (- view-width (* 2 default-left-margin-size)) \-))) 
          #"//" :>> (fn [_] ( vector :center (string-with-n-char 
                                               (- view-width 2) \-))) 
          [:default text-with-marker])
        colored-text (length-and-colored text-body) 
        colored-length (text-length colored-text) 
        left-margin-size (case alignment
                           :left default-left-margin-size
                           :right (- view-width default-left-margin-size 
                                     colored-length)
                           :flush-left 1
                           :flush-right (- view-width 3 colored-length)
                           :center (quot (- view-width 2 colored-length) 2) 
                           :default default-left-margin-size)
        left-margined-text (str (margin left-margin-size) colored-text)]  
    left-margined-text))

(defn format-code [code-block]
  (map #(let [{:keys [view-width]} @view-config]
          (full-line view-width (str (margin default-left-margin-size) %))) 
       (clojure.string/split-lines (glow/highlight code-block))))

(defn format-line 
  ([line]
   (format-line line (:view-width @view-config)))
  ([line view-width]
   (full-line view-width (get-line line view-width))))

(defn format-text [text-block]
  (if  text-block
    (let [{:keys  [view-width]} @view-config]
      (map #(format-line % view-width) 
           (clojure.string/split-lines text-block)))))


(defn slide-height [texts-and-codes]
  (reduce (fn [sum content] 
            (+ sum
               (inc (if (and content (not (empty? content))) 
                      (count (re-seq #"\n" content)) 0)))) 
          0 
          (flatten texts-and-codes)))

(defn format-build [build]
  (map-indexed (fn [idx content]
                 (if (even? idx) 
                   (format-text content)
                   (format-code content)))
               build))

(defn merge-texts-and-codes [texts-and-codes break-limit]
  (let [visible-builds (take break-limit texts-and-codes) 
        slide-height (slide-height texts-and-codes)
        slide-as-list (flatten (map format-build visible-builds))
        {:keys [view-width view-height show-footage]} @view-config

        remaining-lines (- view-height slide-height)
        n-lines-before (quot remaining-lines 2)
        n-lines-after (- 
                        (- view-height (count slide-as-list) n-lines-before)
                        (if show-footage 1 0))
        lines-before (repeat n-lines-before (full-line view-width ""))
        lines-after (repeat n-lines-after (full-line view-width ""))
        ]
    (clojure.string/join "\n" (into ( into ( into [] lines-before) slide-as-list) lines-after))))

(defn get-horizontal-frame []
  (string-with-n-char (:view-width @view-config) \*))

(defn format-slide [slide-content-text break-limit]
  (let [formatted-content  
        (merge-texts-and-codes slide-content-text break-limit)
        line 
        (get-horizontal-frame)]
    (apply str line "\n" formatted-content)))


(defn show-slide [content slide-footage break-limit]
  (println (format-slide content break-limit))
  (if (:show-footage @view-config)
    (println (format-line (str ">> " slide-footage))))
  (symbol (get-horizontal-frame)))
