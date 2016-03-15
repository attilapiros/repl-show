(ns repl-show.core (:gen-class)
  (:require [glow.core :as glow]))

(def presentation-state 
  (atom {:curr-page-index -1
         :pages []}))

(def page-size [89 40])

(def content-file "content.txt")

(defn unify-end-of-line [text]
  (clojure.string/replace text "\r\n" "\n"))

(defn to-raw-pages [file-content]
  (clojure.string/split file-content #"\n---"))

(defn to-texts-and-codes [raw-page]
  (map-indexed 
    #(if (even? %1) [:text %2] [:code %2]) 
    (clojure.string/split raw-page #"\n```")))

(defn to-texts-and-codes-list [raw-pages]
  (map to-texts-and-codes raw-pages))

(defn pres-and-fullcode [texts-and-codes]
  {:pres  texts-and-codes
   :full-code (reduce 
           (fn [all [k v]] 
             (if (= k :code) (str all v "\n") all)) 
           "" 
           texts-and-codes)})

(defn fullcode-and-pres-list [texts-and-codes-list]
  (vec (map pres-and-fullcode texts-and-codes-list)))


(defn text-length [colored-text]
  (count (clojure.string/replace colored-text 
                                 #"\u001b\[[0-9]{0,2}m" "")))

(defn margin [margin-size]
  (apply str (repeat margin-size \ )) )

(defn length-and-colored [text] 
  (loop [in-marker? false
         [curr & rest-text] text
         colored-text ""] 
    (if curr
      (if in-marker?
        (case curr 
          \\  (recur false rest-text (str colored-text \\))
          \*  (recur false rest-text (str colored-text "\u001b[1m"))
          \!  (recur false rest-text (str colored-text "\u001b[7m"))
          \_  (recur false rest-text (str colored-text "\u001b[4m"))
          \s  (recur false rest-text (str colored-text "\u001b[0m"))
          \r  (recur false rest-text (str colored-text "\u001b[31m"))
          \w  (recur false rest-text (str colored-text "\u001b[37m"))
          \k  (recur false rest-text (str colored-text "\u001b[30m"))
          \g  (recur false rest-text (str colored-text "\u001b[32m"))
          \b  (recur false rest-text (str colored-text "\u001b[34m"))
          \y  (recur false rest-text (str colored-text "\u001b[33m"))
          \c  (recur false rest-text (str colored-text "\u001b[36m"))
          \m  (recur false rest-text (str colored-text "\u001b[35m"))
          \R  (recur false rest-text (str colored-text "\u001b[41m"))
          \W  (recur false rest-text (str colored-text "\u001b[47m"))
          \K  (recur false rest-text (str colored-text "\u001b[40m"))
          \G  (recur false rest-text (str colored-text "\u001b[42m"))
          \B  (recur false rest-text (str colored-text "\u001b[44m"))
          \Y  (recur false rest-text (str colored-text "\u001b[43m"))
          \C  (recur false rest-text (str colored-text "\u001b[46m"))
          \M  (recur false rest-text (str colored-text "\u001b[45m"))
          (recur false rest-text (str colored-text curr)))
        (case curr
          \\  (recur true rest-text colored-text)
          (recur false rest-text (str colored-text curr))))  
        colored-text)))

(defn full-line [page-size-x left-margined-text]
  (let [right-margin-size (- page-size-x 
                             1
                             (text-length left-margined-text) 
                             1)]
    (str \* left-margined-text (margin right-margin-size)  "\u001b[0m*")))

(defn get-line [text-with-marker page-size-x default-left-margin-size]
  (let [[alignment text-body]
        (condp re-matches text-with-marker
          #"<<(.*)"  :>> #(vector :flush-left (second %)) 
          #">>(.*)"  :>> #(vector :flush-right (second %)) 
          #"[|](.*)" :>> #(vector :center (second %)) 
          [:default text-with-marker])
        colored-text (length-and-colored text-body) 
        colored-length (text-length colored-text) 
        left-margin-size (case alignment
                           :flush-left 1
                           :flush-right (- page-size-x 3 colored-length)
                           :center (quot (- page-size-x 2 colored-length) 2) 
                           :default default-left-margin-size)
        left-margined-text (str (margin left-margin-size) colored-text)]  
    left-margined-text))

(defn format-code [code-block]
  (map #(let [[page-size-x _] page-size]
          (full-line page-size-x (str (margin 1) %))) 
       (clojure.string/split (glow/highlight code-block) #"\n")))

(defn format-text [text-bolock]
  (let [[page-size-x page-size-y] page-size
        default-left-margin-length 4]
    (map #(full-line page-size-x (get-line % page-size-x default-left-margin-length)) 
         (clojure.string/split text-bolock #"\n"))))

(defn merge-texts-and-codes [texts-and-codes]
  (clojure.string/join "\n" 
                       (filter #(not (empty? %))
                               (map (fn [[k v]]
                                      (let [r
                                            (clojure.string/join 
                                              "\n" 
                                              (case k 
                                                :code (format-code v) 
                                                :text  (format-text v)
                                                )) ] 
                                        r))
                                    texts-and-codes))))

(defn update-pres-merged-text [pres-and-fullcode-list]
  (map #(update % :pres merge-texts-and-codes) pres-and-fullcode-list))

(defn start []
  (reset! 
    presentation-state 
    {
     :curr-page-index -1 
     :pages (-> (slurp content-file)
                (unify-end-of-line) ; \r\n -> \n
                (to-raw-pages) ; cut by ---
                (to-texts-and-codes-list) ; ["page1" pag2] ->  [{:text "" :code ""}]
                (fullcode-and-pres-list) ; {:pres [:text "" :code ""] :full-code ""}
            )})
  "")

(defn get-horizontal-frame []
    (apply str (repeat (first page-size) \*)))

(defn format-page [page-content-text]
  (let [
        formatted-content  
                   (merge-texts-and-codes page-content-text)
        line 
          (get-horizontal-frame)]
    (apply str line "\n" formatted-content)))

(defn- curr-page []
  (let [contents (:pages @presentation-state)
        curr-page-content 
          (let [index (:curr-page-index @presentation-state)]
            (if (< index 0) "Use (start)"
              (get contents index "The End!")))]
      (format-page (:pres curr-page-content))))

(defn redraw-page []
  (println (curr-page))
  (symbol (get-horizontal-frame)))

(defn next-page [] 
  (when (< (:curr-page-index @presentation-state) 
             (count (:pages @presentation-state))) 
      (swap! presentation-state #(update-in % [:curr-page-index] inc)))
  (redraw-page))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
