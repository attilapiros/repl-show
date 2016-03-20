(ns repl-show.core (:gen-class)
  (:require [glow.core :as glow]))

(def presentation-state 
  (atom {:curr-page-index -1
         :break-limit 1
         :pages []}))

(def page-size [70 40])

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

(defn to-builds-with-break [texts-and-codes] 
    (mapcat (fn [[ k v]] 
              (if (= k :text) 
                (interpose [:break] 
                           (map #(vector :text %) 
                                (clojure.string/split v #"\n--")))  
                [[:code v]])) 
            texts-and-codes))

(defn count-breaks [texts-and-codes]
  (count (filter #(= % [:break]) texts-and-codes)))

(defn take-num-breaks [texts-and-codes n]
  (loop [[h & more] texts-and-codes
         num-visited-breaks 0
         res []]
    (if (or (nil? h) (>= num-visited-breaks n)) 
      res
      (recur more 
             (if (= h [:break]) 
               (inc num-visited-breaks) 
               num-visited-breaks) 
             (conj res h)))))

(defn pres-and-fullcode [texts-and-codes]
  {:pres  (to-builds-with-break texts-and-codes)
   :full-code (reduce 
           (fn [all [k v]] 
             (if (= k :code) (str all v "\n") all)) 
           "" 
           texts-and-codes)})

(defn- exec-code-expression-by-expression [code]
  (with-open 
    [reader (new java.io.PushbackReader(new java.io.StringReader code))] 
    (loop [s (read reader false :eof-during-code-reaading)
           res nil
           very-first true]
      (if (not= :eof-during-code-reaading s)
        (do 
          (if (not very-first) (println res))
          (print (str (.name *ns*) "=>"))
          (prn s)
          (let [res (eval s)]
            (recur (read reader false :eof-during-code-reaading) res false)))
        res))))

(defn run []
  (let [index (:curr-page-index @presentation-state)
        code (get-in @presentation-state [:pages index :full-code])]
        (if (not (nil? code)) 
          (exec-code-expression-by-expression code))))


(defn fullcode-and-pres-list [texts-and-codes-list]
  (vec (map pres-and-fullcode texts-and-codes-list)))


(defn text-length [colored-text]
  (count (clojure.string/replace colored-text 
                                 #"\u001b\[[0-9]{0,2}m" "")))

(defn margin [margin-size]
  (apply str (repeat margin-size \ )) )

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

(defn full-line [page-size-x left-margined-text]
  (let [right-margin-size (- page-size-x 
                             1
                             (text-length left-margined-text) 
                             1)]
    (str \* left-margined-text "\u001b[0m" (margin right-margin-size) "*")))

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

(defn merge-texts-and-codes [texts-and-codes break-limit]
  (let [t-c (take-num-breaks texts-and-codes break-limit)]
    (clojure.string/join "\n" 
                         (filter #(not (empty? %))
                                 (map (fn [[k v]]
                                        (let [r
                                              (clojure.string/join 
                                                "\n" 
                                                (case k 
                                                  :code (format-code v) 
                                                  :text  (format-text v)
                                                  :break  ""
                                                  )) ] 
                                          r))
                                      t-c)))))

(defn start []
  (reset! 
    presentation-state 
    {
     :curr-page-index -1 
     :break-limit 1
     :pages (-> (slurp content-file)
                (unify-end-of-line) ; \r\n -> \n
                (to-raw-pages) ; cut by ---
                (to-texts-and-codes-list) ; ["page1" pag2] ->  [{:text "" :code ""}]
                (fullcode-and-pres-list) ; {:pres [:text "" :code ""] :full-code ""}
            )})
  "")

(defn get-horizontal-frame []
    (apply str (repeat (first page-size) \*)))

(defn format-page [page-content-text break-limit]
  (let [
        formatted-content  
                   (merge-texts-and-codes page-content-text break-limit)
        line 
          (get-horizontal-frame)]
    (apply str line "\n" formatted-content)))

(defn get-page-content [index]
   (let  [ {:keys [break-limit pages]} @presentation-state]
          (if (< index 0) [[:text "Use (start)"]]
              (:pres (get pages index {:pres [[:text "The End!"]]})))))

(defn- curr-page []
  (format-page (get-page-content (:curr-page-index @presentation-state)) (:break-limit @presentation-state)))

(defn redraw-page []
  (println (curr-page))
  (symbol (get-horizontal-frame)))

(defn n []
  (let [{:keys [curr-page-index pages break-limit]} @presentation-state
        num-breaks (count-breaks (get-page-content curr-page-index))] 
    (if (< break-limit num-breaks) 
      (swap! presentation-state #(update-in % [:break-limit] inc)) 
      (if (< curr-page-index (count pages))
        (swap! presentation-state #(update-in % [:curr-page-index] inc)))
      ) 
    ) 
  (redraw-page))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
