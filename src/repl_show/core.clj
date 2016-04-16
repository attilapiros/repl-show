(ns repl-show.core (:gen-class)
  (:require [glow.core :as glow]
            [repl-show.parser :refer [parse-to-slides]]
            [repl-show.view :refer [show-slide]]
            [clojure.tools.trace :as trace]))

(def presentation-state 
  (atom {:curr-slide-index -1
         :break-limit 1
         :slides []}))

(def content-file-name "content_orig.txt")

(defn count-breaks [texts-and-codes]
  (count (filter #(= % [:break]) texts-and-codes)))

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
  (let [index (:curr-slide-index @presentation-state)
        code (get-in @presentation-state [:slides index :full-code])]
        (if (not (nil? code)) 
          (exec-code-expression-by-expression code))))

(defn start 
  ([] (start content-file-name))
  ([content-file-name]
    (reset! 
      presentation-state 
      {
       :curr-slide-index -1 
       :break-limit 1
       :slides (parse-to-slides content-file-name)})
  ""))

(defn get-slide-content [index]
   (let  [ {:keys [break-limit slides]} @presentation-state]
          (if (< index 0) [[:text "Use (start)"]]
              (:pres (get slides index {:pres [[:text "| \\r The End!"]]})))))

(defn redraw-slide []
 (let [{:keys [curr-slide-index slides break-limit]} @presentation-state]
    (show-slide 
      (get-slide-content curr-slide-index) break-limit)))

(defn n
  ([] (n 1))
  ([num]
  (let [{:keys [curr-slide-index slides break-limit]} @presentation-state
        num-breaks (count-breaks (get-slide-content curr-slide-index))] 
    (if (<= break-limit num-breaks) 
      (swap! presentation-state #(update-in % [:break-limit] inc)) 
      (if (< curr-slide-index (count slides))
        (swap! presentation-state #(update-in % [:curr-slide-index] inc)))
      ) 
    ) 
  (redraw-slide)))
