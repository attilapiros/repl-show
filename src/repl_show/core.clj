(ns repl-show.core (:gen-class)
  (:require [glow.core :as glow]
            [repl-show.parser :refer [parse-to-slides]]
            [repl-show.view :refer [show-slide view-config]]
            [clojure.tools.trace :as trace]))

(def help-content ["| \\r Repl-Show Help"
                   ""
                   "(n <num>) : go to next <num> slides/builds, <num> is 1 by default"
                   ""
                   "(p <num>) : go back to <num> slides/builds, <num> is 1 by default"   
                   ""
                   "(re)      : redraw the current slide / exit help"
                   ""   
                   "(g <idx>) : go to the <idx> slide"
                   ""
                   "(f)       : go to the first side"
                   "(l)       : go to the last side"
                   ""
                   "(config-view w h s): config view, where "
                   "| - w is the width of the view          "
                   "| - h is the height of the view         "
                   "| - s is optional boolean (show-footage)"
                   ])

(defn add-help-slide [loaded-slides]
  (into [ {:pres [[(apply str (interpose "\n" help-content))]]
           :num-breaks 0
           :full-code ""}] loaded-slides))

(def presentation-state 
  (atom {:curr-slide-index 0
         :curr-break-limit 1
         :slides (add-help-slide [])}))

(def content-file-name "tutorial.txt")

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


(defn get-slide-content [index]
  (let  [{:keys [curr-break-limit slides]} @presentation-state]
    (if (< index 0) {:pres [["| \\r Index lower bound"]]
                     :num-breaks 0
                     :full-code ""}
      (get slides index {:pres [["| \\r Index upper bound"]]
                         :num-breaks 0
                         :full-code ""}))))

(defn last-side-index []
  (dec (count (:slides @presentation-state))))

(defn re []
  (let [{:keys [curr-slide-index slides curr-break-limit]} 
        @presentation-state]
    (show-slide 
      (:pres (get-slide-content curr-slide-index)) 
      (str curr-slide-index "\\" (last-side-index))    
      curr-break-limit)))

(defn g 
  ([slide-index] (g slide-index 1))
  ([slide-index curr-break-limit] 
   (if (and (< 0 slide-index) (<= slide-index (last-side-index)))
     (swap! presentation-state #(assoc % 
                                       :curr-slide-index slide-index
                                       :curr-break-limit curr-break-limit)))
   (re)))

(defn h [] (show-slide (:pres (get-slide-content 0)) "help" 1))

(defn f [] (g 1))

(defn l [] (g (last-side-index)))

(defn num-breaks [slide-index] 
 (:num-breaks (get-slide-content slide-index)))

(defn get-current-slide-idx-w-break-limit
  ([]
   (let [{:keys [curr-slide-index _ curr-break-limit]} @presentation-state] 
     [curr-slide-index curr-break-limit])))


(defn n
  ([] 
   (n 1))
  ([num-steps-to-go] 
   (n num-steps-to-go (get-current-slide-idx-w-break-limit))) 
  ([num-steps-to-go [slide-index curr-break-limit]]
   (let [next-break (+ curr-break-limit num-steps-to-go)
         last-side-index (last-side-index)
         break-limit (num-breaks slide-index)]
     (if (<= next-break (inc break-limit))
       (g slide-index next-break)
       (recur 
         (- num-steps-to-go 1 (- break-limit curr-break-limit)) 
         [(inc slide-index) 0])))))

(defn p
  ([] 
   (p 1))
  ([num-steps-to-go] 
   (p num-steps-to-go (get-current-slide-idx-w-break-limit))) 
  ([num-steps-to-go [slide-index curr-break-limit]]
   (let [next-break (- curr-break-limit num-steps-to-go)
         prev-break-limit (num-breaks (dec slide-index))]
     (if (>= next-break 1)
       (g slide-index next-break)
       (recur (- num-steps-to-go 1 curr-break-limit) 
              [(dec slide-index) prev-break-limit])))))

(defn config-view
  ([width height] (config-view width height true)) 
  ([width height show-footage]
    (reset! view-config {:view-width width 
                         :view-height height 
                         :show-footage show-footage})
    (re)))

(defn start 
  ([] (start content-file-name))
  ([content-file-name]
   (reset! 
     presentation-state 
     {
      :curr-slide-index 0 
      :curr-break-limit 1
      :slides (-> content-file-name 
                  parse-to-slides 
                  add-help-slide)})
   (n)))

