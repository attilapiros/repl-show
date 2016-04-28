(ns repl-show.parser_test (:gen-class)
  (:use clojure.test
        repl-show.parser)

  (:require [clojure.tools.trace :as trace]))

(def test-data 
  (apply str
         (interpose "\n"  ["slide1"
                    "---"
                    "slide2.build1.line1"
                    "slide2.build1.line2"
                    "--"
                    "slide2.build2"
                    "```"
                    "source1.line1"
                    "source1.line2"
                    "```"
                    "---"
                    "slide3"
                    "---"
                    "```"
                    "slide4.code"
                    "```"])))


(is (=  
     '((["slide1"]) 
       (["slide2.build1.line1\nslide2.build1.line2"] ["slide2.build2" "source1.line1\nsource1.line2"]) 
       (["slide3"]) ([nil "slide4.code"]))
      (slides test-data)))
