(ns repl-show.parser_test (:gen-class)
  (:use clojure.test
        repl-show.parser))

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

(deftest parsing-test
  (is (=  
        '((["slide1"]) 
          (["slide2.build1.line1\nslide2.build1.line2"] ["slide2.build2" "source1.line1\nsource1.line2"]) 
          (["slide3"]) ([nil "slide4.code"]))
        (slides test-data))))
