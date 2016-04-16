(defproject repl-show "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
		  [org.clojure/tools.trace "0.7.9"]
                  [lein-cljfmt "0.4.1"]
		  [venantius/glow "0.1.3"]]
  :main ^:skip-aot repl-show.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
