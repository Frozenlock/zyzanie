(defproject zyzanie "0.2.5"
  :description "Emacs-like keybinding for your browser. (Clojurescript)"
  :source-path "src"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [siren "0.1.7"]
                 [enfocus "1.0.0-beta3"]
                 [domina "1.0.0"]]                
  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-1450"]
                                  [org.clojure/google-closure-library "0.0-1376-2"]
                                  [org.clojure/google-closure-library-third-party "0.0-1376-2"]]}}
  :plugins [[lein-cljsbuild "0.2.5"]]
  :cljsbuild {:builds [{:source-path "src"
                        :jar true
                        :compiler {:libs ["goog/dom/query.js"]
                                   :pretty-print true
                                   :output-dir ".cljsbuild/zyzanie"
                                   :output-to "public/zyzanie.js"}}]})

