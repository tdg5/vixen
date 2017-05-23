(defproject vixen "1.0.0"
  :description "Manipulate DOM"
  :url "http://github.com/zensight/vixen"
  :plugins [[cider/cider-nrepl "0.12.0"]]
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [crouton "0.1.2"]
                 [medley "0.8.2"]])
