(ns condiments.core
  (:require [cljs.nodejs :as nodejs]))
(nodejs/enable-util-print!) (defn -main [& args]
      (println "It works!"))
    (set! *main-cli-fn* -main)

(def xml (js/require "node-xml-lite"))
(.parseFileSync xml "test.xml")
