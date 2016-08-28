(require 'cljs.build.api)

(cljs.build.api/build "src"
 {:main 'condiments.core
  :output-to "main.js"
  :target :nodejs})
