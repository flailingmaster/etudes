(require 'cljs.build.api)
(cljs.build.api/watch "src"
  {:main 'etudes.core
	 :output-to "out/main.js"})
