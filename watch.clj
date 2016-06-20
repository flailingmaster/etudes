(require 'clj.build.api)
(cljs.build.api/watch "src"
  {:main 'sample-project.core
	 :output-to "out/main.js"})
