(require 'cljs.build.api)
(cljs.build.api/build "src"
	{:main 'etudes.core
	 :output-to "out/main.js"})
