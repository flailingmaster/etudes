(def xml (js/require "node-xml-lite"))
(.parseFileSync xml "test.xml")
