Clojure Flags
=============

Usage:
------

A commandline flag parsing library.

clojure-flags allows you to:

1. Specify the flags anywhere in your code.
2. Self Document flags
3. Type checking flags.


   (require '[com.marzhillstudios.flags :as flags])
  
   (defflag :string "string-flag"
            "Documentation about the flag")
   (defflag :bool "bool-flag"
            "Documentation about the bool flag")
  
   (defn -main [& args]
    (flags/parse args) ; parse the command line arguments
    (do
      (let [string-flag-val (get-flag "string-flag")] ; retrieve the flag value
        (some-fun (flags/unparsed-args)))))


You can define your own flag type handlers using the defflag-type function:


    (defflag-type :type-specifier (fn [matched] (last %1)))


The function will get one argument which will be the vector returned
from parsing the flag the last item in the vector will be the value, the
third item will be the flags name. You will mostly only care about the third.

Leiningen
---------

clojure-flags in hosted on clojars. To use it in your project just: include
this [clojure-flags "0.01.0"] in your projects :dependencies list.
