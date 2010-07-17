; This software Copyright (C) 2010 Jeremy Wall <jeremy@marzhillstudios.com>
; and is available under the terms of the Artistic License 2.0
; A copy of this license should be included with the source but if
; it is not you can view a copy of the license here:
; http://www.opensource.org/licenses/artistic-license-2.0.php
(ns com.marzhillstudios.flags
  "Command Line flags parsing library.

You can define flags anywhere in your code base. The flags are typed and can have an explanatory doc string. A parser method is provided that will parse the flags passed on the command line.

Example code:
(defflag :string \"path\" \"A filesystem path\")
(defflag :bool \"do-this\" \"A boolean flag\")

(parse args)

(let [path (get-flag \"path\")
      do-this? (get-flag \"do-this\")]
  ; code goes here
  )

Example command line:
$> path/to/app --path=/some/file/path --(no)do-this -- unparsed-arg
"

  (:require [clojure.contrib.string :as s]))

(def *flags* (ref {}))

(def *flag-vals* (ref {}))

(def *flag-defs* (ref {}))

(def *unparsed-args* (ref []))

(defn get-flags []
  "Get a map of all the flags parsed so far."
  @*flag-vals*)

(defn get-flag-defs
  [] @*flag-defs*)

(defn get-flag-def
  [nm] (get (get-flag-defs) nm))

(defn get-flag-desc [nm] 
  (let [flag-def (get-flag-def nm)]
    (str "--" nm "\t" (:type flag-def) "\t" (:doc flag-def))))

(defn get-flag [name]
  "Get the value of a parsed flag."
  (get (get-flags) name))

(defn flag-type? [t nm]
  (= t (:type (get-flag-def nm))))

(defn get-unparsed []
  @*unparsed-args*)

(defn- parse-on-off
  [value]
  (not (re-find (re-matcher #"^no" value))))

; TODO(jwall): pluggable flag type parsers
(def *type-parsers* (ref {}))

(defn defflag-type
  [type f]
  (dosync (ref-set *type-parsers* (assoc @*type-parsers* type f))))

(defflag-type :int #(Integer/parseInt (last %1)))
(defflag-type :bool #(parse-on-off (get %1 1)))
(defflag-type :string #(last %1))

(defn- parse-typed-value-for-flag
  [type matched]
  (cond (nil? matched) nil
        :else [(matched 3) ((get @*type-parsers*  type) matched)]))

(defn parse-flag-components
  [nm arg]
  (re-find (re-pattern (str "^--((no)?(" nm "))+=?(.*)"))
           arg))

(def flag-nm-re #"^--((no)?(.+))+=?(.*)")

(defn parse-flag-nm
  [flag]
  (get (re-find flag-nm-re flag) 3))

(defn defflag
  ([t nm] (defflag t nm ""))
  ([t nm doc]
     (assert (nil? (get @*flags* nm)))
     (dosync (ref-set *flag-defs* (assoc @*flag-defs* nm {:type t :doc doc}))
             (ref-set *flags*
              (assoc @*flags* nm
                (fn [arg] (parse-typed-value-for-flag
                           t (parse-flag-components nm arg))))))))

(defn- accumulate-parsed-val
  [arg acc parser]
  (let [parsed (parser arg)]
    (cond (nil? parsed) (or acc nil)
          :else parsed)))

(defn parse-flag-to-vec
  ([arg] (parse-flag-to-vec arg (vals @*flags*)))
  ([arg parsers]
     (let [parsed (reduce (partial accumulate-parsed-val arg) nil parsers)]
       (cond (nil? parsed) (throw (java.lang.IllegalStateException.
                                   (format "Unknown flag %s" arg)))
             :else parsed))))

(defn- flags-reducer
  [acc [key val]]
  (let [prev-val (get acc key)]
    (cond (nil? prev-val)
            (assoc acc key val)
          :else
            (assoc acc key
                   (cond (vector? prev-val) (concat prev-val [val])
                         :else (vec [prev-val val]))))))

(defn- illegal-state [msg & args]
  (java.lang.IllegalStateException. (apply format msg args)))

(defn- has-eq [flag]
  (some #(= %1 \=) flag))

(defn flag? [flag]
  (= (take 2 flag) '(\- \-)))

(defn has-no? [flag]
  (= (take 2 flag) '(\n \o)))

(defn bool-flag? [flag]
  (let [name (parse-flag-nm flag)]
    (flag-type? :bool name)))

(defn test-flag-pair
  [flag1 flag2]
  (if (flag? flag1)
    (if (has-eq flag1) nil
      ; no equal sign in flag1
      (if (not (bool-flag? flag1))
        ; flag1 is not boolean
        (if (not (flag? flag2))
          ; flag2 is not a flag so it's our value
          (str flag1 "=" flag2)
          ; flag2 is our flag so flag1 has no value
          (throw (illegal-state "flag %s has no value" flag1)))
        ; flag1 is boolean without an equal sign so its all good
        nil))
    ; flag1 is not a flag so we have an unknown flag parse error
    (throw (illegal-state "encountered unknown flag %s" flag1))))

(defn- preprocess-flags
  [acc flags]
  (let [f1 (first flags)
        f2 (second flags)
        nxt (drop 2 flags)]
    (cond (and f1 f2)
          (let [fjoined? (test-flag-pair f1 f2)]
            (if fjoined? (recur (conj acc fjoined?) nxt)
                (recur (conj acc f1) (cons f2 nxt))))
          :else (conj acc f1))))

(defn flags-help-string
  "Builds a string with all the flags descriptions."
  []
  (format "Flag descriptions: \n%s"
          (s/join "\n"
                  (map get-flag-desc (keys (get-flag-defs))))))

(defn print-help
  ([] (prn (flags-help-string)))
  ([msg] (prn (str msg "\n\n"))
     (print-help)))

(defn parse
  "Parse command line flags looking for defined flags"
  [args]
  (let [parted (partition-by #(= "--" %1) args)
        to-parse (filter #(not (nil? %1)) (preprocess-flags [] (first parted)))
        no-parse (vec (drop 1 (apply concat (rest parted))))]
    (prn to-parse)
    (dosync (ref-set *flag-vals*
                     (reduce flags-reducer
                             {} (map parse-flag-to-vec to-parse)))
            (ref-set *unparsed-args* no-parse)))
  [(get-flags) (get-unparsed)])
