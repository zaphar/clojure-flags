(ns com.marzhillstudios.flags
  (:import [java.lang Boolean Integer String]))

; TODO(jwall): unit tests
(def *flags* (ref {}))

(def *flag-vals* (ref {}))

(def *unparsed-args* (ref []))

(defn get-flags []
  "Get a map of all the flags parsed so far."
  @*flag-vals*)

(defn get-flag [name]
  "Get the value of a parsed flag."
  (get (get-flags) name))

(defn get-unparsed []
  @*unparsed-args*)

(defn- parse-on-off
  [value]
  (not (re-find (re-matcher #"^no" value))))

(defn- parse-typed-value-for-flag
  [type matched]
  (cond (nil? matched) nil
        (isa? type Integer) [(matched 3) (Integer/parseInt (last matched))]
        (isa? type Boolean) [(matched 3) (parse-on-off (matched 1))]
        (isa? type String) [(matched 3) (last matched)]))

(defn parse-flag-components
  [nm arg]
  (re-find (re-pattern (str "^--((no)?(" nm "))+=?(.*)"))
           arg))

(defn defflag
  ([type nm] (defflag type nm ""))
  ([type nm doc]
     (assert (nil? (get @*flags* nm)))
     (dosync (ref-set *flags*
              (assoc @*flags* nm
                (fn [arg] (parse-typed-value-for-flag
                           type (parse-flag-components nm arg))))))))

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
                   (cond (vector? prev-val) (conj val prev-val)
                         :else (vec [val prev-val]))))))

(defn parse [args]
  (let [parted (partition-by #(= "--" %1) args)
        to-parse (first parted)
        no-parse (vec (drop 1 (apply concat (rest parted))))]
    (dosync (ref-set *flag-vals*
                     (reduce flags-reducer
                             {} (map parse-flag-to-vec to-parse)))
            (ref-set *unparsed-args* no-parse)))
  [(get-flags) (get-unparsed)])
