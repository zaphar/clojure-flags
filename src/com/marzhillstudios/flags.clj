(ns com.marzhillstudios.flags
  (:import [java.lang Boolean Integer String]))

; TODO(jwall): unit tests
(def *flags* (ref {}))

(def *flag-vals* (ref {}))

(def *flag-defs* (ref {}))

(def *unparsed-args* (ref []))

(defn get-flags []
  "Get a map of all the flags parsed so far."
  @*flag-vals*)

(defn get-flag [name]
  "Get the value of a parsed flag."
  (get (get-flags) name))

(defn flag-type? [t nm]
  (= t (:type (get @*flag-defs* nm))))

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

(defn parse-flag-nm
  [flag]
  (re-find (re-pattern (str "^--((no)?(.+))+=?(.*)"))
           flag))

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
                   (cond (vector? prev-val) (conj val prev-val)
                         :else (vec [val prev-val]))))))

(defn- illegal-state [msg & args]
  (java.lang.IllegalStateException. (apply format msg args)))

(defn- has-eq [flag]
  (some #(= %1 \=) flag))

(defn flag? [flag]
  (= (take 2 flag) '(\- \-)))

(defn has-no? [flag]
  (= (take 2 flag) '(\n \o)))

(defn bool-flag? [flag]
  (let [name (get (parse-flag-nm flag) 3)]
    (flag-type? Boolean name)))

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

(defn parse [args]
  (let [parted (partition-by #(= "--" %1) args)
        to-parse (filter #(not (nil? %1)) (preprocess-flags [] (first parted)))
        no-parse (vec (drop 1 (apply concat (rest parted))))]
    (prn to-parse)
    (dosync (ref-set *flag-vals*
                     (reduce flags-reducer
                             {} (map parse-flag-to-vec to-parse)))
            (ref-set *unparsed-args* no-parse)))
  [(get-flags) (get-unparsed)])
