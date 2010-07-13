(ns com.marzhillstudios.flags
  (:import [java.lang Boolean Integer String]))

(def #^{:doc "Global flag variable ref."} *flags* (ref {}))

(defn parse-on-off
  [value]
  (not (re-find (re-matcher #"^no" value))))

(defn parse-typed-value-for-flag
  [type matched]
  (prn (format "type is: %s" type))
  (prn (format "matched is: %s" matched))
  (let [nm (matched 1)
        value (last matched)]
  (cond (nil? matched) nil
        (isa? type Integer) {nm (Integer/parseInt value)}
        (isa? type Boolean) {nm (parse-on-off nm)}
        (isa? type String) {nm value})))

(defn parse-flag-components
  [nm arg]
  (re-find (re-pattern (str "^--((no)?" nm ")+=?(.*)"))
           arg))

(defn defflag
  ([type nm] (defflag type nm ""))
  ([type nm doc]
     (fn [arg] (parse-typed-value-for-flag type
                  (parse-flag-components nm arg)))))

(defn parse-arg-to-map
  [arg] nil)

(defn parse [args]
  (reduce #() {} (map parse-arg-to-map args)))
