(ns com.marzhillstudios.test-flags
  (:use clojure.test
        com.marzhillstudios.flags))

(defn set-up []
  (dosync (ref-set com.marzhillstudios.flags/*flags* {}))
  (defflag Boolean "foo")
  (defflag String "baz")
  (defflag Integer "bar"))

(deftest test-bool-flag?
  (set-up)
  (is (bool-flag? "--foo"))
  )

(deftest test-parse
  (set-up)
  (is (= (parse ["--nofoo"])
         [{"foo" false} []]))
  (is (= (parse ["--foo" ])
         [{"foo" true} []]))
  (is (= (parse ["--foo" "--bar=1" "--baz=razzle" "--" "cmd" "arg1"])
         [{"foo" true "bar" 1 "baz" "razzle"} ["cmd" "arg1"]]))
  (is (= (parse ["--nofoo" "--bar" "1" "--baz" "razzle" "--" "cmd" "arg1"])
         [{"foo" false "bar" 1 "baz" "razzle"} ["cmd" "arg1"]]))
  )

(deftest test--test-flag-pair
  (set-up)
  (is (= (test-flag-pair "--bar" "1")
         "--bar=1"))
  (is (= (test-flag-pair "--bar" "1")
         "--bar=1"))
  (is (nil? (test-flag-pair "--bar=1" "--foo")))
  (is (try (test-flag-pair "--bar" "--foo")
           "--bar=1"
           (catch IllegalStateException e
             true)
           (catch Exception e
             false)))
  (is (try (test-flag-pair "bar" "--foo")
           "--bar=1"
           (catch IllegalStateException e
             true)
           (catch Exception e
             false)))
  )
