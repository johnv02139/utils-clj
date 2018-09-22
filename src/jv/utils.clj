
(ns jv.utils
  (:require [clojure.string :refer [lower-case split replace]
             :rename {replace str-replace}]
            [clojure.xml :as xml]
            [clojure.pprint :as pp]
            [clojure.java.io :as io])
  (:import (java.io ByteArrayInputStream)))


;; repl debugging utils

(defmacro dvar [& v]
  `(do ~@(map #(list 'printf (str % " = %s\n") %) v)))

(defn print-lines [coll]
  (doseq [x coll] (print (prn-str x))))

(defn pp
  "Pretty-print the argument."
  [x]
  (pp/pprint x))

;; debugging from code

(def debug-level (atom 1))

(defn debug [& rest]
  (when (> @debug-level 1)
    (apply printf rest)))

(defn fail [& rest]
  (apply printf rest)
  false)

(defn nilmsg [& rest]
  (apply printf rest)
  nil)

;; general (Clojure) utils

(defmacro vcond [& forms]
  (if forms
    (if (every? vector? forms)
      (let [clause# (first forms)
            alternatives# (next forms)]
        `(if ~(first clause#)
           (do ~@(rest clause#))
           ~(if alternatives#
              `(vcond ~@alternatives#))))
      (println "Error: all clauses to vcond must be vectors!!"))))


;; The clojure.core function contains? is misleadingly named and
;; I feel it should be deprecated and not used in general code.
;; Put a wrapper around it here.  (TODO?)  Could use a macro or
;; some other sort of wrapper.
(defn contains-key? [coll key]
  (contains? coll key))

(defn divide-at
  "Uses the predicate to divide the collection into two parts, in the
   following way:

   - if the first element does not satisfy the predicate, returns an
     empty sequence as the first part, and the given collection as the
     second part
   - if the first element does satisfy the predicate, takes it; then,
     continues taking elements as long as they do NOT satisfy the
     predicate.  Returns those up to (but not including) the second one
     to satisfy the predicate as the first part, and the remainder of
     the collection, from the second match through the end of the
     collection, as the second part.

     The idea is that the predicate detects a 'beginning'.  We expect that
     the first element will match, and we want to continue until we see
     another beginning.  We assume that the end of the current part is
     whatever immediately precedes the beginning of the next part.

     This function is not lazy, and will always return a realized value
     for the first part.  (If given a lazy sequence, though, it may
     well return one as the second part.)"
  [pred coll]
  (if (pred (first coll))
    (loop [taken (list (first coll))
           [next-elt & untested :as dropped] (rest coll)]
      (if (or (empty? dropped) (pred next-elt))
        [taken dropped]
        (recur (concat taken (list next-elt))
               untested)))
    [() coll]))

(defn string-to-int [s]
  (try
    (Integer/parseInt s)
    (catch Throwable th
      nil)))

(defn make-element-set [& stuff]
  (->> stuff
       flatten
       (remove nil?)
       set))

;; se-map == "side-effect map"; basically just a way to do doseq
;; with a syntax like map.  Not lazy, and doesn't collect values.
;; Returns nil.
(defn se-map [f coll]
  (loop [[e1 & erest] coll]
    (f e1)
    (when erest
      (recur erest))))

(defn by-frequency [coll]
  (sort-by val > (frequencies coll)))

(defn flatten-singleton [s]
  (if (and (sequential? s)
           (not (empty? s))
           (empty? (rest s)))
    (first s)
    s))

(defn char-range [from to]
  (map char (range (int from) (inc (int to)))))

(defn char-range-set [& ranges]
  (set (mapcat #(apply char-range %) ranges)))

(defn read-file-lines [fname]
  (with-open [rdr (io/reader fname)] (doall (line-seq rdr))))


;; string utils

(defn simple-split [phrase]
  (split phrase #"[._ -]"))

(defn parse-xml-string [s]
  (xml/parse (ByteArrayInputStream. (.getBytes s))))

(defn parse-xml-file [f]
  (xml/parse f))

(defn clj-keyword [k]
  (-> (name k)
      (.replaceAll "_" "-")
      (.replaceAll "([A-Z]+)([A-Z][a-z])" "$1-$2")
      (.replaceAll "([a-z\\d])([A-Z])" "$1-$2")
      lower-case
      keyword))

(defn literalize-for-regex [s]
  (str-replace s "\\" "\\\\"))

(defn limit-length [strng max-line-len indent]
  (if (< (count strng) max-line-len)
    strng
    (loop [n max-line-len]
      (if (= \space (.charAt strng n))
        (str
         (subs strng 0 n)
         "\n"
         (apply str (repeat indent \space))
         (subs strng n))
        (if (pos? n)
          (recur (dec n))
          strng)))))

;; environment

(defn homedir []
  (let [env-java (System/getProperty "user.home")]
    (if (not-empty env-java)
      env-java
      (System/getenv "HOME"))))
