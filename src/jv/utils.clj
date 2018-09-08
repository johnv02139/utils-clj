
(ns jv.utils
  (:require [clojure.string :refer [lower-case split replace]
             :rename {replace str-replace}]
            [clojure.edn :as edn]
            [clojure.xml :as xml]
            [clojure.pprint :as pp]
            [clojure.java.io :as io])
  (:import (java.io FileReader InputStreamReader PushbackReader
                    File ByteArrayInputStream)))

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

(defn- read-edn-from-pushback-reader
  [edn-reader]
  (with-open [r edn-reader]
    (try
      (let [data (edn/read {:eof ::eof} r)]
        (if (= ::eof data)
          {:data nil
           :error "Encountered end of file"}
          {:data data
           :error nil}))
      (catch java.lang.Throwable th
        {:data nil 
         :error (.getMessage th)}))))
              
(defn read-edn-resource-file
  ([resource-file-specification default-value]
   (let [edn-resource (io/resource resource-file-specification)
         edn-reader (if edn-resource
                      (PushbackReader. (InputStreamReader. (.openStream edn-resource)))
                      (let [edn-file (File. resource-file-specification)]
                        (if (.exists edn-file)
                          (PushbackReader. (FileReader. edn-file))
                          (println "EDN config file not found at"
                                   resource-file-specification))))
         edn-data (read-edn-from-pushback-reader edn-reader)]
     (or (:data edn-data)
         (do
           (println "Error while reading EDN from"
                    resource-file-specification
                    (if edn-resource
                      (str " (" edn-resource ")")
                      "")
                    ":"
                    (:error edn-data))
           default-value))))
  ([resource-file-specification]
   ;; most EDN data are maps; make that the default
   (read-edn-resource-file resource-file-specification {})))



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
