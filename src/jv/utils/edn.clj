
(ns jv.utils.edn
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io FileReader InputStreamReader PushbackReader File)))

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
