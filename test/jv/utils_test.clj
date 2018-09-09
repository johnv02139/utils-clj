(ns jv.utils-test
  (:require [clojure.test :refer [deftest testing is]]
            [jv.utils :refer [char-range flatten-singleton literalize-for-regex
                              divide-at]]))

(deftest divide-at-test
  (testing "divide-at"
    (is (= [() [:a]]
           (divide-at
            (constantly false)
            [:a])))
    (is (= [() [:a :b :c :d]]
           (divide-at
            (constantly false)
            [:a :b :c :d])))
    (is (= [[:a :b :c :d] [:a :b :c :d]]
           (divide-at
            #{:a}
            [:a :b :c :d :a :b :c :d])))
    (is (= [[:a :b :c] [:a]]
           (divide-at
            #{:a}
            [:a :b :c :a])))
    ;; divide-at is not lazy, itself; it always immediately searches for
    ;; the point at which to divide the collection.  However, it can still
    ;; be given a lazy (even infinite) sequence, and will return a lazy
    ;; sequence as the second value.
    (is (= '(0 1)
           (let [result (divide-at even? (range))]
             (first result))))
    ))

(deftest flatten-singleton-test
  (testing "flatten-singleton"
    (is (= "foo" (flatten-singleton ["foo"])))
    (is (= "foo" (flatten-singleton "foo")))
    (is (= [] (flatten-singleton [])))
    (is (= ["foo" "bar"] (flatten-singleton ["foo" "bar"])))
        ;; For this test, the equality is not really the point.
        ;; It verifies that the function does not cause an infinite
        ;; evaluation.  Of course, if it does, the test won't report
        ;; failure, it will just run until you kill it.  But at least
        ;; it won't report success.
    (is (= 0 (first (flatten-singleton (range)))))
    ))

(deftest char-range-test
  (testing "char-range"
    (is (= (char-range \a \z)
           '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z)))))

(deftest literal-for-regex-test
  (testing "literalize-for-regex"
    ;; Our test string is a 7 character string, 'foo\bar'; in order to pass that to Clojure,
    ;; we need to escape the backslash
    (let [raw-string "foo\\bar"
          ;; Search string is some junk on either side of what we're looking for.
          search-string (str "x - " raw-string " - y")
          test-fn (fn [f] (re-find (f raw-string) search-string))]
      ;; We verify that the Clojure function, re-pattern, fails for what we want it to do
      (is (nil? (test-fn re-pattern)))
      ;; Now that we see re-pattern isn't good enough, try using literalize-for-regex
      (is (= raw-string (test-fn #(re-pattern (literalize-for-regex %))))))))
