(ns parjec.core
  (:use [clojure.algo.monads
         :only (domonad with-monad
                m-fmap m-plus m-result m-seq
                maybe-m
                state-t)]))

(def parse-m
  "TODO long explanation..."
  (state-t maybe-m))

(defn any
  "Return a parser that parses any token or returns nil."
  [[first-token & other-tokens :as stream]]
  (if (empty? stream)
    nil
    [first-token other-tokens]))

(defn token-test
  "Return a parser that succeeds when given a stream that starts with a token
  that satisfies the given predicate."
  [pred]
  (domonad parse-m
           [token any :when (pred token)]
           token))

(defn token
  "Return a parser for the given token."
  [t]
  (token-test (partial = t)))

(defn tokens
  "Return a parser for the given token-stream."
  [target]
  (if (empty? target)
    (with-monad parse-m
      (m-result ()))
    (domonad parse-m
             [first-token (token (first target))
              other-tokens (tokens (rest target))]
             (conj other-tokens first-token))))

; =============================================================================
;                                  Combinators
; =============================================================================

(with-monad parse-m
  (def choice
    "Return a parser that tries the given parsers in order and returns the
    result of the first one that works. If none work, return nil."
    m-plus)

  (def nothing
    "A parser that leaves the stream untouched and returns nil."
    (m-result nil)) 

  (defn prefix
    "Return a parser that will match the given prefix followed by the given
    pattern, returning the result of the pattern."
    [pre pattern]
    (m-bind pre (fn [_] pattern)))

  (defn all
    "Return a parser that executes the given parsers in order."
    [& parsers]
    (m-seq parsers)))

(defn optional
  "Return a parser that doesn't consume input and returns nil if the given
  parser fails."
  [parser]
  (choice parser nothing))

(def many1)
(defn many
  "Return a parser that will execute the given parser 0 or more times."
  [parser]
  (optional (many1 parser)))

(defn many1
  "Return a parser that will execute the given parser 1 or more times."
  [parser]
  (domonad parse-m
           [a parser
            as (many parser)]
           (conj as a)))

(defn one-of
  "Return a parser that will match any of the given tokens."
  [tokens]
  (token-test (partial contains? (into #{} tokens))))

(defn none-of
  "Return a parser that will match anything but the given tokens."
  [tokens]
  (token-test (fn [token]
                (not (contains? (into #{} tokens)
                                token)))))

(defn sep-by
  "Return a parser that will match any number of the given pattern
  separated by the given separator, returning all of the matched
  patterns."
  [separator pattern]
  (domonad parse-m
           [p pattern
            ps (many1 (prefix separator pattern))]
           (conj ps p)))

(defn between
  "Return a parser that will match `left` followed by `middle` followed by
  `right`, and return the result of `middle`."
  [left middle right]
  (domonad parse-m
           [_ left
            m middle
            _ right]
           m))

(defn tally
  "Return a parser that will match `pattern` `n` times, returning the results."
  [n pattern]
  (apply all (repeat n pattern)))

; =============================================================================
;                                Common helpers
; =============================================================================

(def lower-case
  "Match any lowercase letter [a-z]."
  (one-of "abcdefghijklmnopqrstuvwxyz"))

(def upper-case
  "Match any uppercase letter [A-Z]."
  (one-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def letter
  "Match any letter [a-zA-Z]."
  (choice lower-case upper-case))

(def digit
  "Match any digit [0-9]."
  (one-of "0123456789"))

(def hex-digit
  "Match any hexadecimal digit [0-9a-fA-F]."
  (choice digit (one-of "abcdefABCDEF")))

(def alphanumeric
  "Match any alphanumeric character [0-9a-zA-Z]."
  (choice digit letter))

(def comma
  "Match a comma character."
  (token \,))

(def carriage-return
  "Match a carriage return character."
  (token \return))

(def new-line
  "Match a newline character."
  (token \newline))

(def space
  "Match a space character."
  (token \space))

(def tab
  "Match a tab character."
  (token \tab))

(def whitespace
  "Match any whitespace character."
  (choice carriage-return new-line space tab))

