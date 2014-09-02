(ns parjec.examples.css
  "A parser for a subset of the CSS2 specification
  (see http://www.w3.org/TR/CSS2/grammar.html)."
  (:require [parjec.core :refer :all]))

(defparser identifier
  "CSS identifier parser."
  [dash (optional (token \-))
   l    letter
   ls   (many (choice alphanumeric (token \-)))]

  (str dash l (apply str ls)))

(def element-name
  "CSS element name parser."
  (choice (string "*") identifier))

(def css-id
  "CSS ID parser."
  (prefix-by (token \#) identifier))

(def css-class
  "CSS class parser."
  (prefix-by (token \.) identifier))

(defparser without-element-name
  "Parse a CSS simple selector without an element name."
  [id (optional css-id)
   classes (many css-class)]
  
  {:id id :classes classes})

(defparser with-element-name
  "Parse a CSS simple selector with an element name."
  [element-name element-name
   rest         (optional without-element-name)]

  (assoc rest :element-name element-name))

(def simple-selector
  "CSS simple selector parser."
  (choice with-element-name without-element-name))

(defparser zero
  "Attempt to parse the character \0, returning the integer 0."
  [z (token \0)]
  0)

(defparser nonzero-integer
  "Positive integer parser."
  [dash         (optional (token \-))
   first-digit  (one-of "123456789")
   other-digits (many digit)]

  (read-string (str dash first-digit (apply str other-digits))))

(def integer
  "Integer parser."
  (choice zero nonzero-integer))

(def left-paren  (token \())
(def right-paren (token \)))

(defparser rgba
  "CSS rgba color value parser."
  [_      (string "rgba")
   values (between left-paren
                   (sep-by (prefix-by comma (many space))
                           integer)
                   right-paren)]

  values)

(def unit
  "CSS unit parser."
  (choice (string "cm")
          (string "em")
          (string "ex")
          (string "in")
          (string "mm")
          (string "pc")
          (string "pt")
          (string "px")))

(defparser css-float
  "CSS float parser."
  [dash            (optional (token \-))
   integral-part   integer
   _               (token \.)
   fractional-part (many1 digit)]

  (read-string (str dash integral-part \. (apply str fractional-part))))

(def number
  "CSS number parser."
  (choice css-float integer))

(defparser length
  "CSS length value parser."
  [n number
   u unit]

  [n u])

(defparser css-keyword
  "CSS keyword parser."
  [kw (many1 letter)]
  (apply str kw))

(def css-value
  "CSS value parser."
  (choice rgba length css-keyword))

(defparser declaration
  "CSS declaration parser."
  [property  (end-by (many space) identifier)
   _         (token \:)
   _         (skip-many space)
   value     css-value
   _         (token \;)]

  {:property property :value value})

(def left-brace  (token \{))
(def right-brace (token \}))

(defparser rule
  "CSS rule parser."
  [selectors    (sep-by (prefix-by comma (skip-many whitespace))
                        simple-selector)
   _            (skip-many whitespace)
   _            (prefix-by left-brace (skip-many whitespace))
   declarations (sep-by (many whitespace) declaration)
   _            (prefix-by (many whitespace) right-brace)]

  {:selectors selectors
   :declarations declarations})

(defparser stylesheet
  "CSS stylesheet parser."
  [rules (between (many whitespace)
                  (sep-by (many whitespace) rule)
                  (many whitespace))]
  
  {:rules rules})

