(ns fwpd.core)
(def filename "suspects.csv")
(slurp filename)

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

;; NOTE:
;; Executes from the innermost expression
;; First the "\n" split, and then the "," split
(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(parse (slurp filename))

;; NOTE: the map within the reduce does not run on each iteration
;; similar to above this is evaluated first and the passed to the reduce function
;; evaluates to what is passed to reduce
;; unlike JS the sequence we are reducing is the last argument
;; DOES NOT EXIST WITHIN THE REDUCE LOOP
(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(def parsed (mapify (parse (slurp filename))))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn named
  [glitter-filtered]
  (reduce (fn [named-list {:keys [name]}] (conj named-list name)), '(), glitter-filtered))

(defn names
  [records]
  (map :name records))

(def validations {:name string? :glitter-index integer?})

(defn validate?
  [rules record]
  (every? (fn [[key validator-fn]]
            (validator-fn (key record))) rules))

(defn append
  [records record]
  (if (validate? validations record)
    (conj records record)
    (do (println "invalid keys")
        records)))

(defn map-str
  [{:keys [name glitter-index]}]
  (str name "," glitter-index))

(defn str->csv
  [strings]
  (clojure.string/join "\n" (map map-str strings)))

(map-str (first parsed))

(str->csv parsed)

;; ({:name "Edward Cullen", :glitter-index 10}
;;  {:name "Bella Swan", :glitter-index 0}
;;  {:name "Charlie Swan", :glitter-index 0}
;;  {:name "Jacob Black", :glitter-index 3}
;;  {:name "Carlisle Cullen", :glitter-index 6})


(def threaded 
  (
    ->> filename
    slurp
    parse
    mapify
    (glitter-index 3)
    str->csv))

threaded
