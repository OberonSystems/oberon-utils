(ns oberon.utils
  (:require #?@(:cljs
                [[goog.string :as gstring]
                 [goog.string.format]])
            ;;
            [clojure.walk   :refer [postwalk]]
            [clojure.set    :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]))

;;; --------------------------------------------------------------------------------
;;  Threading

(defmacro cond-some->
  "Like cond-> but returns nil when initial expr is nil."
  {:style/indent 1}
  [expr & clauses]
  `(when-let [e# ~expr]
     (cond-> e# ~@clauses)))

(defmacro cond-some->>
  "Like cond->> but returns nil when initial expr is nil."
  {:style/indent 1}
  [expr & clauses]
  `(when-let [e# ~expr]
     (cond->> e# ~@clauses)))

;;;

(defn nil-when->     [arg pred] (if     (pred arg) nil arg))
(defn nil-when-not-> [arg pred] (if-not (pred arg) nil arg))

(defn nil-when->>     [pred arg] (if     (pred arg) nil arg))
(defn nil-when-not->> [pred arg] (if-not (pred arg) nil arg))

;;;

(defn dump->
  ([x] (pprint x) x)
  ([x message]
   (println message)
   (pprint x)
   x))

(defn dump->>
  ([x] (pprint x) x)
  ([message x]
   (println message)
   (pprint x)
   x))

;;; --------------------------------------------------------------------------------

(defn canonical-values
  [m & {:keys [purge?]}]
  ;; It is very common for values to be set to empty strings during
  ;; JSONification in a browser, so fields that might otherwise be
  ;; numbers, booleans, etc, can all become strings.  We trim and
  ;; canonicalise to nil here to just avoid all that mess.
  (postwalk #(cond
               (and purge? (map-entry? %)) (if (-> % second nil?)
                                             nil
                                             %)
               ;;
               (and (vector? %) (empty? %)) nil
               (and (list?   %) (empty? %)) nil
               (and (map?    %) (empty? %)) nil
               ;;
               (string? %) (when-not (s/blank? %)
                             (s/trim %))
               :else %)
            m))

;;; --------------------------------------------------------------------------------

(defn dissoc*
  [m ks]
  (if (empty? ks)
    m
    (apply dissoc m ks)))

;;; --------------------------------------------------------------------------------
;;  Maps

(defn map-entry
  [k v]
  (clojure.lang.MapEntry/create k v))

(defn hash-map*
  [& [head & tail :as params]]
  (let [[head tail] (if (-> params count odd?)
                      [head tail]
                      [{} params])]
    (->> tail
         (partition 2)
         (remove #(-> % second nil?))
         (map vec)
         (into head))))

(defn has-keys?
  [m ks]
  (every? #(contains? m %) ks))

;;; --------------------------------------------------------------------------------
;;  Strings

(defn strunc
  [s n]
  (subs s 0 (min (count s) n)))

(defn clean-string
  [s]
  (some-> s
          s/trim
          (nil-when-> s/blank?)))

(defn join-cleanly
  ([coll]       (join-cleanly " " coll))
  ([delim coll] (some->> coll
                         (map clean-string)
                         (remove nil?)
                         seq
                         (s/join delim))))

#?(:cljs
   (defn format
     "Formats a string using goog.string.format.
      e.g: (format \"Cost: %.2f\" 10.0234)"
     [fmt & args]
     (apply gstring/format fmt args)))

(defn capitalize-keyword
  [k]
  (some->> (some-> k name (s/split #"-"))
           (map #(-> % s/trim s/capitalize))
           (join-cleanly " ")))

;;; --------------------------------------------------------------------------------
;;  Char predicates.

(defn digit?
  [c]
  #?(:clj  (Character/isDigit c)
     :cljs (let [zero (.charCodeAt "0")
                 nine (.charCodeAt "9")]
             (<= zero (.charCodeAt c) nine))))

(defn non-alphas?
  [s]
  (some->> (re-seq #"[^a-zA-Z ]+" s)
           (s/join ", ")))

;;; --------------------------------------------------------------------------------

(defn prefix-keyword
  [prefix kw & {:keys [delim]
              :or {delim "."}}]
  (-> (str (name prefix) delim (name kw))
      keyword))

(defn map->nsmap
  [n m]
  (let [ns-name (if (keyword? n) (name n) n)]
    (reduce-kv (fn [acc k v]
                 (let [new-kw (if (and (keyword? k)
                                       (not (qualified-keyword? k)))
                                (keyword ns-name (name k))
                                k)]
                   (assoc acc new-kw v)))
               {} m)))

(defn nsmap->map
  [m]
  (reduce-kv (fn [acc k v]
               (let [new-kw (keyword (name k))]
                 (assoc acc new-kw v)))
             {} m))

(defn- keyword-ns
  ([s] (keyword s))
  ([s ns]
   (if (nil? ns)
     (keyword s)
     (keyword ns s))))

(defn join-keys
  [& ks]
  (when-let [k (some->> ks
                        (remove nil?)
                        seq
                        (map name)
                        (s/join "-"))]
    (keyword-ns k (-> ks first namespace))))

(let [-prefix-key (fn [k-str p-str ns]
                    (-> (str p-str k-str)
                        (keyword-ns ns)))]
  (defn prefix-key
    [k prefix & {:keys [ns]}]
    (let [k-str (name k)
          p-str (str (name prefix) "-")
          ns    (or (some-> ns name)
                    (namespace prefix)
                    (namespace k))]
      (-prefix-key k-str p-str ns)))

  (defn make-prefix-key
    [prefix & {:keys [ns]}]
    (let [p-str (str (name prefix) "-")
          ns    (or (some-> ns name)
                    (namespace prefix))]
      (if ns
        #(-prefix-key (name %) p-str ns)
        #(-prefix-key (name %) p-str (namespace %))))))

(let [-unprefix-key (fn [k-str p-str ns]
                      (when (and (s/starts-with? k-str p-str)
                                 (< (count p-str) (count k-str)))
                        (-> (subs k-str (count p-str) (count k-str))
                            (keyword-ns ns))))]
  (defn unprefix-key
    [k prefix & {:keys [ns]}]
    (let [k-str (name k)
          p-str (str (name prefix) "-")
          ns    (or (some-> ns name)
                    (namespace prefix)
                    (namespace k))]
      (-unprefix-key (name k) (str (name prefix) "-") ns)))

  (defn make-unprefix-key
    [prefix & {:keys [ns]}]
    (let [p-str (str (name prefix) "-")
          ns    (or (some-> ns name)
                    (namespace prefix))]
      (if ns
        #(-unprefix-key (name %) p-str ns)
        #(-unprefix-key (name %) p-str (namespace %))))))

(defn prefix-keys
  [coll prefix & {:keys [ns]}]
  (let [prefix-k (make-prefix-key prefix :ns ns)]
    (if (map? coll)
      (->> (map (fn [[k v]] [(prefix-k k) v]) coll)
           (into {}))
      ;; Assume it's a seq of keywords
      (map prefix-k coll))))

(defn unprefix-keys
  [coll prefix & {:keys [ns]}]
  (let [unprefix-k (make-unprefix-key prefix :ns ns)]
    (if (map? coll)
      (->> (map (fn [[k v]] [(unprefix-k k) v]) coll)
           (into {}))
      ;; Assume it's a seq of keywords
      (map unprefix-k coll))))

(defn nest-map
  [m parent-key & {:keys [ns nils?]}]
  (reduce-kv (fn [acc k v]
               (if-let [child-key (unprefix-key k parent-key :ns ns)]
                 (cond-> (dissoc acc k)
                   (or nils? (not (nil? v)))
                   (assoc-in [parent-key child-key] v))
                 (assoc acc k v)))
             {}
             m))

(defn unnest-map
  [m parent-key & {:keys [ns child-key renames]}]
  (let [child-key (or child-key parent-key)]
    (merge (dissoc m child-key)
           (prefix-keys (-> (get m child-key)
                            (rename-keys renames))
                        parent-key
                        :ns ns))))

;;; --------------------------------------------------------------------------------

(defn nest-keys
  "Selects the `ks` from the map `m` and assoc's them under the `k`,
  then dissocs the `ks` from `m`."
  [m k ks]
  (as-> m m
    (assoc  m k (select-keys m ks))
    (apply dissoc m ks)))
