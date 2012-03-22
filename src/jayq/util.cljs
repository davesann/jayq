(ns jayq.util)

(defn map->js [m]
  (let [out (js-obj)]
    (doseq [[k v] m]
      (aset out (name k) v))
    out))

(defn wait [ms func]
  (js* "setTimeout(~{func}, ~{ms})"))

(defn log [v & text]
  (let [vs (if (string? v)
             (apply str v text)
             v)]
    (. js/console (log vs))))

(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
   other ClojureScript colls into JavaScript arrays, and ClojureScript
   keywords into JavaScript strings."
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.-strobj (reduce (fn [m [k v]]
               (assoc m (clj->js k) (clj->js v))) {} x))
    (coll? x) (apply array (map clj->js x))
    :else x))
  

(defn mapkeys [f a-map]
  "apply f to the keys in the supplied map
   to produce a map {(f k1) v1 (f k2) v2 ...etc}"
  (into {} 
        (map (fn [[k v]] [(f k) v])
         a-map)))

