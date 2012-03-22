(ns jayq.core 
  (:refer-clojure :exclude [val empty remove])
  (:require [clojure.string :as string])
  (:use [jayq.util :only [map->js]]))

(defn crate-meta [func]
  (.-prototype._crateGroup func))

(def $ (fn [selector] 
         (let [selector (cond
                          (string? selector) selector
                          (fn? selector) (str "[crateGroup=" (crate-meta selector) "]")
                          (keyword? selector) (name selector)
                          :else selector)]
           (js/jQuery selector))))

(extend-type js/jQuery
  ISeqable
  (-seq [this] (when (.get this 0)
                 this))
  ISeq
  (-first [this] (.slice this 0 1))
  (-rest [this] (if (> (count this) 1)
                  (.slice this 1)
                  (list)))

  ICounted
  (-count [this] (. this (size)))

  IIndexed
  (-nth [this n]
    (when (< n (count this))
      (.slice this n (inc n))))
  (-nth [this n not-found]
    (if (< n (count this))
      (.slice this n (inc n))
      (if (undefined? not-found)
        nil
        not-found)))

  ISequential

  ILookup
  (-lookup
    ([this k]
       (or (.slice this k (inc k)) nil))
    ([this k not-found]
       (-nth this k not-found)))

  IReduce
  (-reduce [this f]
    (ci-reduce this f (first this) (count this)))
  (-reduce [this f start]
    (ci-reduce this f start i))) 

(set! jQuery.prototype.call
      (fn
        ([_ k] (-lookup (js* "this") k))
        ([_ k not-found] (-lookup (js* "this") k not-found))))

(defn anim [elem props dur]
  (.animate elem (map->js props) dur))

(defn text [$elem txt]
  (.text $elem txt))

(defn css [$elem opts]
  (if (keyword? opts)
    (.css $elem (name opts))
    (.css $elem (map->js opts))))

(defn attr [$elem a & [v]]
  (let [a (name a)]
    (if-not v
      (. $elem (attr a))
      (. $elem (attr a v)))))

(defn add-class [$elem cl]
  (let [cl (name cl)]
    (.addClass $elem cl)))

(defn remove-class [$elem cl]
  (let [cl (name cl)]
    (.removeClass $elem cl)))

(defn append [$elem content]
  (.append $elem content))

(defn prepend [$elem content]
  (.prepend $elem content))

(defn remove [$elem]
  (.remove $elem))

(defn hide [$elem & [speed on-finish]]
  (.hide $elem speed on-finish))

(defn show [$elem & [speed on-finish]]
  (.show $elem speed on-finish))

(defn fade-out [$elem & [speed on-finish]]
  (.fadeOut $elem speed on-finish))

(defn fade-in [$elem & [speed on-finish]]
  (.fadeIn $elem speed on-finish))

(defn slide-up [$elem & [speed on-finish]]
  (.slideUp $elem speed on-finish))

(defn slide-down [$elem & [speed on-finish]]
  (.slideDown $elem speed on-finish))

(defn bind [$elem ev func]
  (.bind $elem (name ev) func))

(defn bind-if 
  "Like bind but allows pred? to filter events
   In some cases this will make the handler simpler.
   The pred? does not have to rely only on the event
   e.g. \"enable\" a handler only when the app or DOM is in a particular state.
   returns f - the delegate handler - in case you really want to unbind this.
  "
  [pred? $elem ev func]
  (let [f (fn [event]
            (when (pred? event)
              (func event)))]
    (bind $elem (name ev) f)
    f))

(defn unbind [$elem ev func]
  (.unbind $elem (name ev) func))

(defn trigger 
  ([$elem ev]
    (.trigger $elem (name ev)))
  ([$elem ev data]
    (.trigger $elem (name ev) data)))

(defn async-trigger
  ([$elem ev] 
    (js/setTimeout #(trigger $elem ev)) 0)
  ([$elem ev {:keys [data timeout]}]
    (let [f (if-not data 
              #(trigger $elem ev)
              #(trigger $elem ev data)
              )
          t (or timeout 0)]
      (js/setTimeout f t))))

(defn delegate [$elem parent ev func]
  (.delegate $elem parent ev func))

(defn safe-name [i]
  (try
    (name i)
    (catch js/Error e i)
    ))

; if a map is passed func is ignored.
(defn on 
  [$elem events selector data func]
  (if (map? events)
    (let [m (map->js event-map)] 
      (.on $elem m selector data))
    (.on $elem (name events) (safe-name selector) data func)))
  
(defn namespace-events 
  "apply the supplied namespace to all events
    can handle keyword, list, map, string
    you should prefix your ns with '.' 
      (this is for simplicity 
         - all jquery namespace use effectively require you to add this anyway)
  "
  ([events] (namespace-events events (str "." (gensym))))
  ([events e-ns]
    (cond 
      (keyword? events)
      (str (name events) e-ns)
      
      (string? events) 
      (namespace-events (s/split events #"\s+") e-ns)
      
      (seq? events)
      (s/join " " (map #(str (name %) e-ns) events)) 
      
      (map? events)
      (umap/mapkeys
        #(str (name %) e-ns)
        events
        ))))

(defn on-ns 
  "Same as on but puts all events into the supplied namespace"
  [e-ns $elem events selector data func]
  (let [ns-events (namespace-events events e-ns)]
    (on $elem ns-events selector data func)))


(defn off [$elem events]
  (.off $elem events))

(defn inner [$elem v]
  (.html $elem v))

(defn empty [$elem]
  (.empty $elem))

(defn val [$elem & [v]]
  (if v
    (.val $elem v)
    (. $elem (val))))

(defn queue [$elem callback]
  (. $elem (queue callback)))

(defn dequeue [elem]
  (. ($ elem) (dequeue)))

(defn xhr [[method uri] content callback]
  (let [params (map->js {:type (string/upper-case (name method))
                         :data (map->js content)
                         :success callback})]
    (.ajax js/jQuery uri params)))

