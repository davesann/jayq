(ns jayq.core
  (:refer-clojure :exclude [val empty remove find])
  (:require 
    ;[dsann.utils.x.core :as u]

    [clojure.string :as string]
    [cljs.reader :as reader]
    )
  (:use [jayq.util :only [clj->js mapkeys]]))

(defn crate-meta [func]
  (.-prototype._crateGroup func))

(defn ->selector [sel]
  (cond
    (string? sel) sel
    (fn? sel) (if-let [cm (crate-meta sel)]
                (str "[crateGroup=" cm "]")
                sel)
    (keyword? sel) (name sel)
    :else sel))

(def $ (fn [sel & [context]]
         (if-not context
           (js/jQuery (->selector sel))
           (js/jQuery (->selector sel) context))))

(extend-type js/jQuery
  ISeqable
  (-seq [this] (when (.get this 0)
                 this))
  ISeq
  (-first [this] (.get this 0))
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
    (ci-reduce this f))
  (-reduce [this f start]
    (ci-reduce this f start))) 

(set! jQuery.prototype.call
      (fn
        ([_ k] (-lookup (js* "this") k))
        ([_ k not-found] (-lookup (js* "this") k not-found))))

(defn anim [elem props dur]
  (.animate elem (clj->js props) dur))

(defn text [$elem txt]
  (.text $elem txt))

(defn css [$elem opts]
  (if (keyword? opts)
    (.css $elem (name opts))
    (.css $elem (clj->js opts))))

(defn attr [$elem a & [v]]
  (let [a (name a)]
    (if-not v
      (. $elem (attr a))
      (. $elem (attr a v)))))

(defn data [$elem k & [v]]
  (let [k (name k)]
    (if-not v
      (. $elem (data k))
      (. $elem (data k v)))))

(defn cljs-data [$elem k & [v]]
  (let [k (str "data-" (name k))]
    (if-not v
      (reader/read-string (. $elem (attr k)))
      (. $elem (attr k v)))))

(defn position [$elem]
  (js->clj (.position $elem) :keywordize-keys true))

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

(defn toggle [$elem & [speed on-finish]]
  (.toggle $elem speed on-finish))

(defn fade-toggle [$elem & [dur on-finish]]
  (.fadeToggle $elem dur on-finish))

(defn fade-out [$elem & [speed on-finish]]
  (.fadeOut $elem speed on-finish))

(defn fade-in [$elem & [speed on-finish]]
  (.fadeIn $elem speed on-finish))

(defn slide-toggle [$elem & [dur on-finish]]
  (.slideToggle $elem dur on-finish))

(defn slide-up [$elem & [speed on-finish]]
  (.slideUp $elem speed on-finish))

(defn slide-down [$elem & [speed on-finish]]
  (.slideDown $elem speed on-finish))

(defn parent [$elem]
  (.parent $elem))

(defn find [$elem selector]
  (.find $elem (name selector)))

(defn clone [$elem]
  (.clone $elem))

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

(defn document-ready [func]
  (.ready ($ js/document) func))

(defn xhr [[method uri] content callback]
  (let [params (clj->js {:type (string/upper-case (name method))
                         :data (clj->js content)
                         :success callback})]
    (.ajax js/jQuery uri params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bind [$elem ev func]
  (.bind $elem (name ev) func))

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


(defn trigger [$elem ev]
  (.trigger $elem (name ev)))

(defn delegate [$elem sel ev func]
  (.delegate $elem (->selector sel) (name ev) func))

(defn ->event [e]
  (cond
    (keyword? e) (name e)
    (string? e) e
    (map? e) (clj->js e)
    (coll? e) (string/join " " (map name e))
    
    :else (throw (js/Error. (str "Unknown event type: " e)))))

(defn on [$elem events & [sel data handler]]
  (.on $elem
       (->event events)
       (->selector sel)
       data
       handler))

(defn one [$elem events & [sel data handler]]
  (.one $elem
        (->event events)
        (->selector sel)
        data
        handler))

(defn off [$elem events & [sel handler]]
  (.off $elem
        (->event events)
        (->selector sel)
        handler))

(defn prevent [e]
  (.preventDefault e))

(defn namespace-events- 
  "expected e-ns to be str of form \".namespace\""
  [events e-ns]
  (cond 
    (keyword? events)
    (str (name events) e-ns)
    
    (string? events) 
    (namespace-events (s/split events #"\s+") e-ns)
    
    (seq? events)
    (s/join " " (map #(str (name %) e-ns) events)) 
    
    (map? events)
    (mapkeys
      #(str (name %) e-ns)
      events
      )))

(defn namespace-events 
  "apply the supplied namespace to all events
    can handle events defined as keyword, list, map, string
  "
  ([events] (namespace-events events (gensym)))
  ([events e-ns]
    (let [e-ns-prefixed (str "." (name e-ns))]
      (namespace-events- events e-ns-prefixed)
      )))

(defn on-ns 
  "Same as on but puts all events into the supplied namespace"
  [e-ns $elem events & [sel data handler]]
  (let [ns-events (namespace-events events e-ns)]
    (on $elem ns-events sel data handler)))

(defn off-ns 
  [$elem e-ns] (off $elem (str "." (name e-ns))))

;; not sure if these are useful or not
(defn bind-if 
  "Like bind but allows pred? to filter events
   In some cases this will make the handler simpler.
   The pred? does not have to rely only on the event. 
     DOM state or atoms captured in the scope can also be used
   e.g. \"enable\" a handler only when the app or DOM is in a particular state.
   returns f - the delegate handler - in case you really want to unbind this.
  "
  [pred? $elem ev func]
  (let [f (fn [event] (when (pred? event) (func event)))]
    (bind $elem (name ev) f)
    f))

(defn on-if 
  "Like on but allows pred? to filter events
   In some cases this will make the handler simpler.
   The pred? does not have to rely only on the event. 
     DOM state or atoms captured in the scope can also be used
   e.g. \"enable\" a handler only when the app or DOM is in a particular state.
   returns f - the delegate handler - in case you really want to unbind this.
  "
  [pred? $elem events & [sel data handler]]
  (let [f (fn [event] (when (pred? event) (func event)))]
    (on $elem events sel data f)
    f))


