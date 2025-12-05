(ns uix.css.lib
  (:require [clojure.string :as str]))

(def unitless-prop
  #{:animation-iteration-count
    :border-image-outset
    :border-image-slice
    :border-image-width
    :box-flex
    :box-flex-group
    :box-ordinal-group
    :column-count
    :columns
    :flex
    :flex-grow
    :flex-positive
    :flex-shrink
    :flex-negative
    :flex-order
    :grid-area
    :grid-row
    :grid-row-end
    :grid-row-span
    :grid-row-start
    :grid-column
    :grid-column-end
    :grid-column-span
    :grid-column-start
    :font-weight
    :line-clamp
    :line-height
    :opacity
    :order
    :orphans
    :tab-size
    :widows
    :z-index
    :zoom
    ;; SVG-related properties
    :fill-opacity
    :flood-opacity
    :stop-opacity
    :stroke-dasharray
    :stroke-dashoffset
    :stroke-miterlimit
    :stroke-opacity
    :stroke-width})

(defn interpret-value [k v]
  (if (and (number? v) (not (unitless-prop k)))
    (str v "px")
    v))

#?(:clj
   (do
     (declare class-names)

     (defn class-names-coll [classes]
       (let [classes (reduce (fn [a c]
                               (if c
                                 (->> (if (keyword? c) (name c) (class-names c))
                                      (conj a))
                                 a))
                             []
                             classes)]
         (when (pos? (count classes))
           (str/join " " classes))))

     (defn class-names
       "Merges a collection of class names into a string"
       ([a]
        (cond
          (coll? a) (class-names-coll a)
          (keyword? a) (name a)
          :else a))
       ([a b]
        (if a
          (if b
            (str (class-names a) " " (class-names b))
            (class-names a))
          (class-names b)))
       ([a b & rst]
        (reduce class-names (class-names a b) rst)))))