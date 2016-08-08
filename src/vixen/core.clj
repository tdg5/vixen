(ns vixen.core
  (:refer-clojure :exclude [remove find any])
  (:require [clojure.set :as s]
            [clojure.string :as string]
            [crouton.html :as crout]
            [medley.core :as medley]))

;;utility
(defn- matches? [known in]
  (boolean
   (cond
     (not (string? in)) false
     (string? known) (= known in)
     (keyword? known) (= (name known) in)
     :else (re-find known in))))

(defn- to-crouton [path]
  (if (empty? path)
    [:doc]
    (concat [:doc :content] (interpose :content path))))

(defn- node-at [vix path]
  (if (empty? path)
    [(:doc vix)]
    (do
      (get-in vix (to-crouton path)))))

(defn doit [{:keys [doc cursor] :as vix}]
  (reduce
   (fn [nodes sel]
     (->>
      nodes
      (mapcat (fn [[node path]] (sel node path vix)))
      (medley/distinct-by second)))
   [[doc []]]
   cursor))

;;compose
(defn or-fn [& fs]
  (fn [node path vix] (mapcat #(% node path vix) fs)))

(defn and-fn [& fs]
  (fn [node path vix]
    (let [sets (map #(set (% node path set)) fs)]
      (apply s/intersection sets))))

(defn expand [selector]
  (fn [node path vix]
    (concat
     [[node path]]
     (selector node path vix))))

;;the medley one doesn't do what i need
(defn dissoc-in [m [k & ks]]
  (cond
    (nil? k) {}
    (not (coll? m)) m
    :else (let [r (dissoc-in (get m k) ks)]
            (if (and (coll? r) (empty? r))
              (if (map? m)
                (dissoc m k)
                (vec (keep-indexed (fn [i v] (when-not (= i k) v)) m)))
              (assoc (if (seq? m) (vec m) m) k r)))))

(defn bool-check [pred]
  (fn [node path _]
    (if (pred node) [[node path]] [])))

;;create
(defn parse [source]
  {:doc (crout/parse source)
   :cursor []})

(defn parse-string [string]
  {:doc (crout/parse-string string)
   :cursor []})

(defn nth-child
  ([])
  ([selector]))

(defn child [node path vix]
  (if-let [content (:content node)]
    (map-indexed (fn [i child]
                   [child (conj path i) vix]) content)
    []))

(defn sub [selector]
  [child selector])

;;filters
(defn elem= [tag]
  (bool-check #(= (:tag %) (keyword tag))))

(defn attr= [attr value]
  (bool-check #(matches? value (get (:attrs %) (keyword attr)))))

(defn text? [node _ _]
  (bool-check (string? node)))

(defn text= [value]
  (bool-check #(matches? value %)))

(defn text= [value]
  (bool-check #(matches? value %)))

(defn meta= [key value]
  (bool-check #(= (get (meta %) key) value)))

(defn class= [class]
  (attr= :class (re-pattern (str "(\\s|^)" (name class) "(\\s|$)"))))

(defn any [node path _]
  [[node path]])

(defn parent [node path vix]
  (if (empty? path)
    []
    (let [new-path (butlast path)]
      [[(node-at vix new-path) new-path]])))

(defn- siblings* [pred?]
  (fn [node path vix]
    (let [l (last path)
          p (vec (butlast path))
          parent (node-at vix p)]

      (keep-indexed (fn [i n]
                      (when (pred? i l)
                        [n (conj p i)]))
                    (:content parent)))))

(def siblings (siblings* #(not= %1 %2)))
(def siblings-before (siblings* #(> %2 %1)))
(def siblings-after (siblings* #(< %2 %1)))

(defn search [selector]
  (fn [node path vix]
    (loop [found []
           [[n p] & others] [[node path]]]
      (if n
        (let [newly (selector n p vix)]
          (recur (if (empty? newly) found (concat found newly))
                 (if (empty? (:content n)
                             others
                             (concat others
                                     (map-indexed (fn [i child]
                                                    [child (conj p i)])
                                                  (:content n)))))))
        found))))

(defn path->selector [path]
  (fn [_ _ vix]
    [[(node-at vix path) path]]))

(defn- freeze [vix paths]
  (assoc vix
         :cursor
         [(apply or-fn (map path->selector paths))]))

(defn concrete [vix]
  (if (empty? (:cursor vix))
    vix
    (freeze (map second (doit vix)))))

;;output
(defn nodes [vix]
  (map first (doit vix)))

(defn paths [vix]
  (map second (doit vix)))

(defn to-text [vix])

(defn- escape [s]
  ;;stolen from hiccup
  (.. s
      (replace "&"  "&amp;")
      (replace "<"  "&lt;")
      (replace ">"  "&gt;")
      (replace "\"" "&quot;")
      (replace "'" "&apos;")))


(defn- to-html* [{:keys [tag attrs content] :as n}]
  (cond (nil? n) ""
        ;;(and (string? n) (string/blank? n)) ""
        (string? n) n
        :else (str "<"
                   (name tag)
                   (if (empty? attrs)
                     ""
                     (str " " (string/join " " (map (fn [[k v]] (str (name k) "='" (escape (name v)) "'")) attrs))))
                   ">"
                   (if content
                     (string/join (map to-html* content))
                     "")
                   "</" (name tag) ">")))

(defn to-html [vix]
  ;;i wish crouton provided this
  (map to-html* (nodes vix)))

;;navigate
(defn reset [vix]
  (assoc vix :cursor []))

(defn select [vix & selectors]
  (update vix :cursor (comp vec concat) (flatten selectors)))

(defn find [vix & selectors]
  (update vix :cursor (comp vec concat) (map search selectors)))

(defn xform [vix xform]
  (reduce (fn [v [_ path]]
            (update-in v (to-crouton path) xform))
          vix
          (doit vix)))

(defn remove [vix]
  (let [drop-node-at (fn [vix path]
                       (let [dissoc-path (to-crouton path)]
                         (dissoc-in vix dissoc-path)))
        ;;todo - this should take some shortcuts, just finding the first matching path
        next #(first (doit %))]

    (loop [whole-thing vix]
      (if-let [[node path] (next whole-thing)]
        (recur (drop-node-at whole-thing path))
        (reset whole-thing)))))

(defn xform [vix xform]
  (reduce (fn [v [_ path]]
            (update-in v (to-crouton path) xform))
          vix
          (doit vix)))

(defn set-attr [vix attr value]
  (xform vix #(if (map? %)
                (assoc-in % [:attrs attr] value)
                %)))

(defn set-meta [vix key value]
  (xform vix #(with-meta % {key value})))

(defn wrap [vix tag]
  (xform vix (fn [node] {:tag tag :content [node]})))
