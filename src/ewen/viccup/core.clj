(ns ewen.viccup.core
  (:require [cljs.analyzer.api :as ana-api])
  (:use ewen.viccup.util)
  (:import [clojure.lang IPersistentVector ISeq Named]))

(defn analyze [env x]
  (let [ana-x (ana-api/analyze env x)
        form (:form ana-x)
        new-env (:env ana-x)]
    (cond
      (seq? form)
      (#(doall (map %1 %2))
       (partial analyze new-env) form)
      :else x)))

(defn ast-show-only [ast keys]
  (assoc (select-keys ast keys)
         :children (mapv #(ast-show-only % keys) (:children ast))))

(defmacro mm [x]
  `(quote ~(analyze &env x)))

(def ^{:doc "Regular expression that parses a CSS-style id and class from an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(defn- unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(defmulti compile-form
  "Pre-compile certain standard forms, where possible."
  {:private true}
  form-name)

#_(defmethod compile-form "for"
  [[_ bindings body]]
  `(apply str (for ~bindings ~(compile-html body))))

#_(defmethod compile-form "if"
  [[_ condition & body]]
  `(if ~condition ~@(for [x body] (compile-html x))))

(defmethod compile-form :default
  [expr]
  `(with-meta ~expr
     {:node nil :dynamic true}))

(defn- not-hint?
  "True if x is not hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (not (isa? (eval hint) type))))

(defn- hint?
  "True if x is hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (isa? (eval hint) type)))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (unevaluated? x))
      (not-hint? x java.util.Map)))

(defn format-attribute [[name value]]
  (let [name (to-attr-val name)]
    (if-not value
      `()
      `(~name ~(escape-html (to-attr-val value))))))

(defn render-attr-map [id class {children :children}]
  (let [id-and-class (cond
                       (and id class) `(("id" ~id) ("class" ~class))
                       id `(("id" ~id))
                       class `(("class" ~class)))]
    (->> (map format-attribute (partition 2 children))
         (into id-and-class)
         (apply concat)
         (cons 'cljs.core/js-array))))

(defn- merge-attributes [{:keys [id class]} map-attrs]
  (->> map-attrs
       (merge (if id {:id id}))
       (merge-with #(if %1 (str %1 " " %2) %2) (if class {:class class}))))

(defn normalize-element
  "Ensure an element vector is of the form [tag-name attrs content]."
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (IllegalArgumentException. (str tag " is not a valid element name."))))
  (let [[_ tag id class] (re-matches re-tag (as-str tag))
        tag-attrs        {:id id
                          :class (if class (.replace ^String class "." " "))}
        map-attrs        (first content)]
    (if (map? map-attrs)
      [tag (merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

(defn normalize-tag [tag]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (IllegalArgumentException.
            (str tag " is not a valid element name."))))
  (let [[_ tag id class] (re-matches re-tag (as-str tag))]
    [tag id (if class (.replace ^String class "." " "))]))

(defprotocol HtmlRenderer
  (render-html [this]))

(defn render-element [element]
  (let [[tag attrs content] (normalize-element element)]
    (let [attrs (render-attr-map attrs)
          node `(goog.dom.createDom ~tag ~attrs)]
      (if-not (nil? content)
        (let [node-sym (gensym "node-sym")]
          `(let [~node-sym ~node]
             (goog.dom.append
              ~node-sym (~'js-array ~@(render-html content)))
             ~node-sym))
        node))))

(extend-protocol HtmlRenderer
  IPersistentVector
  (render-html [this]
    (render-element this))
  ISeq
  (render-html [this]
    (map render-html this))
  Named
  (render-html [this]
    `(goog.dom.createTextNode ~(name this)))
  Object
  (render-html [this]
    `(goog.dom.createTextNode ~(str this)))
  nil
  (render-html [this]
    `(goog.dom.createTextNode "")))

(defn- element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  [[tag attrs & content :as element]]
  (cond
    (every? literal? element)
    ::all-literal                    ; e.g. [:span "foo"]
    (and (literal? tag) (map? attrs))
    ::literal-tag-and-attributes     ; e.g. [:span {} x]
    (and (literal? tag) (not-implicit-map? attrs))
    ::literal-tag-and-no-attributes  ; e.g. [:span ^String x]
    (literal? tag)
    ::literal-tag                    ; e.g. [:span x]
    :else
    ::default))

(declare compile-seq)

(defmulti compile-invoke
  (fn [{[{fn-name :form}] :children}]
    (if fn-name
      [fn-name]
      [:default])))

(defmethod compile-invoke [:default]
  [{:keys [form]}]
  `(render-html ~form))

(defn compile-element-static-strategy
  [{op :op form :form {locals :locals} :env
    [{first-op :op first-tag :tag}
     {second-op :op second-tag :tag }
     :as children] :children}]
  (cond
    (= op :constant)
    [:constant]
    (= op :invoke)
    [:invoke]
    (= op :do)
    [:do]
    (= op :let)
    [:let]
    (and (= op :var))
    [:var]
    (and (= op :vector)
         (or (= 'cljs.core/Keyword first-tag)
             (= 'string first-tag)
             (= 'cljs.core/Symbol first-tag))
         (or (not (second children))
             (= :constant second-op)
             (= :vector second-op)))
    [:node :no-attrs]
    (and (= op :vector)
         (or (= 'cljs.core/Keyword first-tag)
             (= 'string first-tag)
             (= 'cljs.core/Symbol first-tag))
         (= :map second-op))
    [:node :literal-attrs]
    (and (= op :vector)
         (or (= 'cljs.core/Keyword first-tag)
             (= 'string first-tag)
             (= 'cljs.core/Symbol first-tag)))
    [:node :literal-tag]
    :else [:default]))

(defmulti compile-element-static compile-element-static-strategy)

(defmethod compile-element-static [:constant]
  [{:keys [form]}]
  (render-html form))

(defmethod compile-element-static [:invoke]
  [node]
  (compile-invoke node))

(defmethod compile-element-static [:do]
  [{:keys [statements ret]}]
  (doseq [s statements]
     (compile-element-static s))
  (compile-element-static ret))

(defmethod compile-element-static [:let]
  [{bindings :bindings expr :expr}]
  `(let ~(->> (mapcat (fn [binding]
                        [(compile-element-static binding)
                         (compile-element-static (:init binding))])
                      bindings)
              (into []))
     ~(compile-element-static expr)))

(defmethod compile-element-static [:var]
  [{{name :name} :info}] name)

(defmethod compile-element-static [:node :no-attrs]
  [{[tag] :form children :children}]
  (let [[tag id class] (normalize-tag tag)]
    (let [attrs (render-attr-map id class nil)
          node `(goog.dom.createdom ~tag ~attrs)
          node-sym (gensym "node-sym")]
      `(let [~node-sym ~node]
         (goog.dom.append
          ~node-sym
          (~'js-array
           ~@(map compile-element-static (drop 1 children))))
         ~node-sym))))

(defmethod compile-element-static [:node :literal-attrs]
  [{[tag] :form [{} attrs :as children] :children}]
  (let [[tag id class] (normalize-tag tag)]
    (let [attrs (render-attr-map id class attrs)
          node `(goog.dom.createdom ~tag ~attrs)
          node-sym (gensym "node-sym")]
      `(let [~node-sym ~node]
         (goog.dom.append
          ~node-sym
          (~'js-array
           ~@(map compile-element-static (drop 2 children))))
         ~node-sym))))

(defmethod compile-element-static [:node :literal-tag]
  [{:keys [form children]}]
  (let [[tag attrs _] form
        [tag id class] (normalize-tag tag)
        attrs-sym       (gensym "attrs")
        attrs `(let [~attrs-sym ~attrs]
                 (if (map? ~attrs-sym)
                   (render-attr-map ~id ~class ~attrs-sym)
                   (render-attr-map ~id ~class)))
        node `(goog.dom.createDom ~tag ~attrs)
        node-sym (gensym "node-sym")]
    `(let [~node-sym ~node]
       (goog.dom.append
        ~node-sym
        (~'js-array
         ~@(map compile-element-static (drop 2 children))))
       ~node-sym)))

(defmethod compile-element-static [:default]
  [{:keys [form]}]
  `(render-html ~form))

(defmulti emit-dynamic (fn [{:keys [op tag]}] [op tag]))
(defmethod emit-dynamic [:constant 'cljs.core/Symbol]
  [{:keys [form]}]
  `(render-html ~form))
(defmethod emit-dynamic [:constant 'string]
  [{:keys [form]}]
  `(render-html ~form))

(defn compile-html
  "Pre-compile data structures into HTML where possible."
  [content]
  (first (compile-seq [content])))

;;Taken from clojure.tools.macro
(defn name-with-attributes
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
        [attr macro-args]          (if (map? (first macro-args))
                                     [(first macro-args) (next macro-args)]
                                     [{} macro-args])
        attr                       (if docstring
                                     (assoc attr :doc docstring)
                                     attr)
        attr                       (if (meta name)
                                     (conj (meta name) attr)
                                     attr)]
    [(with-meta name attr) macro-args]))

(defmacro defhtml [name & macro-args]
  `(def ~name (Vtree. ~(compile-html (last (rest macro-args)))
                      (quote ~(first macro-args)))))


(defn diff-trees [tree1 tree2]
  (let [result (volatile! [])
        point (volatile! nil)
        n (count tree1)
        m (count tree2)
        v (volatile! (vec (int-array (+ (* 2 (+ n m)) 3))))
        v-seq (volatile! [])
        offset (+ n m 1)]
    (loop [d 0]
      (loop [k (- d)]
        (let [down (or (= k (- d))
                       (and (not= k d)
                            (< (get @v (+ offset (dec k)))
                               (get @v (+ offset (inc k))))))
              k-prev (if down (inc k) (dec k))
              x-start (get @v (+ offset k-prev))
              y-start (- x-start k-prev)
              x-mid (if down x-start (inc x-start))
              y-mid (- x-mid k)
              [x-end y-end] (loop [x-end x-mid
                                   y-end y-mid]
                              (if (and (< x-end n)
                                       (< y-end m)
                                       (= (get tree1 x-end)
                                          (get tree2 y-end)))
                                (recur (inc x-end) (inc y-end))
                                [x-end y-end]))]
          (vswap! v assoc (+ offset k) x-end)
          (cond (and (>= x-end n) (>= y-end m))
                (vreset! point [x-end y-end])
                (> (+ 2 k) d)
                (vreset! point nil)
                :else
                (recur (+ k 2)))))
      (vswap! v-seq conj @v)
      (when (and (< d (+ n m)) (= nil @point))
        (recur (inc d))))
    (loop [d (dec (count @v-seq))
           [px py] @point]
      (let [v (get @v-seq d)
            k (- px py)
            x-end (get v (+ offset k))
            y-end (- x-end k)
            down (or (= k (- d))
                     (and (not= k d)
                          (< (get v (+ offset (dec k)))
                             (get v (+ offset (inc k))))))
            k-prev (if down (inc k) (dec k))
            x-start (get v (+ offset k-prev))
            y-start (- x-start k-prev)
            x-mid (if down x-start (inc x-start))
            y-mid (- x-mid k)
            diag-nb (- x-end x-mid)]
        (vswap! result conj
                [[x-start y-start] [x-mid y-mid] [ x-end y-end]])
        (if (or (> x-start 0)
                (> y-start 0))
          (recur (dec d) [x-start y-start])
          @result)))))


(comment

  (diff-trees [:a :b :c :a :b :b :a] [:c :b :a :b :a :c])
  (diff-trees [:a1 :b1 :c1] [:a2 :b3 :c1])
  (diff-trees [:a1 :b1 :c1] [:a1 :b1 :c1])

  (macroexpand-1 '(defhtml ee [e] [:div e]))

  (macroexpand-1 '(defhtml ee [] [:div]))

  (let [fragment (createDoocumentFragment)])

  (require '[clojure.walk :as walk])

  (walk/prewalk (fn [x] (prn x) x) [[1 [2] 3] [2 4]])

  (macroexpand-1 '(html [:div [:p]]))

  )



(comment

  {:name ""
   :mount-order 0
   :params '[x y]
   :ast {}
   :dynamic-nodes {}}
[(if true [:div] [:p])]
  (render-html [:div])

  (:op (ana-api/analyze (ana-api/empty-env) [:div]))
  (render-html "e")
  (compile-element-static (ana-api/analyze (ana-api/empty-env) [:div [:p]]))
  (second (:children (ana-api/analyze (ana-api/empty-env) [:div [:p]])))

  (if (vector? x)
    )

  [:div (if x [:p] [:span y])]

  (let [x [[[:e] "f"] [:e "r"]]
        y x
        x (assoc-in x [0 1] "r")]
    (prn x)
    (prn y)
    (identical? (get-in x [0 0]) (get-in y [0 0])))

  [:div (if x [:p "e"] (if y [:span "e"] [:img]))]
  )

(comment
  '[:e {} e]

  (let [val '[:div {} [:e {} e]]
        e "s"]
    (if (string? e)
      (assoc-in val [2 2] e)))
  )
