(ns ewen.viccup.emitter
  (:require [cljs.analyzer.api :as ana-api]))

(defmulti emit* :op)

(defmethod emit* :vector
  [{:keys [items]}]
  (apply vector (map emit* items)))

(defmethod emit* :constant
  [{:keys [form env]}] form)

(defmethod emit* :map
  [{:keys [keys vals]}]
  (zipmap (map emit* keys)
          (map emit* vals)))

(defmethod emit* :list
  [{:keys [items]}] (map emit* items))

(defmethod emit* :set
  [{:keys [items]}] (into #{} (map emit* items)))

(defmethod emit* :var
  [{:keys [form name] :as ast}] (or form name))

(defmethod emit* :var-special
  [{{{name :name} :info} :var}] `(var ~name))

(defmethod emit* :def
  [{:keys [var init]}] `(def ~(emit* var) ~(emit* init)))

(defmethod emit* :do
  [{:keys [statements ret]}]
  `(do ~@(into (list (emit* ret)) (map emit* statements))))

(defn emit-do-content [{:keys [statements ret]}]
  (into (list (emit* ret)) (map emit* statements)))

(defmethod emit* :invoke
  [{:keys [f args] :as ast}]
  (conj (map emit* args) (emit* f)))

(defmethod emit* :let
  [{:keys [bindings expr] :as ast}]
  `(~'let ~(->> (for [{:keys [init] :as binding} bindings]
                  [(emit* binding) (emit* init)])
                (apply concat)
                vec)
     ~@(emit-do-content expr)))

(defn emit-fn-params [params variadic]
  (if variadic
    (-> (mapv emit* (butlast params))
        (conj '& (emit* (last params))))
    (mapv emit* params)))

(defmethod emit* :fn
  [{:keys [variadic name methods form] :as ast}]
  (if (= 1 (count methods))
    (let [{:keys [params expr variadic]} (first methods)]
      `(~'fn ~@(if name (list (:name name)) nil)
         ~(emit-fn-params params variadic)
         ~@(emit-do-content expr)))
    (let [params (map #(emit-fn-params (:params %) (:variadic %)) methods)
          bodys (mapcat #(emit-do-content (:expr %)) methods)]
      `(~'fn ~@(if name (list (:name name)) nil)
         ~@(->> (interleave params bodys)
                (partition 2))))))

(defn emit-fn-content
  [{:keys [variadic name methods form] :as ast}]
  (if (= 1 (count methods))
    (let [{:keys [params expr variadic]} (first methods)]
      `(~@(if name (list (:name name)) nil)
         ~(emit-fn-params params variadic)
         ~@(emit-do-content expr)))
    (let [params (map #(emit-fn-params (:params %) (:variadic %)) methods)
          bodys (mapcat #(emit-do-content (:expr %)) methods)]
      `(~@(if name (list (:name name)) nil)
         ~@(->> (interleave params bodys)
                (partition 2))))))

(defmethod emit* :js
  [{:keys [form] :as ast}] form)

(defmethod emit* :if
  [{:keys [test then else] :as ast}]
  `(if ~(emit* test) ~(emit* then) ~(emit* else)))

(defmethod emit* :new
  [{:keys [ctor args] :as ast}]
  `(~(symbol (str (emit* ctor) "."))
    ~@(map emit* args)))

(defmethod emit* :loop
  [{:keys [bindings expr] :as ast}]
  `(~'loop ~(->> (for [{:keys [init] :as binding} bindings]
                  [(emit* binding) (emit* init)])
                (apply concat)
                vec)
     ~@(emit-do-content expr)))

(defmethod emit* :recur
  [{:keys [exprs] :as ast}]
  `(recur ~@(map emit* exprs)))

(defmethod emit* :letfn
  [{:keys [bindings expr] :as ast}]
  `(~'letfn ~(->> (for [{:keys [init]} bindings]
                   [(emit-fn-content init)])
                 (apply concat)
                 vec)
     ~@(emit-do-content expr)))

(defmethod emit* :no-op [ast]
  nil)

(defmethod emit* :dot
  [{:keys [target method args] :as ast}]
  `(~(symbol (str "." method)) ~(emit* target) ~@(map emit* args)))

(defmethod emit* :throw
  [{:keys [throw] :as ast}]
  `(throw ~(emit* throw)))

(defmethod emit* :set!
  [{:keys [target val] :as ast}]
  `(~'set! ~(emit* target) ~(emit* val)))

(defmethod emit* :deftype*
  [{:keys [form t fields pmasks protocols body] :as ast}]
  form)

(defmethod emit* :defrecord*
  [{:keys [form] :as ast}] form)

(defmethod emit* :case*
  [{:keys [v tests thens default] :as ast}]
  `(~'case ~(emit* v)
     #_~@(interleave (map emit* tests)
                   (map emit* thens))))


(comment
  (require '[cljs.compiler :as cljs-comp])
  (keys (methods cljs-comp/emit*))

  (emit*
   (ana-api/analyze
    (ana-api/empty-env)
    '(case e "" e)))
  )

(comment

  (defn ee1 [e f t] [:div {:class "r"} [:div {:e "e" :f "f"} [:p [:p t [:p [:p [:p f]]]] [:p [:p]] [:p [:p]] [:p [:p]]]] [:div] [:div] [:p [:span] [:span e]]])

  (let [s ["div" {:class "r"} ["div" {:e "e" :f "f"} ["p" ["p" 't ["p" ["p" ["p" 'f]]]] ["p" ["p"]] ["p" ["p"]] ["p" ["p"]]]] ["div"] ["div"] ["p" ["span"] ["span" 'e]]]
        c1 [5 2 1]
        c2 [2 2 1 2 1 1 1]
        c3 [2 2 1 1]]
    (defn ee2 [e f g]
      (-> (assoc-in s c1 e)
          #_(assoc-in c2 f)
          (assoc-in c3 g))))

  (defn ee3 [e f t] #js ["div" #js {:class "r"} #js ["div" #js {:e "e", :f "f"} #js ["p" #js ["p" t #js ["p" #js ["p" #js ["p" f]]]] #js ["p" #js ["p"]] #js ["p" #js ["p"]] #js ["p" #js ["p"]]]] #js ["div"] #js ["div"] #js ["p" #js ["span"] #js ["span" e]]])

  (let [s #js ["div" #js {:class "r"} #js ["div" #js {:e "e", :f "f"} #js ["p" #js ["p" "t" #js ["p" #js ["p" #js ["p" "f"]]]] #js ["p" #js ["p"]] #js ["p" #js ["p"]] #js ["p" #js ["p"]]]] #js ["div"] #js ["div"] #js ["p" #js ["span"] #js ["span" "e"]]]
        c1 [5 2 1]
        c2 [2 2 1 2 1 1 1]
        c3 [2 2 1 1]]
    (defn ee4 [e f g]
      (-> (assoc-in* s c1 e)
          #_(assoc-in c2 f)
          (assoc-in* c3 g))))

  (time ((dotimes [i 1000000]
           (ee1 [:img i] 3 4))))

  (defn assoc* [a i v]
    (let [l (.-length a)
          #_a-c #_(.slice a 0)]
      (aset a i v)
      a))

  (defn assoc-in*
    [m [k & ks] v]
    (if ks
      (assoc* m k (assoc-in* (aget m k) ks v))
      (assoc* m k v)))

  (time (dotimes [i 1000000]
          (ee2 3 3 4)))

  (time (dotimes [i 1000000]
          (count (ee3 4 3 4))))

  (time (dotimes [i 1000000]
          (count (ee4 3 3 4))))

  (set! js/window.ee2 (fn [] (dotimes [i 1000000]
                               (ee2 [:img i] 3 4))))

  (set! js/window.ee3 (fn [] (dotimes [i 1000000]
                               (ee3 [:img i] 3 4))))

  (set! js/window.ee4 (fn [] (dotimes [i 1000000]
                               (ee4 [:img i] 3 4))))

  (let [vv (repeatedly 300 (constantly 1))]
    (defn hh1 [] #js [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60]))

  (let [vv (apply array (repeatedly 300 #(.random js/Math)))]
    (defn hh1 [] (.slice vv 0)))

  (defn hh1 [] [1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1 2])

  (time (dotimes [i 1000000]
          (hh1)))

  (let [a [1 2 3]]
    (time (dotimes [i 1000000]
            (assoc-in a [1] 3))))

  )
