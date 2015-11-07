(ns ewen.viccup.test.core
  (:require [ewen.viccup.core :refer :all]
            [cljs.analyzer.api :as ana-api]))


(comment
  (ns-unmap (find-ns 'ewen.viccup.util) 'to-attr-val)

  (render-attr-map "id" "class"
                   (ana-api/analyze
                    (ana-api/empty-env)
                    '{:e "e" :f 'e}))

  (compile-element-static-strategy '[:div])

  (ast-show-only (ana-api/analyze
                  (ana-api/empty-env)
                  '[:div])
                 [:op :tag :form])

  (compile-element-static
   (ana-api/analyze
    (ana-api/empty-env)
    '[:div]))

  (compile-element-static
   (ana-api/analyze
    (ana-api/empty-env)
    '[:div#t.r {:e "e"} [:p "e"] [:span]]))

  (compile-element-static
   (ana-api/analyze
    (ana-api/empty-env)
    '[:div#t.r {f e}]))

  (compile-element-static
   (ana-api/analyze
    (ana-api/empty-env)
    '[:div {} e]))

  (compile-element-static
   (ana-api/analyze
    (ana-api/empty-env)
    '(let [x "e"] [:p] x)))

  (compile-element-static
   (ana-api/analyze
    (ana-api/empty-env)
    '[:div e]))

  (ast-show-only (ana-api/analyze
                  (ana-api/empty-env)
                  '[:div#e "e"])
                 [:op :tag :form])

  (ast-show-only (ana-api/analyze
                  (ana-api/empty-env)
                  '{:e "e"})
                 [:op :tag :form])

  )



(comment
  [:div {(if true :e :f) "e"}]

  [:div (let [ee {:e "e"}]
          (if true
            (assoc ee :f "f")
            ee))]

  (ast-show-only (ana-api/analyze
                  (ana-api/empty-env)
                  '{(if e e :f) "e"})
                 [:form])

  (let [x [[:aa [:ff]] [[:p] :div]]
        y x
        x (assoc-in x [0 0] :span)]
    (identical?  (first x) (first y)))

  (ast-show-only (ana-api/analyze
                  (ana-api/empty-env)
                  '(let [e "e"] e))
                 [:op :tag :form])

  (:locals (get-in (ana-api/analyze
                  (ana-api/empty-env)
                  '(let [e "e"] e))
                   [:children 1 :children 0 :env]))

  (ast-show-only (ana-api/analyze
                  (ana-api/empty-env)
                  '(fn [e] e))
                 [:op :tag :form])

  (:locals (get-in (ana-api/analyze
                    (ana-api/empty-env)
                    '(fn [e] e))
                   [:children 0 :children 0 :env]))
  )
