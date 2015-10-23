(ns ewen.viccup.test.core
  (:require [ewen.viccup.core :refer :all]
            [cljs.analyzer.api :as ana-api]))


(comment
  (ns-unmap (find-ns 'ewen.viccup.core) 'compile-element-static)

  (compile-element-static
   (ana-api/analyze
    (ana-api/empty-env)
    '[:div#t.r {:e "e"} [:p "e"] [:span]]))

  (compile-element-static
   (ana-api/analyze
    (ana-api/empty-env)
    '[:div#t.r {:e e} [:p "e"] [:span]]))

  (ast-show-only (ana-api/analyze
                  (ana-api/empty-env)
                  '[:div#t.r {:e "e"} [:p "e"] [:span]])
                 [:op :tag :form])

  (ast-show-only (ana-api/analyze
                  (ana-api/empty-env)
                  '(let [e 3] [:div {:e e}]))
                 [:op :tag :form])
  )
