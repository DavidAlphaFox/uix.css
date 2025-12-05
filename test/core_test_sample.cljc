(ns core-test-sample
  (:require [uix.core :refer [defui $]]
            [uix.dom.server :as dom.server]
            [uix.css :refer [css]]
            [uix.css.adapter.uix]
            #?(:cljs [fs])))

(def border-color "#000")
(def hover-bg "yellow")
(def v (atom 90))

(def styles
  (let [p-xl 32]
    (css {:margin 64
          :padding (inc p-xl)
          :border (str "1px solid " border-color)
          :&:hover {:color :blue
                    :background hover-bg
                    :width @v}
          "&:hover > div" {:border-radius p-xl}
          "@media (max-width: 800px)" {:color hover-bg
                                       :width (+ 8 9)
                                       :&:hover {:color hover-bg
                                                 :width (+ @v 89)}}})))

(defui component []
  ($ :div {:style styles}))

#?(:cljs
    (defn -main [& args]
      (let [html (dom.server/render-to-string ($ component))
            path "server_render_test/html/test.html"]
        (fs/writeFileSync path html))
      (let [html (dom.server/render-to-static-markup ($ component))
            path "server_render_test/markup/test.html"]
        (fs/writeFileSync path html))))