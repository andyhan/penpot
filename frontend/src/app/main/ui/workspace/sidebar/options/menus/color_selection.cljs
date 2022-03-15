;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.menus.color-selection
  (:require
   [app.common.attrs :as attrs]
   [app.common.colors :as clr]
   [app.common.data :as d]
   [app.common.pages :as cp]
   [app.common.pages.common :as cpc]
   [app.common.text :as txt]
   [app.main.data.workspace.colors :as dc]
   [app.main.store :as st]
   [app.main.ui.hooks :as h]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.options.rows.color-row :refer [color-row]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [rumext.alpha :as mf]))

(defn fill->color
  [fill]
  {:color (:fill-color fill)
   :opacity (:fill-opacity fill)
   :id (:fill-color-ref-id fill)
   :file-id (:fill-color-ref-file fill)
   :gradient (:fill-color-gradient fill)})

(defn stroke->color
  [stroke]
  {:color (:stroke-color stroke)
   :opacity (:stroke-opacity stroke)
   :id (:stroke-color-ref-id stroke)
   :file-id (:stroke-color-ref-file stroke)
   :gradient (:stroke-color-gradient stroke)})

(defn get-colors-shape
  [colors shape]
  (if (= :text (:type shape))
    (as-> colors $
      (into $ (map stroke->color) (:strokes shape))
      (reduce get-colors-shape $ (txt/node-seq txt/is-text-node? (:content shape))))

    (-> colors
        (into (map fill->color) (:fills shape))
        (into (map stroke->color) (:strokes shape)))))

(defn get-colors-shapes
  [shapes]
  (reduce get-colors-shape #{} shapes))

(mf/defc color-selection-menu
  {::mf/wrap [#(mf/memo' % (mf/check-props ["ids" "values"]))]}
  [{:keys [type values] :as props}]
  (let [colors (get-colors-shapes values)
        divided-colors (group-by #(some? (:id %)) colors)
        library-colors (get divided-colors true)
        not-library-colors (get divided-colors false)
        expand-lib-color (mf/use-state false)
        expand-color (mf/use-state false)
        on-detach "eyyyyyyyy"]
    (when (< 1 (count colors))
      [:div.element-set
       [:div.element-set-title
        [:span (tr "workspace.options.selection-color")]]
       [:div.element-set-content
        [:div
         [:*
          (for [color (take 3 library-colors)]
            [:& color-row {:color color
                           :index color
                           :on-detach on-detach}])
          (when (and (false? @expand-lib-color) (< 3 (count library-colors)))
            [:div.expand-colors  {:on-click #(reset! expand-lib-color true)}
             i/actions (tr "workspace.options.more-lib-colors")])
          (when @expand-lib-color
            (for [color (drop 3 library-colors)]
              [:& color-row {:color color
                             :index color
                             :on-detach on-detach}]))]]

        [:div
         [:*
          (for [color (take 3 not-library-colors)]
            [:& color-row {:color color
                           :index color}])
          (when (and (false? @expand-color) (< 3 (count not-library-colors)))
            [:div.expand-colors  {:on-click #(reset! expand-color true)}
             [:span ]
             i/actions (tr "workspace.options.more-colors")])
          (when @expand-color
            (for [color (drop 3 not-library-colors)]
              [:& color-row {:color color
                             :index color}]))]]]])))
