;;  cj_renren.clj
;;  RSRenrenCore
;;
;;  Created by closure on 1/10/14.
;;  Copyright (c) 2014 closure. All rights reserved.
;;

;dependencies [org.clojure/clojure "1.5.1"]
;             [http-kit "2.1.16"]
;             [clj-http "0.7.8"]
;             [enlive "1.1.5"]]

(ns cj-renren.core
  (:require [org.httpkit.client :as http]
            [clojure.string :as str]
            [clj-http.client :as client]
            [net.cgrand.enlive-html :as html])
  (:use [clojure.walk :only [prewalk macroexpand-all]])
  (:import [java.net URI URLEncoder]))

(defmacro url-encode [url] `(URLEncoder/encode ~url))

(defmacro login-body [email pwd]
  `(str "origURL=http://www.renren.com/SysHome.do&domain=renren.com&password=" ~pwd "&email=" ~email))

(def renren-cookie (clj-http.cookies/cookie-store))

(defn login [email password f]
  (if (or (empty? email) (empty? password))
    (if (not (fn? f))
      "f is not fn"
      (f nil))
    (let [post-data (login-body email password)]
      (if-let [result (client/post "http://www.renren.com/PLogin.do/"
                                  {:body (.getBytes post-data)
                                   :length (count post-data)
                                   :headers {"Content-Type" "application/x-www-form-urlencoded"}
                                   :cookie-store renren-cookie
                                   :accept "*/*"
                                   :force-redirects true
                                   :decode-body-headers true
                                   })]
        (f result)
        (do (println "failed") (f nil))))))

(defn get-token[content]
  (let [requestToken (re-find #"get_check:'-?\d+'" content)
        _rtk (re-find #"get_check_x:'\w+'" content)]
    {:requestToken (subs requestToken (count "get_check:'") (dec (count requestToken)))
     :_rtk (subs _rtk (count "get_check_x:'") (dec (count _rtk)))}))

(defmacro user-feed-coll-url [target-id token feed-cnt]
  `(str "http://www.renren.com/moreminifeed.do?p=" ~feed-cnt
       "&u=" ~target-id
       "&requestToken=" (:requestToken ~token)
       "&_rtk=" (:_rtk ~token)))

(def regular-expression-1 (str "\\(?onclick=\\\"ILike_toggleUserLike\\(.*?\\)\\\""))
(def regular-expression-2 (str "\\(.*\\)"))

(defmacro match-all [match-str content]
  `(re-seq (re-pattern ~match-str) ~content))

(defn feed-match-result-parser [item]
  (let [content (str item)]
    (if (or (empty? content)
            (not= (nth content 0) \()
            (not= (nth content (dec (count item))) \)))
      nil
      (let [se (re-seq (re-pattern "\\'.*?\\'") (str item))]
        (zipmap (map keyword '("type" "gid" "uid" "owner" "name"))
                (map #(list (subs % 1 (dec (count %)))) se))))))

(defn do-event-url [parser-result event-type]
  (format "http://like.renren.com/%s?gid=%s_%s&uid=%s&owner=%s&type=%d&name=%s"
          event-type (first (:type parser-result))
          (first (:gid parser-result))
          (first (:uid parser-result))
          (first (:owner parser-result))
          3
          (first (:name parser-result))))

(defn do-event-action [event]
  (let [{:keys[status headers body]} (client/get event {:cookie-store renren-cookie})]
    (if (= 200 status)
      (client/json-decode body)
      "")))

(defn do-like-event-url [event]
  (do-event-url event "addlike"))

(defn do-unlike-event-url [event]
  (do-event-url event "removelike"))

(defmacro for-each [[sym coll] & body]
  `(loop [coll# ~coll]
     (when-let [[~sym & xs#] (seq coll#)]
       ~@body
       (recur (next coll#)))))

(defn event-collection [target-id coll-cnt token]
  "收集新鲜事 用来等会儿点赞"
  (if (neg? coll-cnt)
    ""
    (loop [feed-contents '()
           cnt 0
           idx 0]
      (if (> cnt coll-cnt)
        (map #(feed-match-result-parser %) (first (partition coll-cnt (flatten feed-contents))))
        (recur (cons (map #(match-all regular-expression-2 %) (match-all regular-expression-1 (:body (client/get (user-feed-coll-url
                                                                              target-id
                                                                              token
                                                                              1)
                                                                             {:cookie-store renren-cookie}))))
                     feed-contents)
               (count feed-contents)
               (inc idx))))))

(defn do-events-action
  "GO GO GO 点赞去
  do-type : addlike, removelike
  events  : 来自 event-collection
  delay-time : 为了不让服务器报操作过快加延时 单位ms"
  ([do-type events]
   (let [event-builder (case do-type
                          "addlike" do-like-event-url
                          "removelike" do-unlike-event-url)]
     (map (fn [event]
            (do
              (println "do event " event)
              (-> event event-builder do-event-action))) events)))
  ([do-type events delay-time]
   (let [event-builder (case do-type
                          "addlike" do-like-event-url
                          "removelike" do-unlike-event-url)]
     (loop [idx 0
            cnt (count events)
            r '()]
       (println "do event " (nth events idx))
       (Thread/sleep delay-time)
       (if (>= idx cnt)
         (flatten r)
         (recur (inc idx)
                cnt
                (cons (-> (nth events idx) event-builder r))))))))

(def analyzer (login "account" "password"
 (fn [{:keys [raw-headers links status headers body cookies trace-redirects]}]
   (if (and (= 200 status) (> (count trace-redirects) 0))
     {:user-id (apply str (rest (:uri (client/parse-url (last trace-redirects)))))
      :status status
      :body body}
     {:usre-id 10
      :status status
      :body body}))))

(def token (get-token (:body analyzer)))

(do-events-action "removelike" ; "addlike"
                  (event-collection "target-id" 100 token)
                  1000)

(for-each [event (event-collection "target-id" 10 token)]
  (do (println "do event")
    (Thread/sleep 1000)
    (-> event do-unlike-event-url do-event-action println)))
